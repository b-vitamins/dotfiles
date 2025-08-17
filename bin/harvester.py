#!/usr/bin/env python3
"""
ArXiv paper aggregator with RSS and OAI-PMH support.
"""

from datetime import datetime, timedelta
import time
import re
from typing import Dict, List, Tuple, Optional
import json
import argparse
from pathlib import Path
from collections import defaultdict
import xml.etree.ElementTree as ET
from urllib.parse import urlencode
from urllib.request import urlopen, Request
from urllib.error import URLError, HTTPError
import os
import sys

__version__ = "1.0.0"


def load_config():
    """Load configuration from XDG config directory."""
    config_home = os.environ.get("XDG_CONFIG_HOME", os.path.expanduser("~/.config"))
    config_path = Path(config_home) / "harvester" / "config.json"

    defaults = {
        "categories": [
            "cs.LG",
            "stat.ML",
            "cs.AI",
            "cs.CV",
            "cs.CL",
            "cond-mat.stat-mech",
            "cond-mat.dis-nn",
            "nlin.AO",
            "math.OC",
            "physics.data-an",
        ],
        "output_dir": None,  # Will use XDG_DATA_HOME/harvester if None
        "format": "markdown",
        "method": "rss",
        "delay": 1.0,
        "max_retries": 3,
        "timeout": 30,
    }

    if config_path.exists():
        try:
            with open(config_path, "r") as f:
                config = json.load(f)
                for key, value in config.items():
                    if value is not None:
                        defaults[key] = value
            print(f"Loaded configuration from {config_path}")
        except Exception as e:
            print(f"Warning: Failed to load config from {config_path}: {e}")

    return defaults


def get_xdg_docs_dir():
    """Get XDG documents directory from user-dirs.dirs."""
    config_home = os.environ.get("XDG_CONFIG_HOME", os.path.expanduser("~/.config"))
    user_dirs_file = Path(config_home) / "user-dirs.dirs"

    if user_dirs_file.exists():
        try:
            with open(user_dirs_file, "r") as f:
                for line in f:
                    if line.startswith("XDG_DOCUMENTS_DIR="):
                        path = line.split("=", 1)[1].strip().strip('"')
                        path = path.replace("$HOME", os.path.expanduser("~"))
                        return path
        except (IOError, OSError):
            pass
    return os.path.expanduser("~/Documents")


def save_default_config():
    """Save default configuration file as example."""
    config_home = os.environ.get("XDG_CONFIG_HOME", os.path.expanduser("~/.config"))
    config_dir = Path(config_home) / "harvester"
    config_path = config_dir / "config.json.example"

    try:
        config_dir.mkdir(parents=True, exist_ok=True)
    except PermissionError:
        print(f"Error: Permission denied creating config directory '{config_dir}'")
        print("Please check that you have write access to the parent directory")
        sys.exit(1)
    except OSError as e:
        print(f"Error: Failed to create config directory '{config_dir}': {e}")
        sys.exit(1)

    docs_dir = get_xdg_docs_dir()

    example_config = {
        "categories": ["cs.LG", "stat.ML", "cs.AI", "cs.CV", "cs.CL"],
        "output_dir": f"{docs_dir}/arxiv-papers",
        "format": "markdown",
        "method": "rss",
        "delay": 1.0,
        "max_retries": 3,
        "timeout": 30,
        "_comment": "Copy this to config.json and modify as needed",
    }

    with open(config_path, "w") as f:
        json.dump(example_config, f, indent=2)

    return config_path


class ArXivAggregator:
    def __init__(self, categories: List[str], use_oai: bool = False):
        """Initialize aggregator with arXiv categories.

        Args:
            categories: List of arXiv category codes
            use_oai: Use OAI-PMH protocol instead of RSS
        """
        self.categories = categories
        self.papers = {}
        self.category_counts = defaultdict(int)
        self.use_oai = use_oai
        self.oai_base_url = "http://export.arxiv.org/oai2"

    def fetch_category_rss(self, category: str, max_retries: int = 3) -> List[Dict]:
        """Fetch papers from category RSS feed with retry logic."""
        url = f"http://export.arxiv.org/rss/{category}"

        for attempt in range(max_retries):
            try:
                req = Request(url, headers={"User-Agent": "ArXiv-Harvester/1.0"})
                with urlopen(req, timeout=30) as response:
                    rss_data = response.read()

                root = ET.fromstring(rss_data)
                papers = []

                items = root.findall(".//item")
                if not items:
                    skip_days = root.findall(".//skipDays/day")
                    if skip_days:
                        skipdays = [
                            day.text for day in skip_days if day.text is not None
                        ]
                        print(f"    RSS feed skips: {', '.join(skipdays)}")

                    # Check publication date
                    pub_date = root.find(".//pubDate")
                    if pub_date is not None:
                        print(f"    Last publication: {pub_date.text}")

                    print(
                        "    No items found in RSS feed (likely due to publishing schedule)"
                    )

                for item in items:
                    title = item.find("title")
                    link = item.find("link")
                    description = item.find("description")
                    dc_creator = item.find("{http://purl.org/dc/elements/1.1/}creator")
                    pub_date = item.find("pubDate")

                    if title is None or link is None:
                        continue

                    link_text = link.text if link.text is not None else ""
                    arxiv_id = link_text.split("/abs/")[-1] if link_text else ""

                    authors = (
                        dc_creator.text
                        if dc_creator is not None and dc_creator.text is not None
                        else ""
                    )
                    authors = self._parse_authors(authors)

                    title_text = (
                        title.text.replace("\n", " ").strip()
                        if title.text is not None
                        else ""
                    )
                    categories = []
                    catmatch = re.search(r"\(([^)]+)\)\s*$", title_text)
                    if catmatch:
                        cats = catmatch.group(1).split()
                        categories = [c.strip() for c in cats]
                        title_text = title_text[: catmatch.start()].strip()

                    abstract = (
                        description.text.replace("\n", " ").strip()
                        if description is not None and description.text is not None
                        else ""
                    )
                    abstract = re.sub("<[^<]+?>", "", abstract)

                    paper = {
                        "arxiv_id": arxiv_id,
                        "title": title_text,
                        "authors": authors,
                        "abstract": abstract,
                        "primary_category": category,
                        "all_categories": categories if categories else [category],
                        "link": link_text,
                        "pdf_link": link_text.replace("/abs/", "/pdf/") + ".pdf"
                        if link_text
                        else "",
                        "published": pub_date.text if pub_date is not None else "",
                        "updated": pub_date.text if pub_date is not None else "",
                    }
                    papers.append(paper)

                return papers

            except Exception as e:
                if attempt < max_retries - 1:
                    print(f"  Attempt {attempt + 1} failed: {e}. Retrying...")
                    time.sleep(2**attempt)  # Exponential backoff
                else:
                    print(
                        f"Error fetching {category} after {max_retries} attempts: {e}"
                    )
                    return []

        return []

    def fetch_oai_pmh(
        self,
        from_date: Optional[str] = None,
        until_date: Optional[str] = None,
        category: Optional[str] = None,
        resumption_token: Optional[str] = None,
    ) -> Tuple[List[Dict], Optional[str]]:
        """Fetch papers using OAI-PMH protocol.

        Args:
            from_date: Start date (YYYY-MM-DD)
            until_date: End date (YYYY-MM-DD)
            category: arXiv category/set to harvest
            resumption_token: Token for continuing previous request

        Returns:
            Papers list and resumption token for next page
        """
        papers = []

        if resumption_token:
            params = {"verb": "ListRecords", "resumptionToken": resumption_token}
        else:
            params = {"verb": "ListRecords", "metadataPrefix": "arXiv"}

            if category:
                params["set"] = category

            if from_date:
                params["from"] = from_date

            if until_date:
                params["until"] = until_date

        url = f"{self.oai_base_url}?{urlencode(params)}"
        print(f"OAI-PMH request: {url}")

        max_retries = 3
        response = None

        for attempt in range(max_retries):
            try:
                req = Request(url, headers={"User-Agent": "ArXiv-Harvester/1.0"})
                with urlopen(req, timeout=30) as response:
                    response = response.read()
                break
            except (URLError, HTTPError, TimeoutError) as e:
                if attempt < max_retries - 1:
                    print(f"  Request failed (attempt {attempt + 1}): {e}. Retrying...")
                    time.sleep(2**attempt)
                else:
                    print(f"OAI-PMH request failed after {max_retries} attempts: {e}")
                    return papers, None

        if response is None:
            print("No response data received")
            return papers, None

        try:
            root = ET.fromstring(response)

            ns = {
                "oai": "http://www.openarchives.org/OAI/2.0/",
                "arxiv": "http://arxiv.org/OAI/arXiv/",
            }

            errors = root.findall(".//oai:error", ns)
            if errors:
                for error in errors:
                    print(f"OAI-PMH Error: {error.get('code')} - {error.text}")
                return papers, None

            records = root.findall(".//oai:record", ns)

            for record in records:
                header = record.find("oai:header", ns)
                if header is not None and header.get("status") == "deleted":
                    continue

                metadata = record.find(".//arxiv:arXiv", ns)
                if metadata is None:
                    continue

                paper = self._parse_oai_record(metadata, ns)
                if paper:
                    papers.append(paper)

            resumption = root.find(".//oai:resumptionToken", ns)
            next_token = resumption.text if resumption is not None else None

            return papers, next_token

        except Exception as e:
            print(f"Error in OAI-PMH request: {e}")
            return papers, None

    def _parse_oai_record(self, metadata: ET.Element, ns: dict) -> Optional[Dict]:
        """Parse single OAI-PMH record."""
        try:
            arxiv_id_elem = metadata.find("arxiv:id", ns)
            title_elem = metadata.find("arxiv:title", ns)
            abstract_elem = metadata.find("arxiv:abstract", ns)

            if arxiv_id_elem is None or arxiv_id_elem.text is None:
                return None
            if title_elem is None or title_elem.text is None:
                return None
            if abstract_elem is None or abstract_elem.text is None:
                return None

            arxiv_id = arxiv_id_elem.text
            title = title_elem.text.replace("\n", " ").strip()
            abstract = abstract_elem.text.replace("\n", " ").strip()

            authors = []
            for author in metadata.findall("arxiv:authors/arxiv:author", ns):
                keyname = author.find("arxiv:keyname", ns)
                forenames = author.find("arxiv:forenames", ns)
                if keyname is not None:
                    name = keyname.text
                    if forenames is not None and forenames.text:
                        name = f"{forenames.text} {name}"
                    authors.append(name)

            categories = []
            primary_category = None

            primary = metadata.find("arxiv:categories", ns)
            if primary is not None and primary.text:
                cats = primary.text.split()
                categories = cats
                primary_category = cats[0] if cats else None

            created_elem = metadata.find("arxiv:created", ns)
            if created_elem is None or created_elem.text is None:
                return None
            created = created_elem.text
            updated = metadata.find("arxiv:updated", ns)
            updated_date = updated.text if updated is not None else created

            link = f"http://arxiv.org/abs/{arxiv_id}"
            pdf_link = f"http://arxiv.org/pdf/{arxiv_id}.pdf"

            return {
                "arxiv_id": arxiv_id,
                "title": title,
                "authors": authors,
                "abstract": abstract,
                "primary_category": primary_category,
                "all_categories": categories,
                "link": link,
                "pdf_link": pdf_link,
                "published": created,
                "updated": updated_date,
            }

        except Exception as e:
            print(f"Error parsing OAI record: {e}")
            return None

    def harvest_date_range(
        self, from_date: str, until_date: str, delay: float = 2.0
    ) -> None:
        """Harvest papers from all categories for date range using OAI-PMH.

        Args:
            from_date: Start date in YYYY-MM-DD
            until_date: End date in YYYY-MM-DD
            delay: Seconds to wait between requests
        """
        all_papers = {}

        print(f"\nHarvesting all papers from {from_date} to {until_date}...")

        resumption_token = None
        page = 1

        while True:
            papers, next_token = self.fetch_oai_pmh(
                from_date=from_date,
                until_date=until_date,
                category=None,  # Don't use set parameter, harvest all and filter
                resumption_token=resumption_token,
            )

            retrieved = len(papers)
            total_so_far = len(all_papers) + retrieved
            print(
                f"  Page {page}: Retrieved {retrieved} papers (total: {total_so_far})",
                end="\r",
            )

            for paper in papers:
                arxiv_id = paper["arxiv_id"]

                matches = [
                    cat for cat in paper["all_categories"] if cat in self.categories
                ]

                if matches:
                    for cat in matches:
                        self.category_counts[cat] += 1

                    if arxiv_id not in all_papers:
                        all_papers[arxiv_id] = paper
                    else:
                        existing = all_papers[arxiv_id]
                        all_cats = set(
                            existing["all_categories"] + paper["all_categories"]
                        )
                        existing["all_categories"] = list(all_cats)

            if not next_token:
                break

            resumption_token = next_token
            page += 1
            time.sleep(delay)

        self.papers = all_papers
        print()
        print(
            f"Harvested {len(self.papers)} papers matching specified categories from {from_date} to {until_date}"
        )

    def _parse_authors(self, authors: str) -> List[str]:
        """Parse author string into list of names."""
        cleaned = re.sub("<[^<]+?>", "", authors)

        if "," in cleaned:
            result = [a.strip() for a in cleaned.split(",")]
        else:
            result = [cleaned.strip()]

        return result

    def _sanitize_filename(self, name: str) -> str:
        """Sanitize filename to prevent path traversal.

        Args:
            name: User-provided filename

        Returns:
            Sanitized filename
        """
        if not name:
            raise ValueError("Filename cannot be empty")

        path = Path(name)

        if path.is_absolute():
            raise ValueError(f"Absolute paths not allowed: {name}")

        safename = path.name

        if os.sep in safename or (os.altsep and os.altsep in safename):
            raise ValueError(f"Path separators not allowed in filename: {name}")

        if not safename or safename in (".", ".."):
            raise ValueError(f"Invalid filename: {name}")

        safename = re.sub(r"[^\w\s.-]", "", safename)

        if not safename.strip():
            raise ValueError(f"Filename contains no valid characters: {name}")

        return safename.strip()

    def aggregate(self, delay: float = 1.0) -> None:
        """Fetch papers from all categories using RSS and aggregate, removing duplicates.

        Args:
            delay: Seconds to wait between requests
        """
        all_papers = {}

        print(f"Fetching papers from {len(self.categories)} categories...")
        for i, category in enumerate(self.categories):
            if i > 0:
                time.sleep(delay)

            print(
                f"  [{i + 1}/{len(self.categories)}] {category}... ", end="", flush=True
            )
            papers = self.fetch_category_rss(category)
            print(f"{len(papers)} papers")

            for paper in papers:
                arxiv_id = paper["arxiv_id"]

                self.category_counts[category] += 1

                if arxiv_id not in all_papers:
                    all_papers[arxiv_id] = paper
                else:
                    existing = all_papers[arxiv_id]
                    all_cats = set(existing["all_categories"] + paper["all_categories"])
                    existing["all_categories"] = list(all_cats)

        self.papers = all_papers
        print(f"\nAggregated {len(self.papers)} unique papers")

        if len(self.papers) == 0:
            from datetime import datetime

            current_day = datetime.now().strftime("%A")
            if current_day in ["Saturday", "Sunday"]:
                print("\nNote: arXiv RSS feeds are typically empty on weekends.")
                print("For recent papers, try using OAI-PMH method:")
                print("  python3 harvester.py --method oai --days-back 7")
            else:
                print("\nNote: No papers found. This could be due to:")
                print("  - No new papers in these categories today")
                print("  - arXiv publishing schedule")
                print("  - Network issues")
                print("For recent papers, try using OAI-PMH method:")
                print("  python3 harvester.py --method oai --days-back 3")

    def filter_papers(
        self,
        keywords: Optional[List[str]] = None,
        exclude: Optional[List[str]] = None,
        min_categories: int = 1,
    ) -> Dict:
        """Filter papers based on criteria.

        Args:
            keywords: Include papers with these keywords in title/abstract
            exclude: Exclude papers with these keywords
            min_categories: Minimum number of categories paper must appear in

        Returns:
            Filtered dictionary of papers
        """
        filtered = {}

        if keywords:
            keywords = [kw.strip() for kw in keywords if kw and kw.strip()]
        if exclude:
            exclude = [kw.strip() for kw in exclude if kw and kw.strip()]

        for arxiv_id, paper in self.papers.items():
            if len(paper["all_categories"]) < min_categories:
                continue

            text = (paper["title"] + " " + paper["abstract"]).lower()

            if keywords:
                if not any(kw.lower() in text for kw in keywords):
                    continue

            if exclude:
                if any(kw.lower() in text for kw in exclude):
                    continue

            filtered[arxiv_id] = paper

        return filtered

    def save_results(
        self,
        filename: Optional[str] = None,
        format: str = "json",
        from_date: Optional[str] = None,
        until_date: Optional[str] = None,
        outdir: Optional[str] = None,
    ) -> None:
        """Save aggregated papers to file.

        Args:
            filename: Output filename (auto-generated if None)
            format: Output format ('json' or 'markdown')
            from_date: Start date for filename generation (YYYY-MM-DD)
            until_date: End date for filename generation (YYYY-MM-DD)
            outdir: Output directory (uses XDG data dir if None)
        """
        if outdir is None:
            xdg_data_home = os.environ.get(
                "XDG_DATA_HOME", os.path.expanduser("~/.local/share")
            )
            outdir = os.path.join(xdg_data_home, "harvester")

        dirpath = Path(outdir).expanduser()
        try:
            dirpath.mkdir(parents=True, exist_ok=True)
        except PermissionError:
            print(f"Error: Permission denied creating directory '{dirpath}'")
            print(
                "Please check that you have write access to the parent directory or specify a different output directory with --output-dir"
            )
            sys.exit(1)
        except OSError as e:
            print(f"Error: Failed to create directory '{dirpath}': {e}")
            sys.exit(1)

        if filename is None:
            if from_date and until_date:
                start = from_date.replace("-", "")
                end = until_date.replace("-", "")
                filename = f"arxiv-papers-{start}-{end}.{format}"
            else:
                today = datetime.now().strftime("%Y%m%d")
                filename = f"arxiv-papers-{today}.{format}"

        filename = self._sanitize_filename(filename)
        outpath = dirpath / filename

        if format == "json":
            with open(outpath, "w") as f:
                json.dump(self.papers, f, indent=2, default=str)

        elif format == "markdown":
            self._save_markdown(outpath)
        else:
            raise ValueError(f"Unsupported format: {format}")

        print(f"Saved {len(self.papers)} papers to {outpath}")

    def _save_markdown(self, filepath: Path) -> None:
        """Save papers in Markdown format."""
        with open(filepath, "w") as f:
            f.write(f"# ArXiv Papers - {datetime.now().strftime('%Y-%m-%d')}\n\n")
            f.write(f"Total papers: {len(self.papers)}\n\n")

            f.write("## Statistics by Category\n\n")
            for cat in self.categories:
                f.write(f"- **{cat}**: {self.category_counts[cat]} papers\n")
            f.write("\n---\n\n")

            papers_by_category = defaultdict(list)
            for paper in self.papers.values():
                papers_by_category[paper["primary_category"]].append(paper)

            for category in self.categories:
                papers = papers_by_category[category]
                if not papers:
                    continue

                f.write(f"## {category}\n\n")

                for paper in papers:
                    f.write(f"### [{paper['title']}]({paper['link']})\n\n")
                    f.write(f"**Authors**: {', '.join(paper['authors'][:5])}")
                    if len(paper["authors"]) > 5:
                        f.write(f" et al. ({len(paper['authors'])} authors)")
                    f.write("\n\n")
                    f.write(f"**Categories**: {', '.join(paper['all_categories'])}\n\n")
                    f.write(f"**Abstract**: {paper['abstract'][:500]}...")
                    f.write(f"\n\n[PDF]({paper['pdf_link']}) | ")
                    f.write(f"[arXiv:{paper['arxiv_id']}]({paper['link']})\n\n")
                    f.write("---\n\n")

    def print_summary(self) -> None:
        """Print summary statistics."""
        print("\n" + "=" * 50)
        print("SUMMARY")
        print("=" * 50)
        print(f"Total unique papers: {len(self.papers)}")
        print("\nPapers per category:")
        for cat in self.categories:
            print(f"  {cat}: {self.category_counts[cat]}")

        crosslisted = sum(
            1 for p in self.papers.values() if len(p["all_categories"]) > 1
        )
        print(f"\nCross-listed papers: {crosslisted}")

        if self.papers:
            dates = []
            for p in self.papers.values():
                try:
                    date = datetime.strptime(p["published"][:10], "%Y-%m-%d")
                except ValueError:
                    try:
                        from email.utils import parsedate_to_datetime

                        date = parsedate_to_datetime(p["published"])
                    except (ValueError, TypeError):
                        continue
                dates.append(date)

            if dates:
                start = min(dates)
                end = max(dates)
                print(f"\nDate range: {start.date()} to {end.date()}")


def main():
    config = load_config()

    parser = argparse.ArgumentParser(
        description=f"ArXiv Paper Harvester v{__version__} - Aggregate papers from multiple arXiv categories",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
EXAMPLES:
  # Get today's papers via RSS (default)
  %(prog)s

  # Harvest specific date range using OAI-PMH
  %(prog)s --method oai --from-date 2024-01-15 --until-date 2024-01-20

  # Get last week's papers with custom output
  %(prog)s --method oai --days-back 7 --output-dir ~/research/papers

  # Filter for specific topics
  %(prog)s --keywords transformer attention --exclude survey

  # Use specific categories
  %(prog)s --categories cs.LG stat.ML cs.AI

CONFIGURATION:
  Config file: ~/.config/harvester/config.json
  Output dir:  ~/.local/share/harvester/ (default)

  Run with --save-config-example to create an example configuration file.

CATEGORIES:
  Common ML/AI categories:
    cs.LG      - Machine Learning
    stat.ML    - Machine Learning (Statistics)
    cs.AI      - Artificial Intelligence
    cs.CV      - Computer Vision
    cs.CL      - Computation and Language
    cs.NE      - Neural and Evolutionary Computing

  Physics/Math categories:
    cond-mat.stat-mech - Statistical Mechanics
    cond-mat.dis-nn    - Disordered Systems and Neural Networks
    physics.data-an    - Data Analysis, Statistics and Probability
    math.OC            - Optimization and Control
    nlin.AO            - Adaptation and Self-Organizing Systems
        """
        % {"prog": os.path.basename(sys.argv[0]).replace(".py", "")},
    )

    parser.add_argument(
        "-v", "--version", action="version", version=f"%(prog)s {__version__}"
    )
    parser.add_argument(
        "--save-config-example",
        action="store_true",
        help="Save an example configuration file and exit",
    )

    parser.add_argument(
        "--categories",
        nargs="+",
        default=config["categories"],
        help="List of arXiv categories to aggregate",
    )

    parser.add_argument(
        "--method",
        choices=["rss", "oai"],
        default=config["method"],
        help="Harvesting method: rss for recent papers, oai for date ranges",
    )

    parser.add_argument(
        "--from-date", type=str, help="Start date for OAI harvesting (YYYY-MM-DD)"
    )
    parser.add_argument(
        "--until-date", type=str, help="End date for OAI harvesting (YYYY-MM-DD)"
    )
    parser.add_argument(
        "--days-back",
        type=int,
        help="Harvest papers from last N days (alternative to date range)",
    )

    parser.add_argument("--output", type=str, help="Output filename")
    parser.add_argument(
        "--output-dir",
        type=str,
        default=config.get("output_dir"),
        help="Output directory (default: ~/.local/share/harvester)",
    )
    parser.add_argument(
        "--format",
        choices=["json", "markdown"],
        default=config["format"],
        help="Output format (default: %(default)s)",
    )

    parser.add_argument("--keywords", nargs="+", help="Filter by keywords")
    parser.add_argument(
        "--exclude", nargs="+", help="Exclude papers with these keywords"
    )
    parser.add_argument(
        "--min-categories",
        type=int,
        default=1,
        help="Minimum number of categories for paper inclusion",
    )

    args = parser.parse_args()

    if args.save_config_example:
        config_path = save_default_config()
        print(f"Example configuration saved to: {config_path}")
        print("Copy it to config.json in the same directory and modify as needed.")
        sys.exit(0)

    if args.days_back and (args.from_date or args.until_date):
        parser.error(
            "--days-back cannot be used with --from-date or --until-date. Use either --days-back OR specify date range with --from-date/--until-date."
        )

    aggregator = ArXivAggregator(args.categories, use_oai=(args.method == "oai"))

    fromdate = None
    untildate = None

    try:
        if args.method == "oai":
            if args.days_back:
                end_date = datetime.now().date()
                start_date = end_date - timedelta(days=args.days_back)
                fromdate = start_date.strftime("%Y-%m-%d")
                untildate = end_date.strftime("%Y-%m-%d")
            elif args.from_date and args.until_date:
                fromdate = args.from_date
                untildate = args.until_date
            else:
                date = datetime.now().date() - timedelta(days=1)
                fromdate = date.strftime("%Y-%m-%d")
                untildate = date.strftime("%Y-%m-%d")

            print(f"Harvesting papers from {fromdate} to {untildate} using OAI-PMH...")
            aggregator.harvest_date_range(fromdate, untildate)
        else:
            print(
                f"Fetching recent papers from {len(args.categories)} categories using RSS..."
            )
            aggregator.aggregate()

        if args.keywords or args.exclude or args.min_categories > 1:
            print("\nApplying filters...")
            filtered = aggregator.filter_papers(
                keywords=args.keywords,
                exclude=args.exclude,
                min_categories=args.min_categories,
            )
            aggregator.papers = filtered
            print(f"Filtered to {len(filtered)} papers")

        output_format = args.format
        if args.output and "." in args.output:
            file_ext = args.output.lower().split(".")[-1]
            if file_ext in ["json", "js"]:
                detected_format = "json"
            elif file_ext in ["md", "markdown"]:
                detected_format = "markdown"
            else:
                detected_format = None

            if detected_format and output_format == config["format"]:
                output_format = detected_format
                print(f"Auto-detected {detected_format} format from file extension")
            elif detected_format and detected_format != output_format:
                print(
                    f"Warning: File extension suggests {detected_format} format, but {output_format} format specified"
                )

        aggregator.save_results(
            args.output, output_format, fromdate, untildate, args.output_dir
        )

        aggregator.print_summary()

    except KeyboardInterrupt:
        print("\n\nOperation cancelled by user.")
        if hasattr(aggregator, "papers") and aggregator.papers:
            try:
                print(f"Saving {len(aggregator.papers)} papers collected so far...")
                output_format = args.format
                if args.output and "." in args.output:
                    file_ext = args.output.lower().split(".")[-1]
                    if file_ext in ["json", "js"] and output_format == config["format"]:
                        output_format = "json"
                    elif (
                        file_ext in ["md", "markdown"]
                        and output_format == config["format"]
                    ):
                        output_format = "markdown"

                aggregator.save_results(
                    args.output, output_format, fromdate, untildate, args.output_dir
                )
                print("Partial results saved successfully.")
            except Exception as e:
                print(f"Failed to save partial results: {e}")
        sys.exit(130)


if __name__ == "__main__":
    main()
