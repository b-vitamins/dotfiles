#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

from paper_utils import ARXIV_ABS, ARXIV_FETCH_MANIFEST_FILENAME, ARXIV_PDF, ARXIV_SRC, artifact_slug, ensure_dir, extract_html_meta_tags, extract_source_bundle, fetch_url, normalize_arxiv_id, read_text, write_json


def parse_abstract_metadata(html_path: Path) -> dict[str, object]:
    html_text = read_text(html_path)
    tags = extract_html_meta_tags(html_text)
    authors = [author.strip() for author in re.findall(r'<meta\s+name="citation_author"\s+content="([^"]*)"', html_text, flags=re.IGNORECASE) if author.strip()]
    return {
        "title": tags.get("citation_title"),
        "authors": authors,
        "doi": tags.get("citation_doi"),
        "arxiv_id": tags.get("citation_arxiv_id"),
        "pdf_url": tags.get("citation_pdf_url"),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Fetch arXiv abstract, PDF, and source bundle.")
    parser.add_argument("--input", required=True, help="arXiv URL or identifier")
    parser.add_argument("--output-dir", required=True, help="Directory for fetched artifacts")
    args = parser.parse_args()

    arxiv_id = normalize_arxiv_id(args.input)
    if not arxiv_id:
        raise SystemExit("could not normalize arXiv identifier")

    output_dir = ensure_dir(args.output_dir)
    metadata_dir = ensure_dir(output_dir / "metadata")
    pdf_dir = ensure_dir(output_dir / "pdf")
    source_dir = ensure_dir(output_dir / "source")

    artifact_id = artifact_slug(arxiv_id)
    abstract_html = metadata_dir / f"{artifact_id}.html"
    pdf_file = pdf_dir / f"{artifact_id}.pdf"
    raw_source = source_dir / f"{artifact_id}.src"
    extracted_source = source_dir / "extracted"

    manifest: dict[str, object] = {
        "arxiv_id": arxiv_id,
        "success": False,
        "downloads": {},
        "extraction": {},
        "errors": [],
        "paths": {
            "abstract_html": str(abstract_html),
            "pdf": str(pdf_file),
            "source_raw": str(raw_source),
            "source_extract_dir": str(extracted_source),
        },
    }

    try:
        manifest["downloads"]["abstract"] = fetch_url(ARXIV_ABS.format(id=arxiv_id), abstract_html)
        manifest["metadata"] = parse_abstract_metadata(abstract_html)
    except Exception as exc:
        manifest["errors"].append(f"abstract fetch failed: {exc}")

    try:
        manifest["downloads"]["pdf"] = fetch_url(ARXIV_PDF.format(id=arxiv_id), pdf_file)
    except Exception as exc:
        manifest["errors"].append(f"pdf fetch failed: {exc}")

    try:
        manifest["downloads"]["source"] = fetch_url(ARXIV_SRC.format(id=arxiv_id), raw_source)
        manifest["extraction"] = extract_source_bundle(raw_source, extracted_source)
        manifest["success"] = True
    except Exception as exc:
        manifest["errors"].append(f"source fetch or extraction failed: {exc}")

    write_json(output_dir / ARXIV_FETCH_MANIFEST_FILENAME, manifest)
    sys.stdout.write(json.dumps(manifest, indent=2) + "\n")
    return 0 if manifest["success"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
