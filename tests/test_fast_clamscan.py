"""Integration checks using real ClamAV and harmless local test signatures.

Run: python3 -m unittest discover -s tests -p 'test_fast_clamscan.py' -v
No downloads, root privileges, or official virus definitions are required.
"""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import textwrap
import unittest


REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "scripts" / "fast-clamscan.sh"
MARKER = "HARMLESS_CLAMAV_REGRESSION_MARKER"


class FastClamscanTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        for tool in ("clamscan", "parallel", "bash"):
            if not shutil.which(tool):
                raise unittest.SkipTest(f"Missing integration dependency: {tool}")

    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix=".clamav-tests-", dir=REPO)
        self.root = Path(self.scratch.name)
        self.target = self.root / "target"
        self.target.mkdir()
        self.db = self.root / "review.ndb"
        self.db.write_text(f"Review.Marker:0:*:{MARKER.encode().hex()}\n")
        self.report = self.root / "report"
        self.env = os.environ.copy()
        # Isolate GNU parallel from personal profiles and shell selection.
        self.env.update(PARALLEL="--will-cite", PARALLEL_SHELL=shutil.which("bash"))
        self.env.pop("SUDO_USER", None)

    def tearDown(self):
        for path in self.root.rglob("*"):
            if path.is_file():
                path.chmod(0o600)
        self.scratch.cleanup()

    def fixture(self, name="clean.txt", infected=False):
        path = self.target / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(MARKER if infected else "Harmless plain text\n")
        return path

    def run_scan(self, *options, targets=None, db=None, report=None):
        command = [
            "bash", str(SCRIPT), "--threads", "2", "--chunk", "2", "--no-size",
            "--db", str(db if db is not None else self.db),
            "--out", str(report if report is not None else self.report),
            *options, "--",
            *(str(p) for p in (targets if targets is not None else [self.target])),
        ]
        result = subprocess.run(
            command, cwd=self.root, env=self.env, text=True,
            capture_output=True, timeout=30,
        )
        metrics_path = Path(report if report is not None else self.report) / "metrics.json"
        if not metrics_path.is_absolute():
            metrics_path = self.root / metrics_path
        metrics = json.loads(metrics_path.read_text()) if metrics_path.exists() else None
        return result, metrics

    def fake_tool(self, name, body):
        directory = self.root / "bin"
        directory.mkdir(exist_ok=True)
        tool = directory / name
        tool.write_text("#!/usr/bin/env python3\n" + textwrap.dedent(body))
        tool.chmod(0o700)
        self.env["PATH"] = str(directory) + os.pathsep + self.env["PATH"]

    def test_clean_scan_and_latest_link(self):
        self.fixture()
        result, metrics = self.run_scan(report="nested/report")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(metrics["total_files"], 1)
        self.assertEqual(metrics["clean_approx"], 1)
        self.assertTrue(metrics["scan_complete"])
        self.assertEqual(metrics["completed_jobs"], 1)
        link = self.root / "nested" / "clam-report-latest"
        self.assertEqual(link.resolve(), self.root / "nested" / "report")

    def test_detection_in_names_with_spaces_quotes_colons_and_shell_syntax(self):
        names = ["a: marker.txt", "space and 'quote'.txt", 'dollar$(touch PWNED)`x`.txt']
        for name in names:
            self.fixture(name, infected=True)
        self.fixture()
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(metrics["infected"], len(names))
        self.assertEqual(metrics["clean_approx"], 1)
        self.assertEqual(metrics["completed_jobs"], 2)
        detections = (self.report / "infected.tsv").read_text()
        for name in names:
            self.assertIn(str(self.target / name) + "\tReview.Marker.UNOFFICIAL", detections)
        self.assertFalse((self.root / "PWNED").exists())

    def test_infected_report_has_found_lines_and_one_consolidated_summary(self):
        infected = self.fixture("infected marker.txt", infected=True)
        clean = self.fixture("clean marker.txt")
        compact_report = self.root / "reports" / "scan_report.txt"
        result, metrics = self.run_scan(
            "--infected-report", str(compact_report),
        )
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        report_text = compact_report.read_text()
        self.assertIn(
            f"{infected}: Review.Marker.UNOFFICIAL FOUND", report_text,
        )
        self.assertNotIn(str(clean), report_text)
        self.assertEqual(report_text.count("----------- SCAN SUMMARY -----------"), 1)
        self.assertIn("Known viruses: 1", report_text)
        self.assertIn("Files enumerated: 2", report_text)
        self.assertIn("Scanned files: 2", report_text)
        self.assertIn("Infected files: 1", report_text)
        self.assertIn("Total errors: 0", report_text)
        self.assertIn("Scan complete: true", report_text)
        self.assertEqual(compact_report.stat().st_mode & 0o777, 0o600)
        self.assertEqual(metrics["infected_report"], str(compact_report))

    @unittest.skipIf(os.geteuid() == 0, "Root can read chmod 000 fixtures")
    def test_unreadable_file_is_an_error_not_clean(self):
        self.fixture().chmod(0)
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(metrics["failed_jobs"], 1)
        self.assertFalse(metrics["scan_complete"])
        self.assertIsNone(metrics["clean_approx"])
        self.assertGreater(metrics["errors_total"], 0)

    @unittest.skipIf(os.geteuid() == 0, "Root can read chmod 000 fixtures")
    def test_scan_errors_take_precedence_over_detections_in_same_batch(self):
        self.fixture("a-detected.txt", infected=True)
        self.fixture("b-unreadable.txt").chmod(0)
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(metrics["infected"], 1)
        self.assertIsNone(metrics["clean_approx"])

    @unittest.skipIf(os.geteuid() == 0, "Root can enumerate chmod 000 directories")
    def test_enumeration_errors_mark_scan_incomplete(self):
        self.fixture()
        denied = self.fixture("denied/hidden.txt").parent
        denied.chmod(0)
        try:
            result, metrics = self.run_scan()
        finally:
            denied.chmod(0o700)
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertGreater(metrics["enum_errors"], 0)
        self.assertFalse(metrics["scan_complete"])

    def test_missing_option_value_exits_with_error(self):
        result = subprocess.run(
            ["bash", str(SCRIPT), "--db-max-age"],
            cwd=self.root, env=self.env, text=True, capture_output=True,
        )
        self.assertEqual(result.returncode, 2)
        self.assertIn("requires a value", result.stderr)

    def test_missing_target_marks_remaining_scan_incomplete(self):
        self.fixture()
        result, metrics = self.run_scan(targets=[self.target, self.root / "missing"])
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(metrics["skipped_targets"], 1)
        self.assertIsNone(metrics["clean_approx"])

    def test_explicit_missing_database_is_rejected(self):
        self.fixture()
        result, metrics = self.run_scan(db=self.root / "missing-db")
        self.assertEqual(result.returncode, 2)
        self.assertIn("Database path does not exist", result.stderr)
        self.assertIsNone(metrics)

    def test_invalid_clamscan_option_fails_preflight(self):
        self.fixture()
        result, metrics = self.run_scan("--extra-arg", "--not-a-clamav-option")
        self.assertEqual(result.returncode, 2)
        self.assertIn("not usable", result.stderr)
        self.assertIsNone(metrics)

    def test_file_selection_scanner_options_are_rejected(self):
        self.fixture()
        for option in ("--file-list=other.txt", "--exclude=.*", "--include=.*", "--recursive=yes"):
            with self.subTest(option=option):
                report = self.root / ("report-" + str(abs(hash(option))))
                result, metrics = self.run_scan("--extra-arg", option, report=report)
                self.assertEqual(result.returncode, 2)
                self.assertIn("incompatible with coverage accounting", result.stderr)
                self.assertIsNone(metrics)

    def test_overlapping_targets_are_deduplicated(self):
        file = self.fixture()
        result, metrics = self.run_scan(targets=[self.target, file, self.target])
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(metrics["total_files"], 1)

    def test_relative_target_literal_prune_and_report_exclusion(self):
        self.fixture()
        skipped = self.fixture("skip[*]?/marker.txt", infected=True).parent
        report = self.target / "report[*]?"
        result, metrics = self.run_scan("--prune", str(skipped), targets=["target"], report=report)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(metrics["total_files"], 1)
        listed = (report / "filelist.nul").read_bytes().split(b"\0")
        self.assertEqual([p for p in listed if p], [os.fsencode(self.target / "clean.txt")])

    def test_empty_target_has_distinct_exit(self):
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 4)
        self.assertIsNone(metrics)

    def test_killed_worker_is_an_error(self):
        self.fixture()
        self.fake_tool("clamscan", """
            import os, signal, sys
            if any(a.endswith('/.db-probe-file') for a in sys.argv[1:]):
                sys.exit(0)
            os.kill(os.getpid(), signal.SIGKILL)
        """)
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(metrics["failed_jobs"], 1)
        self.assertIsNone(metrics["clean_approx"])

    def test_parallel_failure_without_joblog_is_an_error(self):
        self.fixture()
        self.fake_tool("parallel", """
            import sys
            print('Runner failed', file=sys.stderr)  # noqa: print
            sys.exit(1)
        """)
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(metrics["runner_errors"], 1)
        self.assertEqual(metrics["completed_jobs"], 0)

    def test_allmatch_counts_unique_files(self):
        self.db.write_text(self.db.read_text() + f"Review.Second:0:*:{MARKER.encode().hex()}\n")
        self.fixture(infected=True)
        result, metrics = self.run_scan("--extra-arg", "--allmatch=yes")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(metrics["detections"], 2)
        self.assertEqual(metrics["infected"], 1)
        self.assertEqual(metrics["clean_approx"], 0)

    def test_size_limit_is_visible_and_marks_coverage_incomplete(self):
        self.fixture(infected=True)
        result, metrics = self.run_scan("--extra-arg", "--max-filesize=32")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertGreater(metrics["limit_alerts"], 0)
        self.assertFalse(metrics["scan_complete"])
        self.assertIsNone(metrics["clean_approx"])

    def test_json_escapes_control_characters_in_target_names(self):
        unusual = self.target / "tab\tand\nnewline"
        unusual.mkdir()
        (unusual / "clean.txt").write_text("Harmless text\n")
        result, metrics = self.run_scan(targets=[unusual])
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(metrics["targets"], str(unusual))

    def test_existing_report_directory_is_not_overwritten(self):
        self.fixture()
        self.report.mkdir()
        sentinel = self.report / "keep.txt"
        sentinel.write_text("keep me")
        result, _ = self.run_scan()
        self.assertEqual(result.returncode, 2)
        self.assertEqual(sentinel.read_text(), "keep me")

    def test_existing_regular_latest_file_is_preserved(self):
        self.fixture()
        latest = self.root / "clam-report-latest"
        latest.write_text("keep me")
        result, metrics = self.run_scan()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(latest.read_text(), "keep me")
        self.assertTrue(metrics["scan_complete"])

    def test_update_db_uses_selected_directory_and_no_system_configuration(self):
        self.fixture()
        database_dir = self.root / "local definitions"
        self.env["REVIEW_DB_SOURCE"] = str(self.db)
        self.fake_tool("freshclam", """
            import os, pathlib, shlex, shutil, sys
            config = next(a.split('=', 1)[1] for a in sys.argv[1:] if a.startswith('--config-file='))
            settings = dict(shlex.split(line) for line in pathlib.Path(config).read_text().splitlines())
            assert settings['DatabaseMirror'] == 'database.clamav.net'
            shutil.copy(os.environ['REVIEW_DB_SOURCE'], pathlib.Path(settings['DatabaseDirectory']) / 'review.ndb')
        """)
        result, metrics = self.run_scan("--update-db", db=database_dir)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(metrics["db_dir_used"], str(database_dir))
        self.assertTrue((database_dir / "review.ndb").exists())

    def test_failed_database_update_stops_before_scanning(self):
        self.fixture()
        self.fake_tool("freshclam", """
            import sys
            print('Test update failure')  # noqa: print
            sys.exit(1)
        """)
        result, metrics = self.run_scan("--update-db", db=self.root / "local-db")
        self.assertEqual(result.returncode, 2)
        self.assertIn("Database update failed", result.stderr)
        self.assertFalse((self.report / "filelist.nul").exists())
        self.assertIsNone(metrics)


if __name__ == "__main__":
    unittest.main()
