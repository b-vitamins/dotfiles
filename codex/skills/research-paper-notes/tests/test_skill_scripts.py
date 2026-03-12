from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path

SKILL_ROOT = Path("/home/b/projects/dotfiles/codex/skills/research-paper-notes")
SCRIPTS_DIR = SKILL_ROOT / "scripts"


def run_script(script_name: str, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPTS_DIR / script_name), *args],
        capture_output=True,
        text=True,
        check=False,
    )


def valid_notes_text() -> str:
    payload = {
        "title": "Example Paper",
        "doi": "10.1000/example",
        "venue": None,
        "year": 2026,
        "paper_type": "empirical",
        "external_ids": {
            "arxiv": "2601.00001",
            "doi": "10.1000/example",
            "openreview_forum": None,
            "openalex": None,
            "semantic_scholar": None,
        },
        "source_version": "2601.00001v1",
        "source_coverage": "combined TeX source (120 lines, 1 chunks); companion PDF staged",
        "source_provenance": {
            "input_type": "arxiv_source_bundle",
            "input_label": "2601.00001",
            "tex_source_authority": "TeX",
            "companion_pdf_used": False,
        },
        "input_quality": "native text",
        "tex_source_gate": "PASS",
        "readthrough_complete": True,
        "readthrough_chunks": {"completed": 1, "total": 1},
        "coordinates": {
            "purpose": ["representation-learning"],
            "problem_form": ["self-supervised"],
            "learning_signal": ["predictive"],
            "mechanism": ["latent-dynamics"],
            "evidence_regime": ["empirical"],
        },
        "claim_surfaces": {
            "problem": "representation learning",
            "data": "image pairs",
            "objective": "predict next latent",
            "mechanism": "shared encoder",
            "regime": "self-supervised pretraining",
            "evaluation": "classification transfer",
            "deployment": None,
        },
        "problem": "Learn predictive representations.",
        "main_claim": "Predictive pretraining improves transfer.",
        "notation": [],
        "assumptions": [],
        "results": [],
        "algorithms": [],
        "datasets": [],
        "metrics": [],
        "hardware": {},
        "effects": [],
        "artifacts": {},
        "relation_hints": [],
        "repro_thresholds": [],
        "limitations": [],
        "red_flags": [],
    }
    json_block = textwrap.indent(json.dumps(payload, indent=2), "        ")
    return textwrap.dedent(
        f"""\
        # Reader Notes: Example Paper

        ## Source coverage
        - TeX source gate: PASS
        - Source version used: 2601.00001v1
        - Source coverage: combined TeX source (120 lines, 1 chunks); companion PDF staged
        - Source provenance: arxiv_source_bundle
        - External IDs: arxiv: 2601.00001; doi: 10.1000/example
        - Input quality: native text
        - Figure availability: source assets
        - Readthrough coverage: 1/1 chunks
        - Confidence: high

        ## 0) Header
        - Citation: Example Paper
        - External IDs: arxiv: 2601.00001; doi: 10.1000/example
        - Coordinates:
          - purpose: representation-learning
          - problem form: self-supervised
          - learning signal: predictive
          - mechanism: latent-dynamics
          - evidence regime: empirical

        ## 1) One-sentence claim (verifiable)
        [P] Anchor: §1; Predictive pretraining improves transfer accuracy on the reported benchmark.

        ## 2) Problem & setup (canonicalized)
        Learn predictive representations from image pairs.

        ## 3) Notation & constants table
        | symbol | meaning | type/units | default/typical value | source anchor | normalization note |
        | --- | --- | --- | --- | --- | --- |

        ## 4) Method/Model (algorithmic core)
        Shared encoder with predictive objective.

        ## 5) Theory/Derivation summary
        [P] Anchor: Theorem 1; The paper proves consistency under bounded noise.

        ## 6) Assumptions & conditions ledger
        A1. Bounded noise in latent space.

        ## 7) Experiments: reproduction checklist
        Dataset, metrics, and hardware are listed in the paper.

        ## 8) Results with matched deltas
        [P] Anchor: Table 1; The model reaches 82.0 accuracy under the main setting.
        [D] Basis: Table 1 reports 82.0 vs 80.0 for the matched baseline; absolute delta is +2.0 points.

        | metric | dataset/task | condition | reported value/uncertainty | N/seeds | abs delta vs matched baseline | relative delta | CI or p-value | test | caveat |
        | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
        | acc | example-set | main | 82.0 | 3 | +2.0 | +2.5% | Not reported | Not reported | matched baseline only |

        ## 9) Figures -> findings map
        Figure 1 shows the pipeline.

        ## 10) Comparison to prior work
        [P] Anchor: §2; Relation: `compares_to`; The paper compares directly against a masked-prediction baseline.

        ## 11) External validity & limits
        [P] Anchor: §6; The results are stated only for image data and may not transfer to text.

        ## 12) Threats to validity
        [P] Anchor: §6; The ablations use only one dataset family.

        ## 13) Vital verbatim sentences
        - definition: "Predictive learning aligns latent dynamics."

        ## 14) Reproduction/verification plan
        Re-run the main benchmark and verify Table 1.

        ## 15) Artifacts
        Code: Not reported.

        ## 16) Red flags & green flags
        [P] Anchor: §4; The paper reports seeds and matched baselines.

        ## 17) Who should care & why it matters
        [I] Basis: The transfer gains and full-seed reporting support a practical pretraining recipe for representation-learning builders.

        ## 18) Open questions
        [I] Basis: The paper only evaluates images; test whether the objective transfers to sequential data.

        ## 19) Machine-readable block (JSON)
        ```json
{json_block}
        ```
        """
    )


class ResearchPaperNotesScriptTests(unittest.TestCase):
    def test_index_tex_project_resolves_root_relative_nested_includes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "src"
            out = Path(tmp) / "out"
            (root / "sections").mkdir(parents=True)
            (root / "main.tex").write_text(
                textwrap.dedent(
                    """\
                    \\documentclass{article}
                    \\begin{document}
                    \\input{sections/appendix}
                    \\end{document}
                    """
                ),
                encoding="utf-8",
            )
            (root / "sections" / "appendix.tex").write_text("\\input{sections/shared}\n", encoding="utf-8")
            (root / "sections" / "shared.tex").write_text("Shared body.\n", encoding="utf-8")

            result = run_script("index_tex_project.py", "--source-root", str(root), "--output-dir", str(out))
            self.assertEqual(result.returncode, 0, msg=result.stdout + result.stderr)

            report = json.loads((out / "source-gate-report.json").read_text(encoding="utf-8"))
            self.assertTrue(report["strict_gate_passed"])
            self.assertEqual(report["unresolved_includes"], [])

            include_report = json.loads((out / "include-report.json").read_text(encoding="utf-8"))
            resolution_modes = {edge["resolution_mode"] for edge in include_report["include_edges"]}
            self.assertIn("source_root", resolution_modes)

    def test_prepare_workspace_preserves_progress_and_scaffolds_notes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "source_tree"
            output_dir = Path(tmp) / "workspaces"
            source.mkdir()
            (source / "main.tex").write_text(
                textwrap.dedent(
                    """\
                    \\documentclass{article}
                    \\begin{document}
                    Hello world.
                    \\end{document}
                    """
                ),
                encoding="utf-8",
            )

            first = run_script("prepare_paper_workspace.py", "--input", str(source), "--output-dir", str(output_dir))
            self.assertEqual(first.returncode, 0, msg=first.stdout + first.stderr)

            workspace = output_dir / "source-tree"
            self.assertTrue((workspace / "notes" / "fact-ledger.md").exists())
            self.assertTrue((workspace / "notes" / "reader-notes.md").exists())

            log_path = workspace / "manifests" / "readthrough-log.json"
            log = json.loads(log_path.read_text(encoding="utf-8"))
            log["chunks"][0]["read"] = True
            log["chunks"][0]["summary"] = "Read the only chunk."
            log["chunks"][0]["timestamp_utc"] = "2026-03-12T00:00:00+00:00"
            log["chunks_completed"] = 1
            log["completed"] = True
            log_path.write_text(json.dumps(log, indent=2), encoding="utf-8")

            second = run_script("prepare_paper_workspace.py", "--input", str(source), "--output-dir", str(output_dir))
            self.assertEqual(second.returncode, 0, msg=second.stdout + second.stderr)

            preserved = json.loads(log_path.read_text(encoding="utf-8"))
            self.assertTrue(preserved["chunks"][0]["read"])
            self.assertEqual(preserved["chunks"][0]["summary"], "Read the only chunk.")

    def test_prepare_workspace_keeps_detailed_failure_report(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "broken"
            output_dir = Path(tmp) / "workspaces"
            source.mkdir()
            (source / "main.tex").write_text(
                textwrap.dedent(
                    """\
                    \\documentclass{article}
                    \\begin{document}
                    \\input{missing}
                    \\end{document}
                    """
                ),
                encoding="utf-8",
            )

            result = run_script("prepare_paper_workspace.py", "--input", str(source), "--output-dir", str(output_dir))
            self.assertEqual(result.returncode, 2, msg=result.stdout + result.stderr)

            gate_report = json.loads((output_dir / "broken" / "manifests" / "source-gate-report.json").read_text(encoding="utf-8"))
            self.assertIn("unresolved_text_includes", gate_report["fail_reasons"])
            self.assertTrue(gate_report["unresolved_includes"])
            self.assertEqual(gate_report["unresolved_includes"][0]["target"], "missing")

    def test_validate_reader_notes_accepts_statement_prefix_grammar(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            notes_path = Path(tmp) / "reader-notes.md"
            workspace_path = Path(tmp) / "workspace.json"
            log_path = Path(tmp) / "readthrough-log.json"
            report_path = Path(tmp) / "validation-report.json"
            export_path = Path(tmp) / "reader-notes.json"

            notes_path.write_text(valid_notes_text(), encoding="utf-8")
            workspace_path.write_text(json.dumps({"strict_gate_passed": True, "input": "2601.00001"}, indent=2), encoding="utf-8")
            log_path.write_text(
                json.dumps(
                    {
                        "completed": True,
                        "chunks_completed": 1,
                        "chunks_total": 1,
                        "chunks": [{"id": "C001", "read": True, "summary": "Done", "timestamp_utc": "2026-03-12T00:00:00+00:00"}],
                    },
                    indent=2,
                ),
                encoding="utf-8",
            )

            result = run_script(
                "validate_reader_notes.py",
                "--notes",
                str(notes_path),
                "--workspace",
                str(workspace_path),
                "--readthrough-log",
                str(log_path),
                "--report-out",
                str(report_path),
                "--export-json",
                str(export_path),
            )
            self.assertEqual(result.returncode, 0, msg=result.stdout + result.stderr)
            self.assertTrue(json.loads(report_path.read_text(encoding="utf-8"))["passed"])
            self.assertTrue(export_path.exists())

    def test_validate_reader_notes_rejects_legacy_tag_syntax(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            notes_path = Path(tmp) / "reader-notes.md"
            workspace_path = Path(tmp) / "workspace.json"
            log_path = Path(tmp) / "readthrough-log.json"

            notes_path.write_text(valid_notes_text().replace("[P] Anchor: §1;", "[P; §1] ", 1), encoding="utf-8")
            workspace_path.write_text(json.dumps({"strict_gate_passed": True, "input": "2601.00001"}, indent=2), encoding="utf-8")
            log_path.write_text(
                json.dumps(
                    {
                        "completed": True,
                        "chunks_completed": 1,
                        "chunks_total": 1,
                        "chunks": [{"id": "C001", "read": True, "summary": "Done", "timestamp_utc": "2026-03-12T00:00:00+00:00"}],
                    },
                    indent=2,
                ),
                encoding="utf-8",
            )

            result = run_script(
                "validate_reader_notes.py",
                "--notes",
                str(notes_path),
                "--workspace",
                str(workspace_path),
                "--readthrough-log",
                str(log_path),
            )
            self.assertEqual(result.returncode, 2, msg=result.stdout + result.stderr)
            report = json.loads(result.stdout)
            self.assertTrue(any("missing statement prefix" in error for error in report["errors"]))


if __name__ == "__main__":
    unittest.main()
