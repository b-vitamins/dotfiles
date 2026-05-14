#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from pathlib import Path


FORBIDDEN_MULTI_SECTION_MODES = {"manual", "local", "local-batch", "serial", "parent"}
FORBIDDEN_MULTI_SECTION_WORKERS = {"", "parent", "not-used", "local", "none", "n/a"}
REVIEW_CANDIDATES = [
    "tts-acceptance-review.csv",
    "tts-acceptance-review.md",
    "tts-line-by-line-audit.md",
    "tts-line-by-line-audit.csv",
    "tts-audit-report.md",
]


def load_csv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def yes_no(value: bool) -> str:
    return "yes" if value else "no"


def resolve(root: Path, raw_path: str) -> Path:
    path = Path(raw_path)
    return path if path.is_absolute() else root / path


def source_plan_dirs(plans_dir: Path) -> list[Path]:
    if (plans_dir / "tts-sections.csv").exists():
        return [plans_dir]
    return sorted(path for path in plans_dir.iterdir() if (path / "tts-sections.csv").exists())


def find_acceptance_review_file(plan_dir: Path) -> Path | None:
    for name in REVIEW_CANDIDATES:
        candidate = plan_dir / name
        if candidate.exists() and candidate.read_text(encoding="utf-8").strip():
            return candidate
    return None


def split_targets(raw_targets: str) -> set[str]:
    return {target.strip() for target in raw_targets.split("|") if target.strip()}


def assignment_targets(rows: list[dict[str, str]]) -> set[str]:
    targets: set[str] = set()
    for row in rows:
        targets.update(split_targets(row.get("target_txt", "")))
    return targets


def mode_is_serial(mode: str) -> bool:
    normalized = mode.strip().lower()
    if normalized in FORBIDDEN_MULTI_SECTION_MODES:
        return True
    return normalized.startswith("local") or normalized.startswith("serial")


def worker_is_parent(worker_id: str) -> bool:
    return worker_id.strip().lower() in FORBIDDEN_MULTI_SECTION_WORKERS


def audit_plan_dir(root: Path, plan_dir: Path) -> dict[str, str]:
    plan_rows = load_csv(plan_dir / "tts-sections.csv")
    validation_rows = load_csv(plan_dir / "tts-validation.csv")
    assignment_rows = load_csv(plan_dir / "tts-worker-assignments.csv")
    acceptance_rows = load_csv(plan_dir / "tts-acceptance-sample.csv")

    source_brief = plan_dir / "tts-source-brief.md"
    acceptance_review = find_acceptance_review_file(plan_dir)
    expected_targets = {row.get("target_txt", "") for row in plan_rows if row.get("target_txt")}
    assigned_targets = assignment_targets(assignment_rows)

    issues: list[str] = []
    warnings: list[str] = []

    source_covered = bool(plan_rows) and all(
        resolve(root, target).exists() and resolve(root, target).read_text(encoding="utf-8").strip()
        for target in expected_targets
    )
    if not source_covered:
        issues.append("source-not-covered")

    source_briefed = source_brief.exists() and bool(source_brief.read_text(encoding="utf-8").strip())
    if not source_briefed:
        issues.append("missing-source-brief")

    validation_present = bool(validation_rows)
    validation_failures = sum(1 for row in validation_rows if row.get("status") == "fail")
    validation_warnings = sum(1 for row in validation_rows if row.get("status") == "warn")
    validated = validation_present and validation_failures == 0
    if not validation_present:
        issues.append("missing-validation")
    elif validation_failures:
        issues.append(f"validation-failures:{validation_failures}")

    if not acceptance_rows:
        issues.append("missing-acceptance-sample")
    if acceptance_review is None:
        issues.append("missing-acceptance-review")

    if not assignment_rows:
        issues.append("missing-worker-assignments")
    elif any(row.get("status", "").strip().lower() != "completed" for row in assignment_rows):
        issues.append("incomplete-worker-assignments")

    missing_assignment_targets = sorted(expected_targets - assigned_targets)
    extra_assignment_targets = sorted(assigned_targets - expected_targets)
    if missing_assignment_targets:
        issues.append("assignment-missing-targets:" + "|".join(missing_assignment_targets[:8]))
    if extra_assignment_targets:
        warnings.append("assignment-extra-targets:" + "|".join(extra_assignment_targets[:8]))

    serial_rows: list[str] = []
    if len(plan_rows) > 1:
        for row in assignment_rows:
            if mode_is_serial(row.get("mode", "")) or worker_is_parent(row.get("worker_id", "")):
                serial_rows.append(row.get("assignment_id", "?"))
    if serial_rows:
        issues.append("serial-or-parent-dispatch:" + "|".join(serial_rows[:12]))

    ready = source_covered and source_briefed and validated and bool(acceptance_rows) and acceptance_review is not None and not issues
    return {
        "source": plan_dir.name,
        "section_count": str(len(plan_rows)),
        "source-covered": yes_no(source_covered),
        "source-briefed": yes_no(source_briefed),
        "validated": yes_no(validated),
        "validation_failures": str(validation_failures),
        "validation_warnings": str(validation_warnings),
        "acceptance-sampled": yes_no(bool(acceptance_rows)),
        "acceptance-reviewed": yes_no(acceptance_review is not None),
        "worker-assignments-complete": yes_no(bool(assignment_rows) and all(row.get("status", "").strip().lower() == "completed" for row in assignment_rows)),
        "native-or-worker-dispatch": yes_no(not serial_rows),
        "ready-for-tts": yes_no(ready),
        "issues": "; ".join(issues),
        "warnings": "; ".join(warnings),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Audit orchestration and readiness artifacts for a HiFi Math TTS run.")
    parser.add_argument("--root", default=".", help="Workspace root.")
    parser.add_argument("--plans-dir", default="plans", help="Plans directory or a single source plan directory.")
    parser.add_argument("--out", required=True, help="Output audit CSV.")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    plans_dir = resolve(root, args.plans_dir)
    rows = [audit_plan_dir(root, plan_dir) for plan_dir in source_plan_dirs(plans_dir)]

    out_path = resolve(root, args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = [
        "source",
        "section_count",
        "source-covered",
        "source-briefed",
        "validated",
        "validation_failures",
        "validation_warnings",
        "acceptance-sampled",
        "acceptance-reviewed",
        "worker-assignments-complete",
        "native-or-worker-dispatch",
        "ready-for-tts",
        "issues",
        "warnings",
    ]
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)

    ready_count = sum(1 for row in rows if row["ready-for-tts"] == "yes")
    issue_count = sum(1 for row in rows if row["issues"])
    print(f"audited {len(rows)} sources: {ready_count} ready-for-tts, {issue_count} with issues")  # noqa: print
    return 1 if issue_count else 0


if __name__ == "__main__":
    raise SystemExit(main())
