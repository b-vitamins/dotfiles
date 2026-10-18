#!/usr/bin/env python3
from __future__ import annotations

import argparse
import shutil
from pathlib import Path


TRANSIENT_FILE_NAMES = {
    "codex-last-message.txt",
    "codex-run.log",
}


def remove_path(path: Path, dry_run: bool) -> None:
    if not path.exists():
        return
    print(f"remove {path}")  # noqa: print
    if dry_run:
        return
    if path.is_dir():
        shutil.rmtree(path)
    else:
        path.unlink()


def main() -> int:
    parser = argparse.ArgumentParser(description="Remove transient HiFi Math TTS run artifacts after successful validation.")
    parser.add_argument("--root", default=".", help="Workspace root.")
    parser.add_argument("--scripts-dir", default="scripts", help="Final scripts directory that must exist.")
    parser.add_argument("--plans-dir", default="plans", help="Transient plans directory to remove.")
    parser.add_argument("--dry-run", action="store_true", help="Print cleanup actions without deleting anything.")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    scripts_dir = root / args.scripts_dir
    plans_dir = root / args.plans_dir

    if not scripts_dir.is_dir() or not any(scripts_dir.rglob("*.txt")):
        raise SystemExit(f"refusing cleanup: no generated script files found under {scripts_dir}")

    remove_path(plans_dir, args.dry_run)

    for path in root.rglob("*"):
        if path.name in TRANSIENT_FILE_NAMES:
            remove_path(path, args.dry_run)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
