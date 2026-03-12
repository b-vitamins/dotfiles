#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path


def load_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def save_json(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding="utf-8")


def recompute(log: dict) -> dict:
    completed = sum(1 for chunk in log["chunks"] if chunk.get("read"))
    total = len(log["chunks"])
    log["chunks_completed"] = completed
    log["chunks_total"] = total
    log["completed"] = completed == total and total > 0
    return log


def main() -> int:
    parser = argparse.ArgumentParser(description="Manage sequential readthrough logs.")
    subcommands = parser.add_subparsers(dest="command", required=True)

    mark_parser = subcommands.add_parser("mark")
    mark_parser.add_argument("--log", required=True)
    mark_parser.add_argument("--chunk-id", required=True)
    mark_parser.add_argument("--summary", required=True)

    status_parser = subcommands.add_parser("status")
    status_parser.add_argument("--log", required=True)
    status_parser.add_argument("--require-complete", action="store_true")

    args = parser.parse_args()

    if args.command == "mark":
        path = Path(args.log)
        log = load_json(path)
        target_index = None
        for index, chunk in enumerate(log["chunks"]):
            if chunk["id"] == args.chunk_id:
                target_index = index
                break
        if target_index is None:
            raise SystemExit(f"unknown chunk id: {args.chunk_id}")
        unread_before = [chunk["id"] for chunk in log["chunks"][:target_index] if not chunk.get("read")]
        if unread_before:
            raise SystemExit(f"cannot mark {args.chunk_id} before earlier chunks: {', '.join(unread_before)}")
        log["chunks"][target_index]["read"] = True
        log["chunks"][target_index]["summary"] = args.summary.strip()
        log["chunks"][target_index]["timestamp_utc"] = datetime.now(timezone.utc).isoformat()
        save_json(path, recompute(log))
        sys.stdout.write(
            json.dumps(
                {
                    "completed": log["completed"],
                    "chunks_completed": log["chunks_completed"],
                    "chunks_total": log["chunks_total"],
                },
                indent=2,
            )
            + "\n"
        )
        return 0

    path = Path(args.log)
    log = recompute(load_json(path))
    save_json(path, log)
    sys.stdout.write(
        json.dumps(
            {
                "completed": log["completed"],
                "chunks_completed": log["chunks_completed"],
                "chunks_total": log["chunks_total"],
            },
            indent=2,
        )
        + "\n"
    )
    if args.require_complete and not log["completed"]:
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
