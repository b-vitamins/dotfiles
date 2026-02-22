#!/usr/bin/env python3
"""Split a contiguous page range into non-overlapping ranges for multiple agents."""

from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Assignment:
    agent: str
    start: int
    end: int
    pages: int
    shard_file: str


def build_assignments(start: int, end: int, agents: int, agent_prefix: str, shard_prefix: str) -> list[Assignment]:
    total = end - start + 1
    base, remainder = divmod(total, agents)
    cursor = start
    out: list[Assignment] = []

    for idx in range(agents):
        size = base + (1 if idx < remainder else 0)
        if size <= 0:
            continue
        s = cursor
        e = cursor + size - 1
        agent = f"{agent_prefix}-{idx + 1}"
        shard_file = f"{shard_prefix}-p{s}-{e}.tex"
        out.append(Assignment(agent=agent, start=s, end=e, pages=size, shard_file=shard_file))
        cursor = e + 1
    return out


def write_csv(path: Path, assignments: list[Assignment]) -> None:
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["agent", "start", "end", "pages", "shard_file"])
        for item in assignments:
            writer.writerow([item.agent, item.start, item.end, item.pages, item.shard_file])


def print_table(assignments: list[Assignment]) -> None:
    print("agent,start,end,pages,shard_file")  # noqa: print
    for item in assignments:
        print(f"{item.agent},{item.start},{item.end},{item.pages},{item.shard_file}")  # noqa: print


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--start", type=int, required=True, help="First page (inclusive).")
    parser.add_argument("--end", type=int, required=True, help="Last page (inclusive).")
    parser.add_argument("--agents", type=int, required=True, help="Number of agents.")
    parser.add_argument("--agent-prefix", default="agent", help="Prefix for generated agent IDs.")
    parser.add_argument("--shard-prefix", default="transcription-shard", help="Prefix for suggested shard filenames.")
    parser.add_argument("--out", type=Path, help="Optional CSV output path.")
    parser.add_argument("--json", action="store_true", help="Print JSON instead of CSV-like table.")
    args = parser.parse_args()

    if args.start < 1 or args.end < 1:
        raise SystemExit("start/end must be >= 1")
    if args.start > args.end:
        raise SystemExit("start must be <= end")
    if args.agents < 1:
        raise SystemExit("agents must be >= 1")

    assignments = build_assignments(
        start=args.start,
        end=args.end,
        agents=args.agents,
        agent_prefix=args.agent_prefix,
        shard_prefix=args.shard_prefix,
    )

    if args.out:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        write_csv(args.out, assignments)

    if args.json:
        print(  # noqa: print
            json.dumps(
                [
                    {
                        "agent": a.agent,
                        "start": a.start,
                        "end": a.end,
                        "pages": a.pages,
                        "shard_file": a.shard_file,
                    }
                    for a in assignments
                ],
                indent=2,
            )
        )
    else:
        print_table(assignments)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
