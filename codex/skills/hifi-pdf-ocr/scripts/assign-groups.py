#!/usr/bin/env python3
"""Assign logical page groups to agents with approximate load balancing."""

from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Group:
    group: str
    start: int
    end: int
    pages: int
    target_tex: str


def load_groups(path: Path) -> list[Group]:
    with path.open("r", newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        required = {"group", "start", "end"}
        missing = required - set(reader.fieldnames or [])
        if missing:
            raise SystemExit(f"Missing required columns: {', '.join(sorted(missing))}")

        groups: list[Group] = []
        for row in reader:
            group = (row.get("group") or "").strip()
            if not group:
                continue
            start = int((row.get("start") or "").strip())
            end = int((row.get("end") or "").strip())
            if start < 1 or end < start:
                raise SystemExit(f"Invalid range for group '{group}': {start}-{end}")
            target_tex = (row.get("target_tex") or "").strip() or f"{group}.tex"
            pages = end - start + 1
            groups.append(Group(group=group, start=start, end=end, pages=pages, target_tex=target_tex))

    if not groups:
        raise SystemExit("No groups found in input file.")

    verify_no_overlap(groups)
    return groups


def verify_no_overlap(groups: list[Group]) -> None:
    ordered = sorted(groups, key=lambda g: (g.start, g.end, g.group))
    prev = ordered[0]
    for curr in ordered[1:]:
        if curr.start <= prev.end:
            raise SystemExit(
                "Overlapping groups detected: "
                f"{prev.group} ({prev.start}-{prev.end}) and {curr.group} ({curr.start}-{curr.end})"
            )
        prev = curr


def assign(groups: list[Group], agents: int) -> tuple[list[list[Group]], list[int]]:
    buckets: list[list[Group]] = [[] for _ in range(agents)]
    loads = [0 for _ in range(agents)]

    for group in sorted(groups, key=lambda g: (-g.pages, g.start, g.group)):
        idx = min(range(agents), key=lambda i: loads[i])
        buckets[idx].append(group)
        loads[idx] += group.pages

    for bucket in buckets:
        bucket.sort(key=lambda g: g.start)

    return buckets, loads


def write_csv(path: Path, assignments: list[list[Group]], agent_prefix: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["agent", "group", "start", "end", "pages", "target_tex"])
        for idx, groups in enumerate(assignments, start=1):
            agent = f"{agent_prefix}-{idx}"
            for group in groups:
                writer.writerow([agent, group.group, group.start, group.end, group.pages, group.target_tex])


def print_summary(assignments: list[list[Group]], loads: list[int], agent_prefix: str) -> None:
    print("agent,total_pages,groups")  # noqa: print
    for idx, groups in enumerate(assignments, start=1):
        agent = f"{agent_prefix}-{idx}"
        label = " | ".join(f"{g.group}({g.start}-{g.end})" for g in groups) if groups else "-"
        print(f"{agent},{loads[idx - 1]},{label}")  # noqa: print


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--groups", type=Path, required=True, help="CSV path with group,start,end,target_tex columns.")
    parser.add_argument("--agents", type=int, required=True, help="Number of agents.")
    parser.add_argument("--agent-prefix", default="agent", help="Prefix for agent IDs.")
    parser.add_argument("--out", type=Path, help="Optional CSV output path.")
    parser.add_argument("--json", action="store_true", help="Print JSON instead of summary table.")
    args = parser.parse_args()

    if args.agents < 1:
        raise SystemExit("agents must be >= 1")

    groups = load_groups(args.groups)
    assignments, loads = assign(groups, args.agents)

    if args.out:
        write_csv(args.out, assignments, args.agent_prefix)

    if args.json:
        payload = []
        for idx, bucket in enumerate(assignments, start=1):
            payload.append(
                {
                    "agent": f"{args.agent_prefix}-{idx}",
                    "total_pages": loads[idx - 1],
                    "groups": [
                        {
                            "group": g.group,
                            "start": g.start,
                            "end": g.end,
                            "pages": g.pages,
                            "target_tex": g.target_tex,
                        }
                        for g in bucket
                    ],
                }
            )
        print(json.dumps(payload, indent=2))  # noqa: print
    else:
        print_summary(assignments, loads, args.agent_prefix)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
