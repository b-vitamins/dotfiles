#!/usr/bin/env python3
"""Shared Nsight Compute parsing helpers."""

from __future__ import annotations

import re
from typing import Iterable

NUM_RE = r"([+-]?(?:\d[\d,]*)(?:\.\d+)?(?:[eE][+-]?\d+)?)"
DEFAULT_SECTIONS = [
    "MemoryWorkloadAnalysis_Tables",
    "LaunchStats",
    "SchedulerStats",
    "WarpStateStats",
    "InstructionStats",
    "SpeedOfLight",
]


def _find_match(patterns: Iterable[str], text: str):
    for pattern in patterns:
        match = re.search(pattern, text, flags=re.MULTILINE)
        if match:
            return match
    return None


def _parse_float(text: str) -> float:
    return float(text.replace(",", ""))


def _parse_metric_value(text: str, metric_name: str):
    pattern = rf"^\s*{re.escape(metric_name)}\s+(?:\S+\s+)?{NUM_RE}\s*$"
    match = re.search(pattern, text, flags=re.MULTILINE)
    if not match:
        return None
    return _parse_float(match.group(1))


def _parse_unit_value_table(text: str, metric_name: str, unit_to_gbs: bool = False):
    pattern = rf"^\s*{re.escape(metric_name)}\s+(\S+)\s+{NUM_RE}\s*$"
    match = re.search(pattern, text, flags=re.MULTILINE)
    if not match:
        return None
    unit = match.group(1).lower()
    value = _parse_float(match.group(2))
    if not unit_to_gbs:
        return value
    unit = unit.replace("bytes", "byte")
    unit = unit.replace("gbyte", "gb")
    unit = unit.replace("mbyte", "mb")
    unit = unit.replace("kbyte", "kb")
    unit = unit.replace("byte", "b")
    if unit in ("gb/s", "gbs", "gbps"):
        return value
    if unit in ("mb/s", "mbs", "mbps"):
        return value / 1024.0
    if unit in ("kb/s", "kbs", "kbps"):
        return value / (1024.0 * 1024.0)
    if unit in ("b/s", "bs", "bps"):
        return value / (1024.0 * 1024.0 * 1024.0)
    return value


def _parse_percent(text: str, patterns: Iterable[str]):
    match = _find_match(patterns, text)
    if not match:
        return None
    return _parse_float(match.group(1))


def _parse_unit_value(text: str, patterns: Iterable[str], unit_to_gbs: bool = False):
    match = _find_match(patterns, text)
    if not match:
        return None
    value = _parse_float(match.group(1))
    unit = match.group(2).lower() if match.lastindex and match.lastindex >= 2 and match.group(2) else ""
    if not unit_to_gbs:
        return value
    if unit in ("", "gb/s", "gbyte/s", "gbytes/s"):
        return value
    if unit in ("mb/s", "mbyte/s", "mbytes/s"):
        return value / 1024.0
    if unit in ("kb/s", "kbyte/s", "kbytes/s"):
        return value / (1024.0 * 1024.0)
    if unit in ("b/s", "bytes/s"):
        return value / (1024.0 * 1024.0 * 1024.0)
    return value


def _parse_size_kib(text: str, patterns: Iterable[str]):
    match = _find_match(patterns, text)
    if not match:
        return None
    value = _parse_float(match.group(1))
    unit = match.group(2).lower() if match.lastindex and match.lastindex >= 2 and match.group(2) else ""
    if unit in ("", "kib", "kb"):
        return value
    if unit in ("mib", "mb"):
        return value * 1024.0
    if unit in ("bytes", "byte", "b"):
        return value / 1024.0
    return value


def _parse_size_kib_table(text: str, metric_name: str):
    pattern = rf"^\s*{re.escape(metric_name)}\s+(\S+)\s+{NUM_RE}\s*$"
    match = re.search(pattern, text, flags=re.MULTILINE)
    if not match:
        return None
    unit = match.group(1).lower().replace("/block", "")
    value = _parse_float(match.group(2))
    if unit in ("kib", "kb", "kbyte"):
        return value
    if unit in ("mib", "mb", "mbyte"):
        return value * 1024.0
    if unit in ("bytes", "byte", "b"):
        return value / 1024.0
    return value


def _extract_advisories(text: str) -> list[dict[str, str]]:
    advisories: list[dict[str, str]] = []
    lines = text.splitlines()
    idx = 0
    while idx < len(lines):
        match = re.match(r"^\s*(OPT|WRN|INF)\s+(.*\S)\s*$", lines[idx])
        if not match:
            idx += 1
            continue
        level = match.group(1)
        parts = [re.sub(r"\s+", " ", match.group(2).strip())]
        idx += 1
        while idx < len(lines):
            line = lines[idx]
            if re.match(r"^\s*(OPT|WRN|INF)\s+", line):
                break
            stripped = line.strip()
            if not stripped:
                idx += 1
                continue
            if line.startswith("    ") or line.startswith("\t"):
                parts.append(re.sub(r"\s+", " ", stripped))
                idx += 1
                continue
            break
        advisories.append({"level": level, "message": " ".join(parts)})
    return advisories


def parse_ncu_outputs(outputs: dict[str, str]) -> dict[str, object]:
    mem_out = outputs.get("MemoryWorkloadAnalysis_Tables", "")
    launch_out = outputs.get("LaunchStats", "")
    sched_out = outputs.get("SchedulerStats", "")
    warp_out = outputs.get("WarpStateStats", "")
    inst_out = outputs.get("InstructionStats", "")
    sol_out = outputs.get("SpeedOfLight", "")

    dram_pct = _parse_percent(
        mem_out,
        [
            rf"dram__throughput(?:\.avg)?\.pct_of_peak_sustained_elapsed\s+{NUM_RE}",
            rf"DRAM Throughput[^\n]*?{NUM_RE}\s*%",
        ],
    )
    if dram_pct is None:
        dram_pct = _parse_metric_value(mem_out, "DRAM Throughput")
    dram_read = _parse_unit_value(
        mem_out,
        [
            rf"dram__bytes_read\.sum\.per_second\s+{NUM_RE}\s*([A-Za-z/]+)?",
            rf"Requested Global Load Throughput[^\n]*?{NUM_RE}\s*([A-Za-z/]+)?",
        ],
        unit_to_gbs=True,
    )
    if dram_read is None:
        dram_read = _parse_unit_value_table(
            mem_out, "dram__bytes_read.sum.per_second", unit_to_gbs=True
        )
    dram_write = _parse_unit_value(
        mem_out,
        [
            rf"dram__bytes_write\.sum\.per_second\s+{NUM_RE}\s*([A-Za-z/]+)?",
            rf"Requested Global Store Throughput[^\n]*?{NUM_RE}\s*([A-Za-z/]+)?",
        ],
        unit_to_gbs=True,
    )
    if dram_write is None:
        dram_write = _parse_unit_value_table(
            mem_out, "dram__bytes_write.sum.per_second", unit_to_gbs=True
        )
    sh_pct = _parse_percent(
        mem_out,
        [
            rf"l1tex__data_pipe_lsu_wavefronts_mem_shared\.sum\.pct_of_peak_sustained_elapsed\s+{NUM_RE}",
            rf"Shared Memory[^\n]*?{NUM_RE}\s*%",
        ],
    )
    if sh_pct is None:
        sh_pct = _parse_metric_value(
            mem_out,
            "l1tex__data_pipe_lsu_wavefronts_mem_shared.sum.pct_of_peak_sustained_elapsed",
        )
    bank_ld = _parse_unit_value(
        mem_out,
        [rf"l1tex__data_bank_conflicts_pipe_lsu_mem_shared_op_ld\.sum\s+{NUM_RE}"],
    )
    if bank_ld is None:
        bank_ld = _parse_metric_value(
            mem_out, "l1tex__data_bank_conflicts_pipe_lsu_mem_shared_op_ld.sum"
        )
    bank_st = _parse_unit_value(
        mem_out,
        [rf"l1tex__data_bank_conflicts_pipe_lsu_mem_shared_op_st\.sum\s+{NUM_RE}"],
    )
    if bank_st is None:
        bank_st = _parse_metric_value(
            mem_out, "l1tex__data_bank_conflicts_pipe_lsu_mem_shared_op_st.sum"
        )

    regs = _parse_unit_value(
        launch_out,
        [rf"launch__registers_per_thread\s+{NUM_RE}"],
    )
    if regs is None:
        regs = _parse_metric_value(launch_out, "Registers Per Thread")
    smem_kib = _parse_size_kib(
        launch_out,
        [rf"launch__shared_mem_per_block\s+{NUM_RE}\s*([A-Za-z]+)?"],
    )
    if smem_kib is None:
        smem_kib = _parse_size_kib_table(launch_out, "Dynamic Shared Memory Per Block")
    theo_occ = _parse_percent(
        launch_out,
        [
            rf"Theoretical Occupancy[^\n]*?{NUM_RE}\s*%",
            rf"launch__occupancy_limit_active_warps\s+{NUM_RE}",
        ],
    )
    if theo_occ is None:
        theo_occ = _parse_metric_value(launch_out, "Theoretical Occupancy")
    ach_occ = _parse_percent(
        launch_out,
        [
            rf"Achieved Occupancy[^\n]*?{NUM_RE}\s*%",
            rf"sm__warps_active\.avg\.pct_of_peak_sustained_active\s+{NUM_RE}",
        ],
    )
    if ach_occ is None:
        ach_occ = _parse_metric_value(launch_out, "Achieved Occupancy")
    grid_blocks = _parse_metric_value(launch_out, "Grid Size")
    sm_count = _parse_metric_value(launch_out, "# SMs")
    waves_per_sm = _parse_metric_value(launch_out, "Waves Per SM")

    no_eligible = _parse_metric_value(sched_out, "No Eligible")
    eligible_warps = _parse_metric_value(sched_out, "Eligible Warps Per Scheduler")
    active_warps = _parse_metric_value(sched_out, "Active Warps Per Scheduler")
    issued_warps = _parse_metric_value(sched_out, "Issued Warp Per Scheduler")

    active_threads = _parse_metric_value(warp_out, "Avg. Active Threads Per Warp")
    not_pred_threads = _parse_metric_value(
        warp_out, "Avg. Not Predicated Off Threads Per Warp"
    )
    stall_long_sb = _parse_metric_value(warp_out, "Stall Long Scoreboard")
    stall_short_sb = _parse_metric_value(warp_out, "Stall Short Scoreboard")
    stall_barrier = _parse_metric_value(warp_out, "Stall Barrier")
    stall_wait = _parse_metric_value(warp_out, "Stall Wait")

    tensor_pct = _parse_percent(
        sol_out,
        [
            rf"sm__pipe_tensor_cycles_active\.avg\.pct_of_peak_sustained_active\s+{NUM_RE}",
            rf"sm__pipe_tensor_active\.avg\.pct_of_peak_sustained_active\s+{NUM_RE}",
            rf"SM:\s*Pipe Tensor Cycles Active[^\n]*?{NUM_RE}\s*%",
        ],
    )
    if tensor_pct is None:
        tensor_pct = _parse_metric_value(sol_out, "SM: Pipe Tensor Cycles Active")
    compute_pct = _parse_percent(
        sol_out,
        [
            rf"Compute \(SM\) Throughput[^\n]*?{NUM_RE}\s*%",
            rf"Compute \(SM\)\s*\[%\][^\n]*?{NUM_RE}",
        ],
    )
    if compute_pct is None:
        compute_pct = _parse_metric_value(sol_out, "Compute (SM) Throughput")

    inst_executed = _parse_metric_value(inst_out, "Instructions Executed")
    if inst_executed is None:
        inst_executed = _parse_metric_value(inst_out, "Executed Instructions")

    total_dram = None
    if dram_read is not None and dram_write is not None:
        total_dram = dram_read + dram_write
    bank_total = None
    if bank_ld is not None or bank_st is not None:
        bank_total = int((bank_ld or 0.0) + (bank_st or 0.0))
    underfilled_grid = None
    if grid_blocks is not None or sm_count is not None or waves_per_sm is not None:
        launch_limited = False
        if grid_blocks is not None and sm_count is not None:
            launch_limited = launch_limited or bool(grid_blocks < sm_count)
        if waves_per_sm is not None:
            launch_limited = launch_limited or bool(waves_per_sm < 1.0)
        underfilled_grid = launch_limited

    advisories: list[dict[str, str]] = []
    for section, text in outputs.items():
        for advisory in _extract_advisories(text):
            advisories.append({"section": section, **advisory})

    return {
        "dram_pct": dram_pct,
        "dram_read_gbs": dram_read,
        "dram_write_gbs": dram_write,
        "dram_total_gbs": total_dram,
        "shared_pct": sh_pct,
        "bank_conflicts_total": bank_total,
        "registers_per_thread": int(regs) if regs is not None else None,
        "dynamic_smem_kib": smem_kib,
        "theoretical_occupancy_pct": theo_occ,
        "achieved_occupancy_pct": ach_occ,
        "grid_blocks": int(grid_blocks) if grid_blocks is not None else None,
        "sm_count": int(sm_count) if sm_count is not None else None,
        "waves_per_sm": waves_per_sm,
        "underfilled_grid": underfilled_grid,
        "no_eligible_pct": no_eligible,
        "eligible_warps_per_scheduler": eligible_warps,
        "active_warps_per_scheduler": active_warps,
        "issued_warps_per_scheduler": issued_warps,
        "active_threads_per_warp": active_threads,
        "not_predicated_threads_per_warp": not_pred_threads,
        "stall_long_scoreboard": stall_long_sb,
        "stall_short_scoreboard": stall_short_sb,
        "stall_barrier": stall_barrier,
        "stall_wait": stall_wait,
        "tensor_pipe_pct": tensor_pct,
        "compute_pct": compute_pct,
        "instructions_executed": inst_executed,
        "advisories": advisories,
    }


def format_summary(summary: dict[str, object]) -> str:
    def fmt_pct(value):
        return "N/A" if value is None else f"{value:.2f}%"

    def fmt_num(value):
        return "N/A" if value is None else f"{value:.2f}"

    def fmt_gbs(value):
        return "N/A" if value is None else f"{value:.2f} GB/s"

    def fmt_kib(value):
        return "N/A" if value is None else f"{value:.2f} KiB"

    def fmt_int(value):
        return "N/A" if value is None else f"{int(value):,}"

    def fmt_bool(value):
        if value is None:
            return "N/A"
        return "yes" if value else "no"

    def clip(text: str, limit: int = 180) -> str:
        if len(text) <= limit:
            return text
        return text[: limit - 3].rstrip() + "..."

    lines = [
        "DRAM",
        f"- Throughput pct of peak: {fmt_pct(summary['dram_pct'])}",
        f"- Read BW: {fmt_gbs(summary['dram_read_gbs'])}",
        f"- Write BW: {fmt_gbs(summary['dram_write_gbs'])}",
        f"- Total DRAM BW: {fmt_gbs(summary['dram_total_gbs'])}",
        "",
        "Shared Memory / Launch",
        f"- Shared-memory throughput pct of peak: {fmt_pct(summary['shared_pct'])}",
        f"- Bank conflicts total: {fmt_int(summary['bank_conflicts_total'])}",
        f"- Registers per thread: {fmt_int(summary['registers_per_thread'])}",
        f"- Dynamic shared memory per block: {fmt_kib(summary['dynamic_smem_kib'])}",
        f"- Theoretical occupancy: {fmt_pct(summary['theoretical_occupancy_pct'])}",
        f"- Achieved occupancy: {fmt_pct(summary['achieved_occupancy_pct'])}",
        f"- Grid blocks: {fmt_int(summary['grid_blocks'])}",
        f"- SM count: {fmt_int(summary['sm_count'])}",
        f"- Waves per SM: {fmt_num(summary['waves_per_sm'])}",
        f"- Underfilled grid: {fmt_bool(summary['underfilled_grid'])}",
        "",
        "Scheduler / Warp State",
        f"- No Eligible: {fmt_pct(summary['no_eligible_pct'])}",
        f"- Eligible warps / scheduler: {fmt_num(summary['eligible_warps_per_scheduler'])}",
        f"- Active warps / scheduler: {fmt_num(summary['active_warps_per_scheduler'])}",
        f"- Issued warps / scheduler: {fmt_num(summary['issued_warps_per_scheduler'])}",
        f"- Active threads / warp: {fmt_num(summary['active_threads_per_warp'])}",
        f"- Not predicated threads / warp: {fmt_num(summary['not_predicated_threads_per_warp'])}",
        f"- Stall Long Scoreboard: {fmt_num(summary['stall_long_scoreboard'])}",
        f"- Stall Short Scoreboard: {fmt_num(summary['stall_short_scoreboard'])}",
        f"- Stall Barrier: {fmt_num(summary['stall_barrier'])}",
        f"- Stall Wait: {fmt_num(summary['stall_wait'])}",
        "",
        "Instruction Mix",
        f"- Compute throughput pct of peak: {fmt_pct(summary['compute_pct'])}",
        f"- Tensor pipe active pct: {fmt_pct(summary['tensor_pipe_pct'])}",
        f"- Instructions executed: {fmt_int(summary['instructions_executed'])}",
        "",
    ]
    advisories = summary.get("advisories") or []
    if advisories:
        lines.append("Advisories")
        for advisory in advisories[:6]:
            lines.append(
                f"- [{advisory['level']}] {advisory['section']}: {clip(advisory['message'])}"
            )
        lines.append("")
    return "\n".join(lines)
