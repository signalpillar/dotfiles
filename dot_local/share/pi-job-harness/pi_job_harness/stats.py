"""Pure task-mapping stats from execution timestamps."""

from __future__ import annotations

import json
import statistics
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import UTC, datetime
from typing import Any

from pi_job_harness.task import ExecutionRecord, task_slices


def parse_utc(value: str | None) -> datetime | None:
    if not value:
        return None
    text = value.strip()
    if text.endswith("Z"):
        text = text[:-1] + "+00:00"
    try:
        dt = datetime.fromisoformat(text)
    except ValueError:
        return None
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=UTC)
    return dt.astimezone(UTC)


def fmt_dur(seconds: float | None) -> str:
    if seconds is None:
        return "-"
    if seconds < 0:
        seconds = 0.0
    if seconds < 60:
        return f"{seconds:.1f}s"
    minutes = seconds / 60.0
    if minutes < 60:
        return f"{minutes:.1f}m"
    hours = minutes / 60.0
    if hours < 48:
        return f"{hours:.1f}h"
    return f"{hours / 24.0:.1f}d"


def fmt_ts(dt: datetime | None) -> str:
    if dt is None:
        return "-"
    return dt.strftime("%Y-%m-%d %H:%M UTC")


# Step keys that are usually human/PR wait, not model work.
DEFAULT_WAIT_KEYS = frozenset(
    {
        "wait-for-feedback",
        "wait-for-feedback-merge",
        "wait-for-landing",
        "wait-for-merge",
    }
)

INSTANT_SECS = 1.0


@dataclass
class Interval:
    slice_key: str
    step_key: str | None  # None => slice-level execution
    status: str
    model: str
    started: datetime
    ended: datetime | None
    duration_secs: float | None
    kind: str  # step | slice


@dataclass
class Agg:
    count: int = 0
    durations: list[float] = field(default_factory=list)

    def add(self, duration: float | None) -> None:
        self.count += 1
        if duration is not None:
            self.durations.append(duration)

    @property
    def total(self) -> float:
        return sum(self.durations)

    @property
    def mean(self) -> float | None:
        return statistics.fmean(self.durations) if self.durations else None

    @property
    def median(self) -> float | None:
        return statistics.median(self.durations) if self.durations else None


def _interval(
    *,
    slice_key: str,
    step_key: str | None,
    status: str,
    execution: ExecutionRecord | None,
    kind: str,
) -> Interval | None:
    if execution is None:
        return None
    started = parse_utc(execution.started)
    if started is None:
        return None
    ended = parse_utc(execution.ended)
    duration = None
    if ended is not None:
        duration = (ended - started).total_seconds()
    return Interval(
        slice_key=slice_key,
        step_key=step_key,
        status=status,
        model=execution.model or "?",
        started=started,
        ended=ended,
        duration_secs=duration,
        kind=kind,
    )


def collect_intervals(doc: dict[str, Any]) -> list[Interval]:
    out: list[Interval] = []
    for sl in task_slices(doc):
        slice_iv = _interval(
            slice_key=sl.key or "?",
            step_key=None,
            status=sl.status,
            execution=sl.execution,
            kind="slice",
        )
        if slice_iv is not None:
            out.append(slice_iv)
        for step in sl.all_steps:
            step_iv = _interval(
                slice_key=sl.key or "?",
                step_key=step.key or "?",
                status=step.status,
                execution=step.execution,
                kind="step",
            )
            if step_iv is not None:
                out.append(step_iv)
    return out


def status_counts(doc: dict[str, Any]) -> dict[str, dict[str, int]]:
    slices: dict[str, int] = defaultdict(int)
    steps: dict[str, int] = defaultdict(int)
    for sl in task_slices(doc):
        slices[sl.status or "?"] += 1
        for step in sl.all_steps:
            steps[step.status or "?"] += 1
    return {"slices": dict(slices), "steps": dict(steps)}


def is_wait(step_key: str | None, wait_keys: frozenset[str]) -> bool:
    return step_key is not None and step_key in wait_keys


def is_instant(duration: float | None) -> bool:
    return duration is not None and duration < INSTANT_SECS


def build_stats(doc: dict[str, Any], task_label: str, wait_keys: frozenset[str]) -> dict[str, Any]:
    intervals = collect_intervals(doc)
    steps = [i for i in intervals if i.kind == "step"]
    finished = [i for i in steps if i.duration_secs is not None]
    active = [
        i
        for i in finished
        if not is_wait(i.step_key, wait_keys) and not is_instant(i.duration_secs)
    ]
    wait_only = [i for i in finished if is_wait(i.step_key, wait_keys)]
    instant = [i for i in finished if is_instant(i.duration_secs)]

    all_ts = [i.started for i in steps] + [i.ended for i in steps if i.ended]
    first = min(all_ts) if all_ts else None
    last = max(all_ts) if all_ts else None
    calendar_secs = (last - first).total_seconds() if first and last else None

    by_model: dict[str, Agg] = defaultdict(Agg)
    by_model_active: dict[str, Agg] = defaultdict(Agg)
    by_step: dict[str, Agg] = defaultdict(Agg)
    by_step_active: dict[str, Agg] = defaultdict(Agg)

    for i in finished:
        assert i.step_key is not None
        by_model[i.model].add(i.duration_secs)
        by_step[i.step_key].add(i.duration_secs)
    for i in active:
        assert i.step_key is not None
        by_model_active[i.model].add(i.duration_secs)
        by_step_active[i.step_key].add(i.duration_secs)

    # Slice cycle time: min step started -> max step ended within slice (finished steps only).
    slice_cycles: list[dict[str, Any]] = []
    by_slice: dict[str, list[Interval]] = defaultdict(list)
    for i in finished:
        by_slice[i.slice_key].append(i)
    for slice_key, items in by_slice.items():
        starts = [x.started for x in items]
        ends = [x.ended for x in items if x.ended]
        if not starts or not ends:
            continue
        s0, s1 = min(starts), max(ends)
        slice_cycles.append(
            {
                "slice": slice_key,
                "started": s0.isoformat(),
                "ended": s1.isoformat(),
                "duration_secs": (s1 - s0).total_seconds(),
                "steps": len(items),
            }
        )
    slice_cycles.sort(key=lambda x: -x["duration_secs"])

    # Throughput: done steps per UTC week of ended timestamp.
    weekly: dict[str, int] = defaultdict(int)
    for i in steps:
        if i.status != "done" or i.ended is None:
            continue
        iso = i.ended.isocalendar()
        weekly[f"{iso.year}-W{iso.week:02d}"] += 1

    # Velocity: done slices per week (slice status done, using last step end or slice end).
    slice_done_weekly: dict[str, int] = defaultdict(int)
    for sl in task_slices(doc):
        if sl.status != "done":
            continue
        ends = [parsed for stamp in sl.ended_stamps() if (parsed := parse_utc(stamp)) is not None]
        if not ends:
            continue
        ended = max(ends)
        iso = ended.isocalendar()
        slice_done_weekly[f"{iso.year}-W{iso.week:02d}"] += 1

    claims = (doc.get("orchestration") or {}).get("cursors") or []
    source = doc.get("source") or {}
    counts = status_counts(doc)

    # Model frequency: every step with a recorded model (how often), independent of duration.
    model_freq: dict[str, int] = defaultdict(int)
    model_step_freq: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    for i in steps:
        model_freq[i.model] += 1
        if i.step_key:
            model_step_freq[i.model][i.step_key] += 1

    def agg_table(
        mapping: dict[str, Agg],
        *,
        limit: int | None = None,
        sort: str = "count",
    ) -> list[dict[str, Any]]:
        rows = []
        for key, agg in mapping.items():
            rows.append(
                {
                    "key": key,
                    "count": agg.count,
                    "total_secs": agg.total,
                    "mean_secs": agg.mean,
                    "median_secs": agg.median,
                }
            )
        if sort == "median":
            rows.sort(
                key=lambda r: (
                    -(r["median_secs"] if r["median_secs"] is not None else -1),
                    -r["count"],
                    r["key"],
                )
            )
        elif sort == "total":
            rows.sort(key=lambda r: (-r["total_secs"], -r["count"], r["key"]))
        else:
            rows.sort(key=lambda r: (-r["count"], -r["total_secs"], r["key"]))
        if limit is not None:
            rows = rows[:limit]
        return rows

    def freq_table(freq: dict[str, int]) -> list[dict[str, Any]]:
        total_n = sum(freq.values()) or 1
        rows = [
            {
                "key": key,
                "count": n,
                "pct": 100.0 * n / total_n,
            }
            for key, n in freq.items()
        ]
        rows.sort(key=lambda r: (-r["count"], r["key"]))
        return rows

    model_usage = []
    for row in freq_table(model_freq):
        active_agg = by_model_active.get(row["key"])
        raw_agg = by_model.get(row["key"])
        top_steps = sorted(
            model_step_freq[row["key"]].items(),
            key=lambda kv: (-kv[1], kv[0]),
        )[:5]
        model_usage.append(
            {
                **row,
                "median_active_secs": active_agg.median if active_agg else None,
                "active_count": active_agg.count if active_agg else 0,
                "median_raw_secs": raw_agg.median if raw_agg else None,
                "raw_count": raw_agg.count if raw_agg else 0,
                "top_steps": [{"key": k, "count": n} for k, n in top_steps],
            }
        )

    active_median = (
        statistics.median([i.duration_secs for i in active if i.duration_secs is not None])
        if active
        else None
    )

    return {
        "task": task_label,
        "title": doc.get("title"),
        "source_discovered": source.get("discovered"),
        "first_activity": first.isoformat() if first else None,
        "last_activity": last.isoformat() if last else None,
        "calendar_secs": calendar_secs,
        "status_counts": counts,
        "execution": {
            "step_intervals": len(steps),
            "finished_with_duration": len(finished),
            "active_filtered": len(active),
            "wait_intervals": len(wait_only),
            "instant_intervals": len(instant),
            "wall_total_secs": sum(i.duration_secs or 0 for i in finished),
            "wait_total_secs": sum(i.duration_secs or 0 for i in wait_only),
            "instant_total_secs": sum(i.duration_secs or 0 for i in instant),
            "active_total_secs": sum(i.duration_secs or 0 for i in active),
            "active_median_secs": active_median,
            "instant_threshold_secs": INSTANT_SECS,
            "distinct_models": len(model_freq),
        },
        "wait_keys": sorted(wait_keys),
        "models_used": model_usage,
        "by_model_raw": agg_table(by_model, sort="total"),
        "by_model_active": agg_table(by_model_active, sort="count"),
        "by_step_raw": agg_table(by_step, limit=20, sort="total"),
        "by_step_active": agg_table(by_step_active, limit=20, sort="median"),
        "longest_slice_cycles": slice_cycles[:15],
        "throughput_done_steps_by_week": dict(sorted(weekly.items())),
        "velocity_done_slices_by_week": dict(sorted(slice_done_weekly.items())),
        "current_claims": [
            {
                "owner": c.get("owner"),
                "slice": c.get("slice"),
                "claimed_at": c.get("claimed_at"),
                "last_seen": c.get("last_seen"),
            }
            for c in claims
        ],
        "decisions_count": len(doc.get("decisions") or []),
        "caveats": [
            "Durations are wall clock (ended - started), not agent-busy time.",
            "Primary duration metric is median; totals are secondary (skewed by parked waits).",
            "Model 'how often' counts every step execution with a model id.",
            "Owner is only on current claims; historical agent attribution uses model id.",
            "Slice execution intervals are omitted from totals to avoid double-counting steps.",
            f"Active filter drops wait keys {sorted(wait_keys)} and durations < {INSTANT_SECS}s.",
        ],
    }


def render_json(report: dict[str, Any]) -> str:
    return json.dumps(report, indent=2, default=str)


def _fmt_counts(counts: dict[str, int]) -> str:
    order = ["done", "skipped", "in_progress", "planned", "blocked"]
    parts = []
    seen = set()
    for key in order:
        if key in counts:
            parts.append(f"{key}={counts[key]}")
            seen.add(key)
    for key, value in sorted(counts.items()):
        if key not in seen:
            parts.append(f"{key}={value}")
    return ", ".join(parts) if parts else "(none)"


def _ordered_by_median(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return sorted(
        rows,
        key=lambda r: (
            -(r["median_secs"] if r["median_secs"] is not None else -1),
            -r["count"],
            r["key"],
        ),
    )


def render_markdown(report: dict[str, Any]) -> str:
    """Render the stats report as Markdown."""
    ex = report["execution"]
    sc = report["status_counts"]
    lines: list[str] = []

    title = report.get("title") or report["task"]
    lines.append(f"# pi-job stats: {report['task']}")
    lines.append("")
    lines.append(f"**{title}**")
    lines.append("")
    lines.append("Report from TaskStore `execution` fields.")
    lines.append("Primary duration metric is median.")
    lines.append("")

    lines.append("## Timeline")
    lines.append("")
    lines.append("| Field | Value |")
    lines.append("| --- | --- |")
    lines.append(f"| source.discovered | {report.get('source_discovered') or '-'} |")
    lines.append(f"| first activity | {fmt_ts(parse_utc(report.get('first_activity')))} |")
    lines.append(f"| last activity | {fmt_ts(parse_utc(report.get('last_activity')))} |")
    lines.append(f"| calendar span | {fmt_dur(report.get('calendar_secs'))} |")
    lines.append("")

    lines.append("## Status")
    lines.append("")
    lines.append(f"- slices: `{_fmt_counts(sc['slices'])}`")
    lines.append(f"- steps: `{_fmt_counts(sc['steps'])}`")
    lines.append(f"- decisions: {report['decisions_count']}")
    lines.append("")

    lines.append("## Time (step executions only)")
    lines.append("")
    lines.append("| Metric | Value |")
    lines.append("| --- | --- |")
    lines.append(
        f"| intervals finished | {ex['finished_with_duration']} "
        f"(of {ex['step_intervals']} with start) |"
    )
    lines.append(f"| distinct models | {ex['distinct_models']} |")
    lines.append(f"| wall total | {fmt_dur(ex['wall_total_secs'])} |")
    lines.append(
        f"| wait total | {fmt_dur(ex['wait_total_secs'])} "
        f"({ex['wait_intervals']} intervals) |"
    )
    lines.append(
        f"| instant (under {ex['instant_threshold_secs']}s) | "
        f"{fmt_dur(ex['instant_total_secs'])} ({ex['instant_intervals']} intervals) |"
    )
    lines.append(
        f"| active (filtered) | median {fmt_dur(ex['active_median_secs'])} "
        f"/ sum {fmt_dur(ex['active_total_secs'])} "
        f"({ex['active_filtered']} intervals) |"
    )
    lines.append("")

    lines.append("## Models used (how often)")
    lines.append("")
    lines.append(
        "| Model | n | share | median (active) | active n | Top steps |"
    )
    lines.append("| --- | --- | --- | --- | --- | --- |")
    for row in report["models_used"]:
        top = ", ".join(f"`{s['key']}` x {s['count']}" for s in row["top_steps"][:3]) or "-"
        lines.append(
            f"| `{row['key']}` | {row['count']} | {row['pct']:.1f}% | "
            f"{fmt_dur(row['median_active_secs'])} | {row['active_count']} | {top} |"
        )
    lines.append("")
    lines.append(f"_{ex['step_intervals']} step executions with a model id._")
    lines.append("")

    lines.append("## Models - median duration (active filter)")
    lines.append("")
    lines.append("| Model | active n | median | mean | total |")
    lines.append("| --- | --- | --- | --- | --- |")
    for row in _ordered_by_median(report["by_model_active"]):
        lines.append(
            f"| `{row['key']}` | {row['count']} | {fmt_dur(row['median_secs'])} | "
            f"{fmt_dur(row['mean_secs'])} | {fmt_dur(row['total_secs'])} |"
        )
    lines.append("")

    lines.append("## Step kinds - by median duration (active filter, top 20)")
    lines.append("")
    lines.append("| Step | n | median | mean | total |")
    lines.append("| --- | --- | --- | --- | --- |")
    for row in _ordered_by_median(report["by_step_active"]):
        lines.append(
            f"| `{row['key']}` | {row['count']} | {fmt_dur(row['median_secs'])} | "
            f"{fmt_dur(row['mean_secs'])} | {fmt_dur(row['total_secs'])} |"
        )
    lines.append("")

    lines.append("## Velocity - done slices / ISO week")
    lines.append("")
    lines.append("| Week | done slices |")
    lines.append("| --- | --- |")
    for week, n in report["velocity_done_slices_by_week"].items():
        lines.append(f"| {week} | {n} |")
    if not report["velocity_done_slices_by_week"]:
        lines.append("| - | 0 |")
    lines.append("")

    lines.append("## Throughput - done steps / ISO week")
    lines.append("")
    lines.append("| Week | done steps |")
    lines.append("| --- | --- |")
    for week, n in report["throughput_done_steps_by_week"].items():
        lines.append(f"| {week} | {n} |")
    if not report["throughput_done_steps_by_week"]:
        lines.append("| - | 0 |")
    lines.append("")

    lines.append("## Longest slice cycles")
    lines.append("")
    lines.append("First step start -> last step end.")
    lines.append("")
    lines.append("| Duration | Steps | Slice |")
    lines.append("| --- | --- | --- |")
    for row in report["longest_slice_cycles"][:10]:
        lines.append(
            f"| {fmt_dur(row['duration_secs'])} | {row['steps']} | `{row['slice']}` |"
        )
    lines.append("")

    if report["current_claims"]:
        lines.append("## Current claims")
        lines.append("")
        lines.append("Not historical ownership.")
        lines.append("")
        lines.append("| Owner | Slice | last_seen |")
        lines.append("| --- | --- | --- |")
        for c in report["current_claims"]:
            lines.append(
                f"| `{c['owner']}` | `{c['slice']}` | `{c['last_seen']}` |"
            )
        lines.append("")

    lines.append("## Caveats")
    lines.append("")
    for line in report["caveats"]:
        lines.append(f"- {line}")
    lines.append("")

    return "\n".join(lines)


