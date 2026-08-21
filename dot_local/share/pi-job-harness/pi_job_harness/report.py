"""Pure since-date shipped-slice report from a task mapping."""

from __future__ import annotations

import json
from collections.abc import Mapping
from datetime import UTC, date, datetime
from typing import Any

from pi_job_harness.stats import parse_utc
from pi_job_harness.task import TaskSlice, task_slices


def parse_since(value: str) -> date:
    """Parse YYYY-MM-DD as a UTC calendar date. Raise ValueError on bad input."""
    text = (value or "").strip()
    try:
        return date.fromisoformat(text)
    except ValueError as exc:
        raise ValueError(f"--since must be YYYY-MM-DD, got {value!r}") from exc


def _step_ended(sl: TaskSlice) -> datetime | None:
    ends = [parsed for stamp in sl.ended_stamps() if (parsed := parse_utc(stamp)) is not None]
    return max(ends) if ends else None


def _prs(sl: TaskSlice) -> list[dict[str, str]]:
    rows: list[dict[str, str]] = []
    repo_work = sl.repo_work or {}
    if not isinstance(repo_work, Mapping):
        return rows
    for work in repo_work.values():
        if not isinstance(work, Mapping):
            continue
        for pr in work.get("prs") or []:
            if not isinstance(pr, Mapping):
                continue
            url = str(pr.get("url") or "")
            status = str(pr.get("status") or "")
            if url or status:
                rows.append({"url": url, "status": status})
    return rows


def build_report(task: dict[str, Any], since: date) -> list[dict[str, Any]]:
    """Return done slices whose ended date is on or after `since` (UTC inclusive)."""
    rows: list[dict[str, Any]] = []
    for sl in task_slices(task):
        if sl.status != "done":
            continue
        ended = _step_ended(sl)
        if ended is None:
            continue
        ended_utc = ended.astimezone(UTC)
        if ended_utc.date() < since:
            continue
        rows.append(
            {
                "slice": sl.key or "?",
                "title": sl.title,
                "status": "done",
                "ended": ended_utc.isoformat().replace("+00:00", "Z"),
                "prs": _prs(sl),
            }
        )
    rows.sort(key=lambda row: (row["ended"], row["slice"]))
    return rows


def render_json(rows: list[dict[str, Any]]) -> str:
    return json.dumps(rows, indent=2)


def render_markdown(task_label: str, since: date, rows: list[dict[str, Any]]) -> str:
    lines = [
        f"# pi-job report: {task_label}",
        "",
        f"Done slices with `ended` on or after `{since.isoformat()}` (UTC, inclusive).",
        "PR urls come from recorded `repo_work`.",
        "",
        "| Slice | Title | Ended | PRs |",
        "| --- | --- | --- | --- |",
    ]
    if not rows:
        lines.append("| - | none | - | - |")
    for row in rows:
        prs = row["prs"]
        if prs:
            pr_cell = ", ".join(f"{p['url']} ({p['status']})" for p in prs)
        else:
            pr_cell = "-"
        title = str(row["title"]).replace("|", "\\|")
        lines.append(
            f"| `{row['slice']}` | {title} | {row['ended']} | {pr_cell} |"
        )
    lines.append("")
    return "\n".join(lines)
