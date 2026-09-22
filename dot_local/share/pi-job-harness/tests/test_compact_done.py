"""Unit tests for compact-done slice compaction."""

from __future__ import annotations

import os
import tempfile
from pathlib import Path

import yaml

from pi_job_harness.report import build_report, parse_since
from pi_job_harness.stats import build_stats
from pi_job_harness.store.yaml import YamlTaskLayout, YamlTaskStore

_TEST_XDG = tempfile.mkdtemp(prefix="pi-job-compact-test-xdg-")
os.environ["XDG_CONFIG_HOME"] = _TEST_XDG
os.environ.pop("PI_JOB_PROFILE_OVERLAY", None)

from pi_job_harness.layout import PiJobLayout


def _step(key: str, status: str, note: str, started: str, ended: str | None) -> dict:
    execution: dict = {"model": "cursor/test", "started": started}
    if ended is not None:
        execution["ended"] = ended
    return {"key": key, "title": key, "status": status, "note": note, "execution": execution}


def _slice(key: str, status: str, note: str, ended: str | None) -> dict:
    return {
        "key": key,
        "kind": "implement",
        "title": key.replace("-", " "),
        "goal": "goal for " + key,
        "status": status,
        "note": note,
        "steps": [_step("edit-code", status, "step evidence " + key, "2026-08-01T10:00:00Z", ended)],
        "final_steps": [],
        "repo_work": {"demo": {"prs": [{"url": "https://example.test/pr/1", "status": "merged", "note": ""}], "worktree": None}},
    }


def _task_doc() -> dict:
    return {
        "title": "t",
        "status": "in_progress",
        "plan": {
            "slices": [
                _slice("done-old", "done", "long slice note", "2026-08-01T10:05:00Z"),
                _slice("done-new", "done", "another note", "2026-09-01T10:05:00Z"),
                _slice("planned", "planned", "live note", None),
            ]
        },
    }


def _store(tmp_path: Path, task: dict) -> YamlTaskStore:
    path = tmp_path / "task.yaml"
    path.write_text(yaml.safe_dump(task, sort_keys=False), encoding="utf-8")
    return YamlTaskStore(YamlTaskLayout(path), PiJobLayout.from_environ())


def test_compact_clears_done_notes_only(tmp_path: Path) -> None:
    store = _store(tmp_path, _task_doc())
    result = store.compact_done_slices()
    assert result == {"slices": 2, "notes": 4}
    task = store.read()
    by_key = {s["key"]: s for s in task["plan"]["slices"]}
    assert by_key["done-old"]["note"] == ""
    assert by_key["done-old"]["steps"][0]["note"] == ""
    assert by_key["planned"]["note"] == "live note"
    assert by_key["done-old"]["goal"] == "goal for done-old"
    assert by_key["done-old"]["repo_work"]["demo"]["prs"][0]["status"] == "merged"
    assert by_key["done-old"]["steps"][0]["execution"]["ended"] == "2026-08-01T10:05:00Z"


def test_compact_since_filters_by_ended_date(tmp_path: Path) -> None:
    store = _store(tmp_path, _task_doc())
    result = store.compact_done_slices(since="2026-09-01")
    assert result == {"slices": 1, "notes": 2}
    task = store.read()
    by_key = {s["key"]: s for s in task["plan"]["slices"]}
    assert by_key["done-old"]["note"] == "long slice note"
    assert by_key["done-new"]["note"] == ""


def test_compact_preserves_stats_and_report(tmp_path: Path) -> None:
    store = _store(tmp_path, _task_doc())
    before_task = store.read()
    stats_before = build_stats(before_task, "t", frozenset({"wait-for-feedback"}))
    report_before = build_report(before_task, parse_since("2026-01-01"))
    store.compact_done_slices()
    after_task = store.read()
    assert build_stats(after_task, "t", frozenset({"wait-for-feedback"})) == stats_before
    assert build_report(after_task, parse_since("2026-01-01")) == report_before


def test_compact_bad_since_rejected(tmp_path: Path) -> None:
    store = _store(tmp_path, _task_doc())
    try:
        store.compact_done_slices(since="not-a-date")
    except ValueError:
        return
    raise AssertionError("expected ValueError for bad --since")


if __name__ == "__main__":
    import sys

    d = Path(tempfile.mkdtemp(prefix="pi-job-compact-manual-"))
    test_compact_clears_done_notes_only(d)
    test_compact_since_filters_by_ended_date(d)
    test_compact_preserves_stats_and_report(d)
    test_compact_bad_since_rejected(d)
    print("ok")
    sys.exit(0)
