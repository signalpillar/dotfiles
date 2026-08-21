"""Unit tests for pi_job_harness stats and report transforms."""

from __future__ import annotations

import json
import tempfile
from pathlib import Path

from pi_job_harness.emit import emit_output
from pi_job_harness.report import build_report, parse_since
from pi_job_harness.stats import DEFAULT_WAIT_KEYS, build_stats
from pi_job_harness.store import YamlTaskStore, open_task_store
from pi_job_harness.task import task_slices


def _slice(
    key: str,
    status: str,
    *,
    steps: list[dict] | None = None,
    execution: dict | None = None,
    prs: list[dict] | None = None,
) -> dict:
    entry = {
        "key": key,
        "kind": "implement",
        "title": key.replace("-", " "),
        "goal": "g",
        "status": status,
        "note": "",
        "steps": steps or [],
        "final_steps": [],
    }
    if execution:
        entry["execution"] = execution
    if prs is not None:
        entry["repo_work"] = {"demo": {"prs": prs, "worktree": None}}
    return entry


def _step(key: str, status: str, started: str, ended: str | None, model: str = "cursor/test") -> dict:
    execution: dict = {"model": model, "started": started}
    if ended is not None:
        execution["ended"] = ended
    return {"key": key, "title": key, "status": status, "note": "", "execution": execution}


def test_stats_filters_wait_and_instant() -> None:
    task = {
        "title": "t",
        "plan": {
            "slices": [
                _slice(
                    "one",
                    "done",
                    steps=[
                        _step(
                            "edit-code",
                            "done",
                            "2026-08-01T10:00:00Z",
                            "2026-08-01T10:05:00Z",
                        ),
                        _step(
                            "wait-for-feedback",
                            "done",
                            "2026-08-01T10:05:00Z",
                            "2026-08-02T10:05:00Z",
                        ),
                        _step(
                            "pi-job-feedback",
                            "done",
                            "2026-08-02T10:05:00Z",
                            "2026-08-02T10:05:00.2Z",
                        ),
                    ],
                )
            ]
        },
        "orchestration": {"cursors": []},
        "source": {},
        "decisions": [],
    }
    report = build_stats(task, "slug", DEFAULT_WAIT_KEYS)
    assert report["status_counts"]["slices"]["done"] == 1
    assert report["execution"]["wait_intervals"] == 1
    assert report["execution"]["instant_intervals"] == 1
    assert report["execution"]["active_filtered"] == 1
    assert report["by_step_active"][0]["key"] == "edit-code"


def test_stats_counts_skipped() -> None:
    task = {
        "plan": {
            "slices": [
                _slice("a", "skipped", steps=[_step("grill", "skipped", "2026-08-01T00:00:00Z", "2026-08-01T00:00:01Z")]),
                _slice("b", "done", steps=[_step("edit-code", "done", "2026-08-01T00:00:00Z", "2026-08-01T00:02:00Z")]),
            ]
        },
        "orchestration": {"cursors": []},
        "source": {},
        "decisions": [],
    }
    report = build_stats(task, "slug", DEFAULT_WAIT_KEYS)
    assert report["status_counts"]["slices"]["skipped"] == 1
    assert report["status_counts"]["slices"]["done"] == 1
    assert report["status_counts"]["steps"]["skipped"] == 1


def test_report_since_done_only() -> None:
    task = {
        "plan": {
            "slices": [
                _slice(
                    "shipped",
                    "done",
                    steps=[_step("edit-code", "done", "2026-08-10T00:00:00Z", "2026-08-12T12:00:00Z")],
                    prs=[{"url": "https://example.test/pr/1", "status": "merged", "note": ""}],
                ),
                _slice(
                    "old",
                    "done",
                    steps=[_step("edit-code", "done", "2026-07-01T00:00:00Z", "2026-07-02T00:00:00Z")],
                ),
                _slice(
                    "skipped",
                    "skipped",
                    steps=[_step("edit-code", "skipped", "2026-08-12T00:00:00Z", "2026-08-12T01:00:00Z")],
                ),
                _slice("no-end", "done", steps=[{"key": "x", "title": "x", "status": "done", "note": ""}]),
            ]
        }
    }
    rows = build_report(task, parse_since("2026-08-01"))
    assert [r["slice"] for r in rows] == ["shipped"]
    assert rows[0]["prs"] == [{"url": "https://example.test/pr/1", "status": "merged"}]


def test_parse_since_rejects_bad_date() -> None:
    try:
        parse_since("2026/08/01")
    except ValueError as exc:
        assert "YYYY-MM-DD" in str(exc)
    else:
        raise AssertionError("expected ValueError")


def test_emit_out_writes_file() -> None:
    target = Path(tempfile.mkdtemp()) / "out.md"
    emit_output("# hi\n", str(target))
    assert target.read_text(encoding="utf-8") == "# hi\n"


def test_stats_json_roundtrip() -> None:
    task = {
        "plan": {"slices": [_slice("a", "planned")]},
        "orchestration": {"cursors": []},
        "source": {},
        "decisions": [],
    }
    payload = build_stats(task, "slug", DEFAULT_WAIT_KEYS)
    json.dumps(payload, default=str)
    assert payload["task"] == "slug"


def test_task_slices_ended_stamps_prefer_steps() -> None:
    task = {
        "plan": {
            "slices": [
                _slice(
                    "one",
                    "done",
                    execution={
                        "model": "m",
                        "started": "2026-01-01T00:00:00Z",
                        "ended": "2026-01-01T01:00:00Z",
                    },
                    steps=[
                        _step(
                            "edit-code",
                            "done",
                            "2026-08-01T10:00:00Z",
                            "2026-08-01T10:05:00Z",
                        ),
                    ],
                )
            ]
        }
    }
    slices = task_slices(task)
    assert len(slices) == 1
    assert slices[0].ended_stamps() == ("2026-08-01T10:05:00Z",)


def test_store_package_exports_backends() -> None:
    assert open_task_store is not None
    assert YamlTaskStore.__name__ == "YamlTaskStore"


if __name__ == "__main__":
    test_stats_filters_wait_and_instant()
    test_stats_counts_skipped()
    test_report_since_done_only()
    test_parse_since_rejects_bad_date()
    test_emit_out_writes_file()
    test_stats_json_roundtrip()
    test_task_slices_ended_stamps_prefer_steps()
    test_store_package_exports_backends()
    print("ok")

