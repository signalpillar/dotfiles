#!/usr/bin/env python3
"""Regression tests for packages/pi-job-harness/bin/pi-job."""

from __future__ import annotations

import argparse
import hashlib
import importlib.machinery
import importlib.util
import os
import subprocess
import sys
import tempfile
import threading
from collections.abc import Iterator
from concurrent.futures import ThreadPoolExecutor
from contextlib import contextmanager
from datetime import UTC, datetime
from pathlib import Path

import yaml

DEFAULT_OWNER = "orchestrator"


def _now_iso() -> str:
    return datetime.now(UTC).isoformat().replace("+00:00", "Z")


def claim_dict(slice_key: str, *, owner: str = DEFAULT_OWNER, last_seen: str | None = None) -> dict:
    """Build one `orchestration.cursors[]` entry (fresh claimed_at/last_seen by default)."""
    now = _now_iso()
    return {"owner": owner, "slice": slice_key, "claimed_at": now, "last_seen": last_seen or now}

ROOT = Path(__file__).resolve().parents[3]
PI_JOB = Path(__file__).resolve().parents[1] / "bin" / "pi-job"
if not PI_JOB.exists():
    PI_JOB = PI_JOB.with_name("executable_pi-job")


def load_pi_job_module():
    """Import pi-job (no .py suffix, chezmoi's executable_ naming) as a module so tests
    can exercise YamlTaskStore, FsTaskStore, and TaskLayout directly instead of only via
    subprocess. Safe: `main()` only runs under `if __name__ == "__main__":`."""
    loader = importlib.machinery.SourceFileLoader("pi_job_under_test", str(PI_JOB))
    spec = importlib.util.spec_from_file_location("pi_job_under_test", PI_JOB, loader=loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module  # dataclasses needs the module registered before exec
    loader.exec_module(module)
    return module

def dump_task_yaml(mapping: dict) -> str:
    return yaml.safe_dump(mapping, sort_keys=False, allow_unicode=True)


def write_task_yaml(path: Path, mapping: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(dump_task_yaml(mapping), encoding="utf-8")


def _orchestration_policy() -> dict:
    return {
        "coding_execution": {
            "subagent_required": True,
            "lower_power_model_preferred": True,
            "orchestrator_reviews_subagent": True,
        }
    }


def standard_fixture_mapping(
    *,
    title: str = "Fixture task",
    cursor: tuple[str, str] = ("old-slice", "old-step"),
    uninitialized: bool = False,
) -> dict:
    """Port of TASK_FIXTURE / UNINITIALIZED_TASK_FIXTURE slice graph."""
    mapping: dict = {
        "title": title,
        "status": "in_progress",
        "project": {"name": "Fixture"},
        "plan": {
            "note": "",
            "slices": [
                {
                    "key": "first",
                    "kind": "implement",
                    "title": "First",
                    "goal": "Already done",
                    "status": "done",
                    "note": "",
                    "steps": [],
                    "final_steps": [],
                },
                {
                    "key": "second-slice",
                    "kind": "implement",
                    "title": "Second",
                    "goal": "Find next planned step",
                    "status": "in_progress",
                    "note": "",
                    "steps": [
                        {"key": "s1", "title": "Done", "status": "done", "note": ""},
                        {"key": "s2", "title": "Next", "status": "planned", "note": ""},
                    ],
                    "final_steps": [
                        {"key": "finish", "title": "Finish", "status": "planned", "note": ""},
                    ],
                },
            ],
        },
    }
    if not uninitialized:
        mapping["orchestration"] = {
            "cursors": [claim_dict(cursor[0])],
            "policy": _orchestration_policy(),
        }
    return mapping


def fixture_with_dependencies_mapping(
    *,
    title: str = "Dependency test",
    cursor: tuple[str, str | None] = ("only-slice", "create-plan"),
) -> dict:
    """Port of PLAN_BODY_WITH_DEPENDENCIES."""
    mapping = standard_fixture_mapping(title=title, cursor=(cursor[0], cursor[1] or ""))
    mapping["plan"]["slices"] = [
        {
            "key": "base",
            "kind": "implement",
            "title": "Base",
            "goal": "Already done",
            "status": "done",
            "note": "",
            "steps": [],
            "final_steps": [],
        },
        {
            "key": "blocked-dependent",
            "kind": "implement",
            "title": "Blocked Dependent",
            "goal": "Depends on not-yet-done",
            "status": "planned",
            "note": "",
            "depends_on": ["not-yet-done"],
            "steps": [],
            "final_steps": [],
        },
        {
            "key": "ready-dependent",
            "kind": "implement",
            "title": "Ready Dependent",
            "goal": "Depends on base (done)",
            "status": "planned",
            "note": "",
            "depends_on": ["base"],
            "steps": [],
            "final_steps": [],
        },
        {
            "key": "blocked-status-slice",
            "kind": "implement",
            "title": "Blocked Status",
            "goal": "Has blocked status",
            "status": "blocked",
            "note": "",
            "steps": [],
            "final_steps": [],
        },
    ]
    mapping["orchestration"]["cursors"] = [claim_dict(cursor[0])]
    return mapping


def closing_slice_mapping(
    *,
    title: str = "Implement done fixture",
    cursor: tuple[str, str] = ("implement-done", "edit-code"),
) -> dict:
    """Port of CLOSING_PLAN_BODY."""
    return {
        "title": title,
        "status": "in_progress",
        "project": {"name": "Fixture"},
        "orchestration": {
            "cursors": [claim_dict(cursor[0])],
            "policy": _orchestration_policy(),
        },
        "plan": {
            "note": "",
            "slices": [
                {
                    "key": "implement-done",
                    "kind": "implement",
                    "title": "Implement done",
                    "goal": "Already done",
                    "status": "done",
                    "note": "",
                    "steps": [
                        {"key": "edit-code", "title": "Edit", "status": "done", "note": ""},
                    ],
                    "final_steps": [],
                },
                {
                    "key": "closing",
                    "kind": "closing",
                    "title": "Closing",
                    "goal": "Cross-slice bookkeeping",
                    "status": "planned",
                    "note": "",
                    "steps": [
                        {"key": "update-test-plan", "title": "Update test plan", "status": "planned", "note": ""},
                        {"key": "update-docs", "title": "Update docs", "status": "planned", "note": ""},
                        {"key": "capture-metrics", "title": "Capture metrics", "status": "planned", "note": ""},
                        {"key": "update-task-file", "title": "Update task file", "status": "planned", "note": ""},
                    ],
                    "final_steps": [],
                },
            ],
        },
    }


def all_done_mapping(
    *,
    title: str = "All slices done fixture",
    cursor: tuple[str, str] = ("second-slice", "finish"),
) -> dict:
    """Port of ALL_DONE_PLAN_BODY."""
    return {
        "title": title,
        "status": "in_progress",
        "project": {"name": "Fixture"},
        "orchestration": {
            "cursors": [claim_dict(cursor[0])],
            "policy": _orchestration_policy(),
        },
        "plan": {
            "note": "",
            "slices": [
                {
                    "key": "first",
                    "kind": "implement",
                    "title": "First",
                    "goal": "Already done",
                    "status": "done",
                    "note": "",
                    "steps": [],
                    "final_steps": [],
                },
                {
                    "key": "second-slice",
                    "kind": "implement",
                    "title": "Second",
                    "goal": "Also done",
                    "status": "done",
                    "note": "",
                    "steps": [
                        {"key": "s1", "title": "Done", "status": "done", "note": ""},
                        {"key": "s2", "title": "Also done", "status": "done", "note": ""},
                    ],
                    "final_steps": [
                        {"key": "finish", "title": "Finish", "status": "done", "note": ""},
                    ],
                },
            ],
        },
    }


def sync_mapping() -> dict:
    """Fixture for sync selection behavior."""
    return {
        "title": "Sync fixture",
        "status": "in_progress",
        "source": {"jira": "", "discovered": "", "context": ""},
        "project": {"key": "sync", "name": "Sync", "route": "", "context": ""},
        "context": "",
        "orchestration": {
            "cursors": [claim_dict("only-slice")],
            "policy": _orchestration_policy(),
        },
        "decisions": [],
        "plan": {
            "note": "",
            "slices": [
                {
                    "key": "active-slice",
                    "kind": "implement",
                    "title": "Active",
                    "goal": "g",
                    "status": "in_progress",
                    "note": "",
                    "steps": [{"key": "s1", "title": "s1", "status": "in_progress", "note": ""}],
                    "final_steps": [],
                },
                {
                    "key": "blocked-slice",
                    "kind": "implement",
                    "title": "Blocked",
                    "goal": "g",
                    "status": "blocked",
                    "note": "",
                    "steps": [{"key": "s1", "title": "s1", "status": "planned", "note": ""}],
                    "final_steps": [],
                },
                {
                    "key": "planned-slice",
                    "kind": "implement",
                    "title": "Planned",
                    "goal": "g",
                    "status": "planned",
                    "note": "",
                    "steps": [{"key": "s1", "title": "s1", "status": "planned", "note": ""}],
                    "final_steps": [],
                },
                {
                    "key": "done-with-open-pr",
                    "kind": "implement",
                    "title": "Done but PR open",
                    "goal": "g",
                    "status": "done",
                    "note": "",
                    "repo_work": {
                        "some-repo": {
                            "prs": [
                                {"url": "https://example.com/pr/9", "status": "open", "note": ""},
                            ],
                        },
                    },
                    "steps": [],
                    "final_steps": [],
                },
            ],
        },
    }


def lifecycle_mapping(**scan_step_overrides) -> dict:
    """Port of LIFECYCLE_FIXTURE; pass scan_step_overrides to patch vulnerability-scan."""
    scan = {
        "key": "vulnerability-scan",
        "title": "Scan",
        "status": "planned",
        "note": "",
    }
    scan.update(scan_step_overrides)
    edit_execution = {
        "model": "anthropic/claude-writer",
        "started": "2026-07-01T10:00:00Z",
        "ended": "2026-07-01T10:05:00Z",
    }
    return {
        "title": "Execution lifecycle",
        "status": "in_progress",
        "orchestration": {
            "cursors": [claim_dict("implementation")],
            "policy": _orchestration_policy(),
        },
        "plan": {
            "note": "",
            "slices": [
                {
                    "key": "implementation",
                    "kind": "implement",
                    "title": "Implementation",
                    "goal": "Ship safely",
                    "status": "in_progress",
                    "note": "",
                    "steps": [
                        {
                            "key": "edit-code",
                            "title": "Edit",
                            "status": "done",
                            "note": "",
                            "execution": edit_execution,
                        },
                        scan,
                    ],
                    "final_steps": [],
                },
            ],
        },
    }


def structure_task_yaml(slice_dict: dict, *, title: str = "Structure lint") -> dict:
    return {
        "title": title,
        "status": "in_progress",
        "project": {"name": "Fixture"},
        "plan": {"note": "", "slices": [slice_dict]},
    }


def find_step(mapping: dict, slice_key: str, step_key: str) -> dict:
    for sl in mapping["plan"]["slices"]:
        if sl["key"] == slice_key:
            for step in (sl.get("steps") or []) + (sl.get("final_steps") or []):
                if step["key"] == step_key:
                    return step
    raise KeyError(f"step {slice_key}/{step_key} not found")


def mutate_step_status(
    path: Path,
    slice_key: str,
    step_key: str,
    status: str,
    **fields,
) -> None:
    module = load_pi_job_module()
    store = module.YamlTaskStore(module.YamlTaskLayout(path))
    task = store.read()
    step = find_step(task, slice_key, step_key)
    step["status"] = status
    step.update(fields)
    store.replace(task)


def step_status(path: Path, slice_key: str, step_key: str) -> str:
    module = load_pi_job_module()
    task = module.YamlTaskStore(module.YamlTaskLayout(path)).read()
    return find_step(task, slice_key, step_key)["status"]










def run(*args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    if args and args[0] == str(PI_JOB) and not os.access(PI_JOB, os.X_OK):
        args = (sys.executable, *args)
    res = subprocess.run(args, cwd=ROOT, text=True, capture_output=True, check=False)
    if check and res.returncode != 0:
        raise AssertionError(f"command failed: {' '.join(args)}\nSTDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}")
    return res


def assert_not_contains(haystack: str, needle: str) -> None:
    if needle in haystack:
        raise AssertionError(f"expected {needle!r} absent from:\n{haystack}")


def subagent_instruction_yaml_task(
    *,
    slice_key: str = "implement-slice",
    step_key: str = "edit-code",
) -> str:
    """Initialized YAML task with cursor on a subagent-owned implement step."""

    claim_ts = _now_iso()
    return f"""title: Subagent instruction test
status: in_progress
orchestration:
  cursors:
    - owner: orchestrator
      slice: {slice_key}
      claimed_at: "{claim_ts}"
      last_seen: "{claim_ts}"
  policy:
    coding_execution:
      subagent_required: true
      lower_power_model_preferred: true
      orchestrator_reviews_subagent: true
plan:
  note: ""
  slices:
    - key: {slice_key}
      kind: implement
      title: Implement
      goal: Test subagent instruction
      status: in_progress
      note: ""
      steps:
        - key: create-plan
          title: Create plan
          status: done
          note: ""
        - key: grill-plan
          title: Grill plan
          status: done
          note: ""
        - key: edit-code
          title: Edit code
          status: planned
          note: ""
      final_steps: []
"""


def orchestrator_instruction_yaml_task() -> str:
    """Initialized YAML task with cursor on an orchestrator-owned setup step."""

    claim_ts = _now_iso()
    return f"""title: Orchestrator instruction test
status: in_progress
orchestration:
  cursors:
    - owner: orchestrator
      slice: setup-slice
      claimed_at: "{claim_ts}"
      last_seen: "{claim_ts}"
plan:
  note: ""
  slices:
    - key: setup-slice
      kind: setup
      title: Setup
      goal: Explore before planning
      status: in_progress
      note: ""
      steps:
        - key: explore-context
          title: Explore context
          status: planned
          note: ""
      final_steps: []
"""


def subagent_create_plan_yaml_task(*, slice_key: str = "plan-slice") -> str:
    """Initialized YAML task with cursor on subagent-owned create-plan."""

    claim_ts = _now_iso()
    return f"""title: Create plan instruction test
status: in_progress
orchestration:
  cursors:
    - owner: orchestrator
      slice: {slice_key}
      claimed_at: "{claim_ts}"
      last_seen: "{claim_ts}"
  policy:
    coding_execution:
      subagent_required: true
      lower_power_model_preferred: true
      orchestrator_reviews_subagent: true
plan:
  note: ""
  slices:
    - key: {slice_key}
      kind: implement
      title: Implement
      goal: Test create-plan instruction
      status: in_progress
      note: ""
      steps:
        - key: create-plan
          title: Create plan
          status: planned
          note: ""
      final_steps: []
"""


def assert_contains(haystack: str, needle: str) -> None:
    if needle not in haystack:
        raise AssertionError(f"expected {needle!r} in:\n{haystack}")


def seed_block_after_marker(stdout: str) -> str:
    """Return stdout after the SEED SLICE PLAN FILES NOW marker (exclusive)."""
    if "SEED SLICE PLAN FILES NOW" not in stdout:
        raise AssertionError(f"expected 'SEED SLICE PLAN FILES NOW' in:\n{stdout}")
    return stdout.split("SEED SLICE PLAN FILES NOW", 1)[1]


def minimal_bootstrap_input_yaml(*, initial_slice_kind: str = "setup", slices_yaml: str = "") -> str:
    """Minimal valid bootstrap input matching other bootstrap tests in this file."""
    return f"""title: Seed block test
initial_slice_kind: {initial_slice_kind}
decisions:
  - date: "2026-07-28"
    note: Test decision
    source: test
{slices_yaml}"""


def test_profiled_task() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "fixture.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: ok")
        assert_contains(status, "orchestrator → old-slice")
        assert_contains(status, "Ready: second-slice")
        assert_contains(status, "claim targets a missing slice")

        deprecated = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if deprecated.returncode == 0:
            raise AssertionError("advance should be a deprecated stub that always fails")
        assert_contains(deprecated.stderr, "deprecated")

        run(str(PI_JOB), "--task", str(task), "release", "--owner", "orchestrator")
        run(str(PI_JOB), "--task", str(task), "claim", "--slice", "second-slice", "--owner", "orchestrator")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "orchestrator → second-slice / s2")
        assert_not_contains(advanced, "Next:")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "PI-JOB EXECUTION INSTRUCTION")
        assert_contains(instruction, "Owner: orchestrator")
        assert_contains(instruction, "Role: orchestrator")
        assert_contains(instruction, "CLI-only store")
        assert_contains(instruction, "Slice: second-slice [implement]")
        assert_contains(instruction, "Slice goal: Find next planned step")
        assert_contains(instruction, "Step: s2 — Next")
        assert_contains(instruction, "NEXT ACTION")
        assert_contains(instruction, "STEP")
        assert_contains(instruction, "RECORD RESULTS")
        assert_contains(instruction, "Do not wait for another user prompt")

        assert_contains(instruction, "Todo tracking:")
        assert_contains(instruction, "Align session todos")
        assert_contains(instruction, "Future-work capture:")
        assert_contains(instruction, "Spawn a new slice")


def test_uninitialized_task_requires_orchestration() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "uninitialized" / "task.yaml"
        write_task_yaml(
            task,
            standard_fixture_mapping(title="Uninitialized fixture task", uninitialized=True),
        )

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: required")
        assert_contains(status, "create [--kind setup|implement|...]")

        claim_cmd = run(str(PI_JOB), "--task", str(task), "claim", "--slice", "second-slice", "--owner", "x", check=False)
        if claim_cmd.returncode == 0:
            raise AssertionError("claim unexpectedly succeeded for uninitialized task")
        assert_contains(claim_cmd.stderr, "missing task.orchestration")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if instruction.returncode == 0:
            raise AssertionError("instruction unexpectedly succeeded for uninitialized task")
        assert_contains(instruction.stderr, "missing task.orchestration")

        init_dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        assert_contains(init_dry, "cursors: []")

        run(str(PI_JOB), "--task", str(task), "create")
        initialized = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(initialized, "Initialization: ok")
        assert_contains(initialized, "Cursors: <none>")
        assert_contains(initialized, "Next: `pi-job claim --slice KEY --owner ID`")


def test_init_with_kind_setup_seeds_setup_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty-plan" / "task.yaml"
        write_task_yaml(task, {
            "title": "Empty plan",
            "status": "in_progress",
            "plan": {"note": "", "slices": []},
        })
        run(str(PI_JOB), "--task", str(task), "create", "--kind", "setup")
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Cursors: <none>")
        assert_contains(status, "Ready: setup-slice")
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "setup-slice")
        assert_contains(show, "explore-context")


def test_setup_template_includes_wayfinder_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "setup-wayfinder" / "task.yaml"
        write_task_yaml(task, {
            "title": "Setup wayfinder",
            "status": "in_progress",
            "plan": {"note": "", "slices": []},
        })
        run(str(PI_JOB), "--task", str(task), "create", "--kind", "setup")
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "wayfinder")


def test_fog_slice_kind_seeds_template() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "fog" / "task.yaml"
        write_task_yaml(task, {
            "title": "Fog kind",
            "status": "in_progress",
            "plan": {"note": "", "slices": []},
        })
        run(str(PI_JOB), "--task", str(task), "create", "--kind", "setup")
        run(str(PI_JOB), "--task", str(task), "add-slice", "--kind", "fog",
            "--key", "chart-x", "--title", "Chart X", "--goal", "clear the fog")
        show = run(str(PI_JOB), "--task", str(task), "show", "--slice", "chart-x").stdout
        assert_contains(show, "clarify-scope")
        assert_contains(show, "wayfinder")
        assert_contains(show, "plan-slices")


def test_wayfinder_context_reports_frontier_and_fog() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "wf-context.yaml"
        write_task_yaml(task, {
            "title": "Chart it",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("b")],
                "policy": _orchestration_policy(),
            },
            "decisions": [
                {"date": "2026-08-04", "note": "Use YAML backend", "source": "chat"},
            ],
            "plan": {
                "note": "Ship the thing",
                "slices": [
                    {"key": "a", "kind": "research", "title": "A", "goal": "learn",
                     "status": "done", "note": "found it", "steps": [], "final_steps": []},
                    {"key": "b", "kind": "fog", "title": "B", "goal": "chart",
                     "status": "in_progress", "note": "charting", "steps": [], "final_steps": []},
                    {"key": "c", "kind": "implement", "title": "C", "goal": "build now",
                     "status": "planned", "note": "ready", "depends_on": ["a"],
                     "steps": [], "final_steps": []},
                    {"key": "d", "kind": "implement", "title": "D", "goal": "build later",
                     "status": "planned", "note": "blocked", "depends_on": ["c"],
                     "steps": [], "final_steps": []},
                ],
            },
        })
        out = run(str(PI_JOB), "--task", str(task), "wayfinder-context").stdout
        assert_contains(out, "DESTINATION:")
        assert_contains(out, "Ship the thing")
        assert_contains(out, "Use YAML backend")
        assert_contains(out, "FRONTIER")
        assert_contains(out, "c [implement, planned]")
        assert_contains(out, "FOG")
        assert_contains(out, "d [implement] blocked_by=['c']")


def test_edit_code_owner_from_step_kinds() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "edit-code-owner.yaml"
        write_task_yaml(task, {
            "title": "Edit code owner",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "only-slice",
                    "kind": "implement",
                    "title": "Only",
                    "goal": "Check owner",
                    "status": "in_progress",
                    "note": "",
                    "steps": [
                        {"key": "create-plan", "title": "Plan", "status": "done", "note": ""},
                        {"key": "grill-plan", "title": "Grill", "status": "done", "note": ""},
                        {"key": "edit-code", "title": "Edit", "status": "planned", "note": ""},
                    ],
                    "final_steps": [],
                }],
            },
        })
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Owner: subagent")
        assert_contains(instruction, "Role: orchestrator dispatching subagent")
        assert_contains(instruction, "Step kind: edit-code")
        assert_contains(instruction, "Subagent prompt:")


def test_subagent_instruction_prohibits_direct_task_store_inspection() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-instruction.yaml"
        task.write_text(subagent_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "do not inspect the task store directly")
        assert_not_contains(instruction, "Read the task file")
        assert_not_contains(instruction, "open the task YAML")
        assert_contains(instruction, "Subagent prompt:")
        assert_contains(instruction, "Owner: subagent")
        assert_contains(instruction, "implement-slice / edit-code")
        assert_contains(instruction, "markdown --slice")
        assert_contains(instruction, "markdown --slice SLICE_KEY")
        assert_contains(instruction, "TASK_FILE")
        assert_contains(instruction, "Treat every ## Decisions entry as binding")
        assert_contains(instruction, "show --slice")
        assert_contains(instruction, "show --slice SLICE_KEY")
        assert_not_contains(instruction, "markdown --slice implement-slice")
        assert_not_contains(instruction, f"--task {task}")


def test_subagent_instruction_includes_scoped_read_command() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-scoped-read.yaml"
        task.write_text(subagent_instruction_yaml_task(slice_key="target-slice"), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "markdown --slice SLICE_KEY")
        assert_contains(instruction, "show --slice SLICE_KEY")
        assert_contains(instruction, "TASK_FILE")
        assert_not_contains(instruction, "markdown --slice target-slice")
        assert_not_contains(instruction, "show --slice target-slice")
        assert_not_contains(instruction, "show --slice {slice_key}")
        assert_not_contains(instruction, "markdown --slice {slice_key}")
        assert_not_contains(instruction, f"--task {task}")


def test_orchestrator_instruction_has_no_subagent_prompt() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "orchestrator-instruction.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_not_contains(instruction, "Subagent prompt:")
        assert_not_contains(instruction, "Read the task file")
        assert_contains(instruction, "Channels: pi-job channels")


def test_subagent_orchestrator_prompt_is_separate_from_execution_body() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-orchestrator-markdown.yaml"
        task.write_text(subagent_instruction_yaml_task(slice_key="target-slice"), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_not_contains(instruction, "Orchestrator instruction:")
        assert_contains(instruction, "Subagent prompt:")
        assert_contains(instruction, "markdown --slice SLICE_KEY --with-decisions")
        assert_not_contains(instruction, "markdown --slice target-slice")
        record_idx = instruction.index("RECORD RESULTS")
        prompt_idx = instruction.index("Subagent prompt:")
        assert record_idx < prompt_idx, "RECORD RESULTS must precede Subagent prompt"


def test_add_decision_and_finish_help_describe_channels() -> None:
    module = load_pi_job_module()
    cli_help = module.load_profile_contract()["cli_help"]
    add_help = " ".join(run(str(PI_JOB), "add-decision", "--help").stdout.split())
    finish_help = " ".join(run(str(PI_JOB), "finish", "--help").stdout.split())
    assert_contains(add_help, str(cli_help["add_decision"]["command"]))
    assert_contains(add_help, " ".join(str(cli_help["add_decision"]["note"]).split()))
    assert_contains(finish_help, " ".join(str(cli_help["finish"]["note"]).split()))


def test_decision_document_schema_describes_channels_contract() -> None:
    schema = run(str(PI_JOB), "schema", "--json").stdout
    assert_contains(schema, "Product, scope, architecture, or policy agreement")
    assert_contains(schema, "not step evidence")
    assert_contains(schema, "finish --note")


def test_subagent_instruction_still_inlines_step_kind_guidance() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-guidance.yaml"
        task.write_text(subagent_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Step kind:")
        assert_contains(instruction, "Guidance:")
        assert_contains(instruction, "Make the change described by this slice's create-plan step.")


def test_subagent_instruction_create_plan_includes_plan_path() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "create-plan-instruction.yaml"
        task.write_text(subagent_create_plan_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Slice plan file:")
        assert_contains(instruction, "do not inspect the task store directly")
        assert_not_contains(instruction, "Read the task file")


def orchestrator_grill_plan_yaml_task(*, slice_key: str = "plan-slice") -> str:
    """Initialized YAML task with cursor on orchestrator-owned grill-plan."""

    claim_ts = _now_iso()
    return f"""title: Grill plan instruction test
status: in_progress
orchestration:
  cursors:
    - owner: orchestrator
      slice: {slice_key}
      claimed_at: "{claim_ts}"
      last_seen: "{claim_ts}"
plan:
  note: ""
  slices:
    - key: {slice_key}
      kind: implement
      title: Implement
      goal: Test grill-plan instruction
      status: in_progress
      note: ""
      steps:
        - key: create-plan
          title: Create plan
          status: done
          note: "Plan file: grill-plan-instruction.plans/{slice_key}.md"
        - key: grill-plan
          title: Grill the plan file
          status: planned
          note: ""
      final_steps: []
"""


def _assert_constraint_and_behaviour_plan_contract(instruction: str) -> None:
    """Phrase-lock the profile-owned create-plan / grill-plan contract in instruction packets."""
    assert_contains(instruction, "constraint-and-behaviour contract")
    assert_contains(
        instruction,
        "intent, types and composition, call stacks, system behaviour",
    )
    assert_contains(instruction, "constraints, verification")
    assert_contains(instruction, "optional short touch surface")
    assert_contains(instruction, "Types and composition")
    assert_contains(instruction, "Call stacks")
    assert_contains(instruction, "one indented stack")
    assert_contains(instruction, "prefer pseudo-code")
    assert_contains(
        instruction,
        "Do not move delivery status, cursor, or session journals into plan files",
    )
    assert_contains(instruction, "DX and agent experience share the same constructs")
    assert_contains(instruction, "Persist product/scope/architecture/policy agreements with `pi-job add-decision`")
    assert_contains(instruction, "Step evidence belongs in `finish --note`, not `add-decision`")
    assert_contains(instruction, "Token smell:")
    assert_not_contains(instruction, "approach, files/functions touched, key tradeoffs")


def test_create_plan_instruction_defines_constraint_and_behaviour_contract() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "create-plan-contract.yaml"
        task.write_text(subagent_create_plan_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        _assert_constraint_and_behaviour_plan_contract(instruction)


def test_grill_plan_instruction_defines_constraint_and_behaviour_contract() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "grill-plan-contract.yaml"
        task.write_text(orchestrator_grill_plan_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        _assert_constraint_and_behaviour_plan_contract(instruction)
        assert_contains(
            instruction,
            "Challenge behaviour, boundaries, must-not constraints, verification, types and",
        )
        assert_contains(instruction, "composition, and call stacks")
        assert_contains(instruction, "prose volume is not an acceptance criterion")
        assert_contains(instruction, "capture product/scope choices that should outlive the session")
        assert_contains(instruction, "Do not use add-decision for PR, deploy, e2e, or progress chatter")


def test_profile_yaml_aliases_shared_guidance_strings() -> None:
    """Anchors keep plan/grill and PR guidance as one source; aliases must resolve equal."""
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    plan_text = profile["plan_and_grill_guardrail"]
    if profile["step_kinds"]["create-plan"]["guidance"] != plan_text:
        raise AssertionError("create-plan guidance must alias plan_and_grill_guardrail")
    if profile["step_kinds"]["grill-plan"]["guidance"] != plan_text:
        raise AssertionError("grill-plan guidance must alias plan_and_grill_guardrail")
    pr_text = profile["pr_template_guardrail"]
    if profile["step_kinds"]["share-with-team"]["guidance"] != pr_text:
        raise AssertionError("share-with-team guidance must alias pr_template_guardrail")


def _assert_record_results_block(instruction: str, *, expected_channel_snippets: list[str] | None = None) -> None:
    assert_contains(instruction, "RECORD RESULTS")
    assert_contains(instruction, "machine-owned")
    assert_contains(instruction, "pi-job commands")
    assert_contains(instruction, "Channels: pi-job channels")
    assert_contains(instruction, "TASK_FILE")
    assert_contains(instruction, "SLICE_KEY")
    assert_contains(instruction, "Channels: pi-job channels")
    assert_not_contains(instruction, "Channels (which write for which fact):")
    if expected_channel_snippets:
        for snippet in expected_channel_snippets:
            assert_contains(instruction, snippet)
    record_idx = instruction.index("RECORD RESULTS")
    todo_idx = instruction.index("Todo tracking:")
    assert record_idx < todo_idx, "RECORD RESULTS must appear before Todo tracking"


def test_orchestrator_instruction_includes_record_results() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "orchestrator-discipline.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        _assert_record_results_block(
            instruction,
            expected_channel_snippets=["STEP NOTE (finish --note)", "SLICE NOTE (finish --slice-only)"],
        )


def test_instruction_includes_next_action_and_step_first_layout() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "next-action.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "NEXT ACTION")
        assert_contains(instruction, "Do not wait for another user prompt")
        assert_contains(instruction, "Role: orchestrator")
        assert_contains(instruction, "Task:")
        assert_contains(instruction, str(task))  # loose YAML: header names the real path
        assert_contains(instruction, "TASK_FILE")  # command hints use the token
        assert_not_contains(instruction, f"--task {task}")
        assert_not_contains(instruction, "{task_file}")
        assert_not_contains(instruction, "{cursor}")
        assert_not_contains(instruction, "Lifecycle recording:")
        assert_not_contains(instruction, "Orchestrator instruction:")
        next_idx = instruction.index("NEXT ACTION")
        step_idx = instruction.index("STEP")
        record_idx = instruction.index("RECORD RESULTS")
        assert next_idx < step_idx, "NEXT ACTION must appear before STEP"
        assert step_idx < record_idx, "STEP must appear before RECORD RESULTS"


def test_bootstrap_then_claim_instruction_includes_next_action() -> None:
    """create no longer seeds a claim; the agent must claim before instruction has anything
    to derive a position from."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-next" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(minimal_bootstrap_input_yaml(), encoding="utf-8")
        created = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input)).stdout
        assert_contains(created, "claim \u2192 <none>")
        assert_not_contains(created, "NEXT ACTION")

        run(str(PI_JOB), "--task", str(task), "claim", "--slice", "task-setup", "--owner", DEFAULT_OWNER)
        out = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(out, "NEXT ACTION")
        assert_contains(out, "Do not wait for another user prompt")
        assert_not_contains(out, "Orchestrator instruction:")


def test_subagent_instruction_includes_record_results() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-discipline.yaml"
        task.write_text(subagent_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        _assert_record_results_block(
            instruction,
            expected_channel_snippets=["STEP NOTE (finish --note)", "REPO (set-worktree)"],
        )
        assert_contains(instruction, "Subagent prompt:")
        assert_contains(instruction, "do not inspect the task store directly")
        assert_not_contains(instruction, "Orchestrator instruction:")


def test_record_results_uses_task_file_and_slice_key_hints() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "discipline-hints.yaml"
        task.write_text(subagent_instruction_yaml_task(slice_key="target-slice"), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "TASK_FILE")
        assert_contains(instruction, "SLICE_KEY")
        assert_contains(instruction, "Channels: pi-job channels")
        prompt = instruction.split("Subagent prompt:", 1)[1]
        assert_contains(prompt, "markdown --slice SLICE_KEY --with-decisions")
        assert_contains(prompt, "show --slice SLICE_KEY")
        assert_not_contains(prompt, "markdown --slice target-slice")
        assert_not_contains(prompt, "show --slice target-slice")
        assert_not_contains(instruction, "{task_file}")
        assert_not_contains(instruction, "{slice_key}")
        assert_not_contains(instruction, f"--task {task}")


def test_update_task_file_guidance_names_mutation_commands() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "update-task-file-guidance.yaml"
        mapping = closing_slice_mapping(cursor=("closing", ""))
        closing = next(s for s in mapping["plan"]["slices"] if s["key"] == "closing")
        for step in closing["steps"]:
            if step["key"] != "update-task-file":
                step["status"] = "done"
        write_task_yaml(task, mapping)
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Guidance:")
        assert_contains(instruction, "add-slice")
        assert_contains(instruction, "Do not edit the task store directly")


def test_plan_output_omits_record_results() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "plan-omits-discipline.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_not_contains(plan, "RECORD RESULTS")


def share_with_team_instruction_yaml_task(*, slice_key: str = "ship-slice") -> str:
    """Initialized YAML task with cursor on share-with-team (orchestrator, long PR guidance)."""

    claim_ts = _now_iso()
    return f"""title: Share with team instruction test
status: in_progress
orchestration:
  cursors:
    - owner: orchestrator
      slice: {slice_key}
      claimed_at: "{claim_ts}"
      last_seen: "{claim_ts}"
plan:
  note: ""
  slices:
    - key: {slice_key}
      kind: implement
      title: Ship
      goal: Share ticket and PR for the repo change
      status: in_progress
      note: ""
      steps:
        - key: create-plan
          title: Create plan
          status: done
          note: "Plan file: share.plans/{slice_key}.md"
        - key: grill-plan
          title: Grill plan
          status: done
          note: ""
        - key: edit-code
          title: Edit code
          status: done
          note: ""
        - key: verify
          title: Verify
          status: done
          note: ""
        - key: share-with-team
          title: Share with team
          status: planned
          note: ""
      final_steps: []
"""


def test_pick_next_packet_is_structural_only() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pick-next-structural.yaml"
        write_task_yaml(task, closing_slice_mapping())
        pick_next = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(pick_next, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next, "NEXT ACTION")
        assert_not_contains(pick_next, "Orchestrator instruction:")
        assert_not_contains(pick_next, "Lifecycle recording:")
        assert_not_contains(pick_next, "Channels (which write for which fact):")
        assert_not_contains(pick_next, "RECORD RESULTS")


def test_channels_cli_prints_catalog_and_step_blurbs() -> None:
    catalog = run(str(PI_JOB), "channels").stdout
    assert_contains(catalog, "decision (add-decision)")
    assert_contains(catalog, "step_note (finish --note)")
    step_out = run(str(PI_JOB), "channels", "--step", "share-with-team").stdout
    assert_contains(step_out, "share-with-team record channels")
    assert_contains(step_out, "step_note:")
    assert_contains(step_out, "pull_request:")
    assert_not_contains(step_out, "finding (add-finding)")


def test_profile_rejects_missing_record_channels_on_step_kind() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["step_kinds"]["verify"]["record_channels"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "record_channels")
    else:
        raise AssertionError("profile accepted step kind without record_channels")


def test_profile_rejects_unknown_record_channel_id() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    profile["step_kinds"]["verify"]["record_channels"] = ["not-a-real-channel"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "not-a-real-channel")
    else:
        raise AssertionError("profile accepted unknown record_channels entry")


def test_instruction_collapses_long_slice_goal() -> None:
    long_goal = "G" * 600
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "long-goal.yaml"
        task.write_text(
            orchestrator_instruction_yaml_task().replace(
                "goal: Explore before planning",
                f"goal: {long_goal}",
            ),
            encoding="utf-8",
        )
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "…")
        assert_contains(instruction, "full goal: pi-job --task TASK_FILE markdown --slice setup-slice --with-decisions")
        assert_not_contains(instruction, long_goal)


def test_execution_packet_budget_share_with_team() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "share-budget.yaml"
        task.write_text(share_with_team_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_not_contains(instruction, "Lifecycle recording:")
        assert_not_contains(instruction, "Orchestrator instruction:")
        assert_not_contains(instruction, "PLAN FILE")
        assert_contains(instruction, "PR (add-pr)")
        assert_not_contains(instruction, "FINDING (add-finding)")
        budget = module.InstructionPacketBudget.measure(instruction)
        limits = module.InstructionPacketBudget
        if budget["total_body_bytes"] > limits.TOTAL_MAX_BYTES:
            raise AssertionError(
                f"execution packet body {budget['total_body_bytes']} bytes exceeds "
                f"{limits.TOTAL_MAX_BYTES}: {instruction[:500]}…"
            )
        if budget["generic_bytes"] > limits.GENERIC_MAX_BYTES:
            raise AssertionError(
                f"generic boilerplate {budget['generic_bytes']} bytes exceeds "
                f"{limits.GENERIC_MAX_BYTES}"
            )
        if budget["step_specific_bytes"] <= budget["generic_bytes"]:
            raise AssertionError(
                f"step-specific ({budget['step_specific_bytes']}) must exceed generic ({budget['generic_bytes']})"
            )


def test_subagent_execution_packet_budget_excludes_prompt_body() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-budget.yaml"
        task.write_text(subagent_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        budget = module.InstructionPacketBudget.measure(instruction)
        limits = module.InstructionPacketBudget
        if budget["total_body_bytes"] > limits.TOTAL_MAX_BYTES:
            raise AssertionError(f"subagent execution body too large: {budget['total_body_bytes']}")
        if budget["generic_bytes"] > limits.GENERIC_MAX_BYTES:
            raise AssertionError(f"subagent generic boilerplate too large: {budget['generic_bytes']}")
        if budget["subagent_prompt_bytes"] <= 0:
            raise AssertionError("expected separate Subagent prompt body")
        if budget["subagent_prompt_bytes"] > limits.SUBAGENT_PROMPT_MAX_BYTES:
            raise AssertionError(
                f"subagent prompt {budget['subagent_prompt_bytes']} bytes exceeds "
                f"{limits.SUBAGENT_PROMPT_MAX_BYTES}"
            )


def test_profile_requires_record_results_intro_packet() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["instruction_packets"]["record_results_intro"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "record_results_intro")
    else:
        raise AssertionError("profile accepted instruction_packets without required record_results_intro")


def test_pick_next_slice_reports_closing_slice_ready() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "all-implement-done.yaml"
        write_task_yaml(task, closing_slice_mapping())

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: closing")
        assert_contains(status, "claim (implement-done) is not Ready")
        assert_not_contains(status, "Next:")

        pick_next = run(str(PI_JOB), "--task", str(task), "instruction", "--current", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(f"instruction --current should exit 0 with pick-next:\n{pick_next.stderr}")
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next.stdout, "closing")

        run(str(PI_JOB), "--task", str(task), "release", "--owner", DEFAULT_OWNER)
        run(str(PI_JOB), "--task", str(task), "claim", "--slice", "closing", "--owner", DEFAULT_OWNER)
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, f"{DEFAULT_OWNER} \u2192 closing / update-test-plan")
        assert_not_contains(advanced, "Next:")


def test_status_shows_claim_and_ready_without_next_line() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "aligned-cursor.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, f"{DEFAULT_OWNER} \u2192 second-slice / s2")
        assert_contains(status, "Ready: second-slice")
        assert_not_contains(status, "Next:")
        assert_not_contains(status, "pick-next")


def test_pick_next_reports_all_slices_done() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "all-slices-done.yaml"
        write_task_yaml(task, all_done_mapping())

        pick_next = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(pick_next, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next, "all slices done")

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Task: all slices done")
        assert_contains(status, "Ready: none")
        assert_not_contains(status, "Next:")


def test_advance_is_deprecated_regardless_of_flags() -> None:
    """`advance` no longer moves anything; it always dies with claim/instruction guidance,
    even when invoked with the flags its old --force/--resync modes used to accept."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "deprecated-advance.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        for extra_args in (
            [],
            ["--force", "--reason", "manual override for test"],
            ["--resync", "--reason", "jump to final step", "--slice", "second-slice", "--step", "finish"],
            ["--dry-run"],
        ):
            res = run(str(PI_JOB), "--task", str(task), "advance", *extra_args, check=False)
            if res.returncode == 0:
                raise AssertionError(f"advance {extra_args} unexpectedly succeeded")
            assert_contains(res.stderr, "deprecated")
            assert_contains(res.stderr, "claim")


def test_claim_release_and_one_claim_per_owner() -> None:
    """claim Ready ok; second slice by same owner refuses; release frees; re-claim works."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "claim-lifecycle.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        mapping["orchestration"]["cursors"] = []
        # Make first slice planned+Ready so two Ready slices exist.
        mapping["plan"]["slices"][0]["status"] = "planned"
        mapping["plan"]["slices"][0]["steps"] = [
            {"key": "a1", "title": "A", "status": "planned", "note": ""},
        ]
        write_task_yaml(task, mapping)

        out = run(
            str(PI_JOB), "--task", str(task), "claim",
            "--slice", "first", "--owner", "agent-a",
        ).stdout
        assert_contains(out, "claimed:")
        assert_contains(out, "agent-a")

        refuse = run(
            str(PI_JOB), "--task", str(task), "claim",
            "--slice", "second-slice", "--owner", "agent-a", check=False,
        )
        if refuse.returncode == 0:
            raise AssertionError("second claim by same owner unexpectedly succeeded")
        assert_contains(refuse.stderr, "already has a claim")

        run(str(PI_JOB), "--task", str(task), "release", "--owner", "agent-a")
        again = run(
            str(PI_JOB), "--task", str(task), "claim",
            "--slice", "second-slice", "--owner", "agent-a",
        ).stdout
        assert_contains(again, "second-slice")


def test_claim_displaces_stale_and_refuses_fresh() -> None:
    """Fresh foreign claim blocks; stale foreign claim is displaced."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "stale-claim.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        mapping["orchestration"]["cursors"] = [
            claim_dict("second-slice", owner="old-agent", last_seen="2020-01-01T00:00:00Z"),
        ]
        write_task_yaml(task, mapping)

        displaced = run(
            str(PI_JOB), "--task", str(task), "claim",
            "--slice", "second-slice", "--owner", "new-agent",
        ).stdout
        assert_contains(displaced, "displaced stale claim")
        assert_contains(displaced, "new-agent")

        mapping2 = standard_fixture_mapping(cursor=("second-slice", "s2"))
        mapping2["orchestration"]["cursors"] = [
            claim_dict("second-slice", owner="fresh-agent"),
        ]
        task2 = Path(tmp) / "fresh-claim.yaml"
        write_task_yaml(task2, mapping2)
        blocked = run(
            str(PI_JOB), "--task", str(task2), "claim",
            "--slice", "second-slice", "--owner", "intruder", check=False,
        )
        if blocked.returncode == 0:
            raise AssertionError("claim should refuse a non-stale foreign claim")
        assert_contains(blocked.stderr, "already claimed")
        assert_contains(blocked.stderr, "not stale")


def test_owner_omit_when_sole_claim_and_ambiguous_refuse() -> None:
    """instruction/status mutations may omit --owner with one claim; refuse when many."""
    with tempfile.TemporaryDirectory() as tmp:
        sole = Path(tmp) / "sole.yaml"
        write_task_yaml(sole, standard_fixture_mapping(cursor=("second-slice", "s2")))
        # No --owner: sole claim resolves.
        inst = run(str(PI_JOB), "--task", str(sole), "instruction").stdout
        assert_contains(inst, "second-slice / s2")

        multi = Path(tmp) / "multi.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        mapping["plan"]["slices"][0]["status"] = "planned"
        mapping["plan"]["slices"][0]["steps"] = [
            {"key": "a1", "title": "A", "status": "planned", "note": ""},
        ]
        mapping["orchestration"]["cursors"] = [
            claim_dict("first", owner="a"),
            claim_dict("second-slice", owner="b"),
        ]
        write_task_yaml(multi, mapping)
        ambiguous = run(str(PI_JOB), "--task", str(multi), "instruction", check=False)
        if ambiguous.returncode == 0:
            raise AssertionError("instruction without owner should refuse when >1 claim")
        assert_contains(ambiguous.stderr, "ambiguous owner")
        ok = run(str(PI_JOB), "--task", str(multi), "instruction", "--owner", "b").stdout
        assert_contains(ok, "second-slice / s2")


def test_finish_slice_only_auto_releases_claim() -> None:
    """finish --slice-only on a terminal slice drops the owning claim."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "auto-release.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        # Exhaust steps so slice-only finish is allowed.
        for step in mapping["plan"]["slices"][1]["steps"]:
            step["status"] = "done"
        for step in mapping["plan"]["slices"][1]["final_steps"]:
            step["status"] = "done"
        mapping["orchestration"]["cursors"] = [claim_dict("second-slice", owner="agent-x")]
        write_task_yaml(task, mapping)

        out = run(
            str(PI_JOB), "--task", str(task), "finish",
            "--slice-only", "--slice", "second-slice",
            "--model", "cursor/test-model", "--owner", "agent-x",
            "--reconcile", "--note", "slice complete",
        ).stdout
        assert_contains(out, "auto-released")
        module = load_pi_job_module()
        claims = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["orchestration"]["cursors"]
        if claims:
            raise AssertionError(f"expected empty cursors after auto-release, got {claims}")


def test_missing_task_points_to_scaffold() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        missing = Path(tmp) / "missing.yaml"
        res = run(str(PI_JOB), "--task", str(missing), "status", check=False)
        if res.returncode == 0:
            raise AssertionError("status unexpectedly succeeded for missing task")
        assert_contains(res.stderr, "task store not found")
        assert_contains(res.stderr, "create")
        assert_contains(res.stderr, "--kind setup")


def test_scaffold_creates_task_file() -> None:
    """`create` always scaffolds a bundle: `task.yaml` plus `plans/` and `references/`."""
    with tempfile.TemporaryDirectory() as tmp:
        bundle = Path(tmp) / "nested" / "new-task"
        task = bundle / "task.yaml"
        dry = run(str(PI_JOB), "--task", str(bundle), "create", "--dry-run").stdout
        assert_contains(dry, "title:")
        assert_contains(dry, "key: do-the-change")
        if task.exists():
            raise AssertionError("dry-run wrote a task file")
        if (bundle / "plans").exists():
            raise AssertionError("dry-run scaffolded bundle directories")

        out = run(
            str(PI_JOB),
            "--task",
            str(bundle),
            "create",
            "--title",
            "Scaffolded example",
        ).stdout
        assert_contains(out, f"created: {task.resolve()}")
        assert task.exists()
        assert (bundle / "plans").is_dir()
        assert (bundle / "references").is_dir()

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Task: Scaffolded example")
        assert_contains(status, "Initialization: ok")

        again = run(str(PI_JOB), "--task", str(task), "create", check=False)
        if again.returncode == 0:
            raise AssertionError("create unexpectedly overwrote without --force")
        assert_contains(again.stderr, "already exists")


def test_toolbelt_lists_for_slice_kinds() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        implement_task = Path(tmp) / "implement.yaml"
        write_task_yaml(implement_task, standard_fixture_mapping())
        out = run(str(PI_JOB), "--task", str(implement_task), "toolbelt").stdout
        assert_contains(out, "implement")
        for key in ("httpyac-api-spec", "test-case-table", "endpoint-status-map"):
            assert_contains(out, key)
        assert_contains(out, "[not registered]")

        out_setup = run(str(PI_JOB), "--task", str(implement_task), "toolbelt", "--kind", "setup").stdout
        assert_contains(out_setup, "setup")
        assert_contains(out_setup, "config-flag-matrix")
        assert_contains(out_setup, "endpoint-status-map")

        research_task = Path(tmp) / "research.yaml"
        write_task_yaml(research_task, {
            "title": "Research",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("r")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "r",
                    "kind": "research",
                    "title": "R",
                    "goal": "g",
                    "status": "in_progress",
                    "note": "",
                    "steps": [{"key": "explore-context", "title": "Explore", "status": "planned", "note": ""}],
                    "final_steps": [],
                }],
            },
        })
        out_research = run(str(PI_JOB), "--task", str(research_task), "toolbelt").stdout
        assert_contains(out_research, "sequence-diagram")
        assert_contains(out_research, "state-transition-table")
        assert_contains(out_research, "endpoint-status-map")


def test_endpoint_status_map_catalog_has_build_example() -> None:
    module = load_pi_job_module()
    aid = module.load_profile_contract()["toolbelt"]["endpoint-status-map"]
    assert_contains(aid["title"], "Endpoint status map")
    assert_contains(aid["purpose"], "mutating endpoints")
    example = aid.get("example") or ""
    assert_contains(example, "endpoint status map")
    assert_contains(example, "before -> after")
    assert_contains(example, "dual writers")
    assert_contains(example, "Do not")
    for kind in ("setup", "research", "implement", "spike"):
        if kind not in (aid.get("suits") or []):
            raise AssertionError(f"endpoint-status-map should suit {kind!r}")


def test_toolbelt_add_records_artifact() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "full.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        out = run(
            str(PI_JOB), "--task", str(task), "toolbelt", "add", "httpyac-api-spec",
            "--path", "docs/api.http", "--status", "done", "--note", "Appendix B",
        ).stdout
        assert_contains(out, "registered toolbelt aid: httpyac-api-spec [done]")

        listed = run(str(PI_JOB), "--task", str(task), "toolbelt").stdout
        assert_contains(listed, "httpyac-api-spec [done]")

        # idempotent update in place (status changes, no duplicate key)
        run(str(PI_JOB), "--task", str(task), "toolbelt", "add", "httpyac-api-spec", "--status", "planned")
        module = load_pi_job_module()
        artifacts = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["orchestration"]["artifacts"]
        if list(artifacts.keys()).count("httpyac-api-spec") != 1:
            raise AssertionError(f"expected one httpyac-api-spec entry, got {artifacts!r}")
        assert artifacts["httpyac-api-spec"]["status"] == "planned"

        # unknown key fails closed
        bad = run(str(PI_JOB), "--task", str(task), "toolbelt", "add", "not-a-real-aid", check=False)
        if bad.returncode == 0:
            raise AssertionError("toolbelt add unexpectedly accepted an unknown key")
        assert_contains(bad.stderr, "unknown toolbelt aid")


def test_select_toolbelt_step_and_instruction() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "setup.yaml"
        write_task_yaml(task, {
            "title": "Setup toolbelt",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("setup-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "setup-slice",
                    "kind": "setup",
                    "title": "Setup",
                    "goal": "Pick aids",
                    "status": "in_progress",
                    "note": "",
                    "steps": [
                        {"key": "explore-context", "title": "Explore", "status": "done", "note": ""},
                        {"key": "select-toolbelt", "title": "Select toolbelt", "status": "planned", "note": ""},
                    ],
                    "final_steps": [],
                }],
            },
        })

        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_contains(plan, "select-toolbelt")

        instr = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instr, "Step kind: select-toolbelt")
        assert_contains(instr, "Toolbelt (planning aids)")
        assert_contains(instr, "config-flag-matrix")


def test_toolbelt_block_in_plan() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "setup-plan.yaml"
        write_task_yaml(task, {
            "title": "Setup plan",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("setup-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "setup-slice",
                    "kind": "setup",
                    "title": "Setup",
                    "goal": "g",
                    "status": "in_progress",
                    "note": "",
                    "steps": [{"key": "select-toolbelt", "title": "Select", "status": "planned", "note": ""}],
                    "final_steps": [],
                }],
            },
        })
        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_contains(plan, "Toolbelt (planning aids)")
        assert_contains(plan, "config-flag-matrix")


def test_show_renders_tree_and_footer() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        out = run(str(PI_JOB), "--task", str(task), "show").stdout
        assert_contains(out, "Fixture task")
        assert_contains(out, "implement/1/3")
        assert_contains(out, "second-slice")
        assert_contains(out, "1/2 slices · 1/3 steps")
        assert_contains(out, "← current")
        assert_contains(out, "no aids registered")

        all_out = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(all_out, "s1")

        # footer reflects a registered aid
        run(str(PI_JOB), "--task", str(task), "toolbelt", "add", "httpyac-api-spec", "--path", "docs/api.http", "--status", "done")
        footer = run(str(PI_JOB), "--task", str(task), "show").stdout
        assert_contains(footer, "httpyac-api-spec")
        assert_contains(footer, "docs/api.http")


def test_show_work_first_puts_open_before_done_newest_completed_last_block() -> None:
    """--work-first: unfinished on top; done/skipped at bottom newest-completed first."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "work-first.yaml"
        write_task_yaml(task, {
            "title": "Work first",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("active")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [
                    {
                        "key": "old-done",
                        "kind": "implement",
                        "title": "Old done",
                        "goal": "finished earlier",
                        "status": "done",
                        "note": "",
                        "execution": {
                            "model": "test/model",
                            "started": "2026-01-01T10:00:00Z",
                            "ended": "2026-01-01T11:00:00Z",
                        },
                        "steps": [],
                        "final_steps": [],
                    },
                    {
                        "key": "new-done",
                        "kind": "implement",
                        "title": "New done",
                        "goal": "finished later",
                        "status": "done",
                        "note": "",
                        "execution": {
                            "model": "test/model",
                            "started": "2026-02-01T10:00:00Z",
                            "ended": "2026-02-01T12:00:00Z",
                        },
                        "steps": [],
                        "final_steps": [],
                    },
                    {
                        "key": "waiting",
                        "kind": "implement",
                        "title": "Waiting",
                        "goal": "deps unmet",
                        "status": "planned",
                        "note": "",
                        "depends_on": ["missing-dep"],
                        "steps": [{"key": "create-plan", "title": "Plan", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "ready-open",
                        "kind": "implement",
                        "title": "Ready open",
                        "goal": "can start",
                        "status": "planned",
                        "note": "",
                        "execution": {
                            "model": "test/model",
                            "started": "2026-03-01T09:00:00Z",
                            "ended": None,
                        },
                        "steps": [{"key": "create-plan", "title": "Plan", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "active",
                        "kind": "implement",
                        "title": "Active",
                        "goal": "cursor slice",
                        "status": "in_progress",
                        "note": "",
                        "execution": {
                            "model": "test/model",
                            "started": "2026-03-02T09:00:00Z",
                            "ended": None,
                        },
                        "steps": [{"key": "edit-code", "title": "Edit", "status": "in_progress", "note": ""}],
                        "final_steps": [],
                    },
                ],
            },
        })

        default = run(str(PI_JOB), "--task", str(task), "show").stdout
        # Plan order unchanged without the flag.
        assert default.index("old-done") < default.index("new-done") < default.index("waiting")

        out = run(str(PI_JOB), "--task", str(task), "show", "--work-first").stdout
        # Cursor/active before ready before waiting before finished block.
        assert out.index("active") < out.index("ready-open") < out.index("waiting")
        assert out.index("waiting") < out.index("new-done")
        assert out.index("waiting") < out.index("old-done")
        # Finished block: newest-completed first.
        assert out.index("new-done") < out.index("old-done")


def test_show_aligns_kind_counts_after_longest_key() -> None:
    """Unfinished slice headers align [kind/N/M] just after the longest key (one space)."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "align.yaml"
        write_task_yaml(task, {
            "title": "Align unfinished",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("second-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [
                    {
                        "key": "first",
                        "kind": "implement",
                        "title": "First",
                        "goal": "g",
                        "status": "planned",
                        "note": "",
                        "steps": [{"key": "s1", "title": "S1", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "second-slice",
                        "kind": "implement",
                        "title": "Second",
                        "goal": "g",
                        "status": "in_progress",
                        "note": "",
                        "steps": [
                            {"key": "s1", "title": "S1", "status": "done", "note": ""},
                            {"key": "s2", "title": "S2", "status": "planned", "note": ""},
                        ],
                        "final_steps": [],
                    },
                ],
            },
        })
        out = run(str(PI_JOB), "--task", str(task), "show", "--color", "never").stdout
        cols = []
        for line in out.splitlines():
            if "[" in line and "/" in line and line.lstrip()[:1] in "✓⊘▸✗○":
                cols.append(line.index("["))
        if len(cols) < 2:
            raise AssertionError(f"expected >=2 unfinished slice headers with [kind/N/M]:\n{out}")
        if len(set(cols)) != 1:
            raise AssertionError(f"[kind/N/M] columns should match, got {cols}:\n{out}")
        expected = 2 + len("second-slice") + 1
        if cols[0] != expected:
            raise AssertionError(f"expected [ at column {expected} (tight to longest key), got {cols[0]}:\n{out}")


def test_show_omits_kind_counts_and_models_for_done_by_default() -> None:
    """Done slices omit [kind/n/m]; tree view omits models unless --full."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "compact-show.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))
        # Record a model on the done slice so --full can surface it.
        module = load_pi_job_module()
        store = module.YamlTaskStore(module.YamlTaskLayout(task))
        data = store.read()
        data["plan"]["slices"][0]["execution"] = {
            "model": "cursor/test-model",
            "started": "2026-07-01T10:00:00Z",
            "ended": "2026-07-01T10:05:00Z",
        }
        data["plan"]["slices"][1]["execution"] = {
            "model": "cursor/active-model",
            "started": "2026-07-01T11:00:00Z",
        }
        store.replace(data)

        out = run(str(PI_JOB), "--task", str(task), "show", "--color", "never").stdout
        assert_contains(out, "implement/1/3")  # unfinished second-slice still shows counts
        for line in out.splitlines():
            if line.lstrip().startswith("✓") and "first" in line:
                if "[" in line:
                    raise AssertionError(f"done slice must omit [kind/n/m]:\n{line}\n{out}")
                if "cursor/test-model" in line:
                    raise AssertionError(f"default show must omit models:\n{line}\n{out}")
        assert_not_contains(out, "cursor/active-model")

        full = run(str(PI_JOB), "--task", str(task), "show", "--full", "--color", "never").stdout
        assert_contains(full, "cursor/active-model")
        assert_contains(full, "cursor/test-model")


def test_show_short_collapses_consecutive_done_slices() -> None:
    """--short puts consecutive done slice keys on one line; skipped breaks the run."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "short-show.yaml"
        write_task_yaml(task, {
            "title": "Short show",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("active")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [
                    {
                        "key": "alpha",
                        "kind": "implement",
                        "title": "Alpha",
                        "goal": "g",
                        "status": "done",
                        "note": "",
                        "repos": ["repo-a"],
                        "steps": [{"key": "s1", "title": "S1", "status": "done", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "beta",
                        "kind": "implement",
                        "title": "Beta",
                        "goal": "g",
                        "status": "done",
                        "note": "",
                        "steps": [{"key": "s1", "title": "S1", "status": "done", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "gamma-skip",
                        "kind": "implement",
                        "title": "Gamma",
                        "goal": "g",
                        "status": "skipped",
                        "note": "",
                        "steps": [{"key": "s1", "title": "S1", "status": "skipped", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "delta",
                        "kind": "implement",
                        "title": "Delta",
                        "goal": "g",
                        "status": "done",
                        "note": "",
                        "steps": [{"key": "s1", "title": "S1", "status": "done", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "active",
                        "kind": "implement",
                        "title": "Active",
                        "goal": "g",
                        "status": "in_progress",
                        "note": "",
                        "steps": [{"key": "s1", "title": "S1", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                ],
            },
        })
        out = run(str(PI_JOB), "--task", str(task), "show", "--short", "--color", "never").stdout
        assert_contains(out, "✓ alpha, beta")
        collapsed = next(line for line in out.splitlines() if "alpha, beta" in line)
        if "repo-a" in collapsed:
            raise AssertionError(f"short done line must be names only:\n{collapsed}\n{out}")
        assert_contains(out, "gamma-skip")
        assert_contains(out, "✓ delta")
        # skipped and active stay on their own lines
        gamma_lines = [line for line in out.splitlines() if "gamma-skip" in line]
        if len(gamma_lines) != 1 or "alpha" in gamma_lines[0]:
            raise AssertionError(f"skipped must stay alone:\n{out}")
        # --all disables collapsing
        all_out = run(str(PI_JOB), "--task", str(task), "show", "--short", "--all", "--color", "never").stdout
        if "alpha, beta" in all_out:
            raise AssertionError(f"--all must not collapse done names:\n{all_out}")
        # default show still one line per done slice
        default = run(str(PI_JOB), "--task", str(task), "show", "--color", "never").stdout
        if "alpha, beta" in default:
            raise AssertionError(f"default show must not collapse:\n{default}")
        # claimed slice stays on its own line even when done
        pi_job_module = load_pi_job_module()
        store = pi_job_module.YamlTaskStore(pi_job_module.YamlTaskLayout(task))
        data = store.read()
        data["orchestration"]["cursors"] = [claim_dict("delta")]
        store.replace(data)
        cursor_out = run(str(PI_JOB), "--task", str(task), "show", "--short", "--color", "never").stdout
        assert_contains(cursor_out, "✓ alpha, beta")
        if "alpha, beta, delta" in cursor_out:
            raise AssertionError(f"claimed done slice must not fold into short run:\n{cursor_out}")
        assert_contains(cursor_out, "delta")


def test_show_started_flag_expands_non_planned_slices() -> None:
    """By default only the current cursor's slice expands. --started additionally
    expands in_progress/blocked slices; done/skipped and still-planned stay collapsed.
    --all expands everything."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "started.yaml"
        write_task_yaml(task, {
            "title": "Started slices test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [
                    {
                        "key": "done-not-current",
                        "kind": "implement",
                        "title": "Done",
                        "goal": "Finished",
                        "status": "done",
                        "note": "",
                        "repos": ["graphius"],
                        "depends_on": ["not-started"],
                        "steps": [{"key": "done-1", "title": "Step one", "status": "done", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "in-progress-not-current",
                        "kind": "implement",
                        "title": "In progress",
                        "goal": "Started work",
                        "status": "in_progress",
                        "note": "",
                        "steps": [{"key": "ip-1", "title": "Step one", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "blocked-not-current",
                        "kind": "implement",
                        "title": "Blocked",
                        "goal": "Stuck on external thing",
                        "status": "blocked",
                        "note": "",
                        "steps": [{"key": "bl-1", "title": "Step one", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "not-started",
                        "kind": "implement",
                        "title": "Not started",
                        "goal": "Still queued",
                        "status": "planned",
                        "note": "",
                        "steps": [{"key": "ns-1", "title": "Step one", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                ],
            },
        })

        default_out = run(str(PI_JOB), "--task", str(task), "show").stdout
        for key in ("ip-1", "bl-1", "ns-1", "done-1"):
            if key in default_out:
                raise AssertionError(f"{key} should NOT expand by default (not the cursor's slice):\n{default_out}")
        # Done slices collapse completely: no deps / repos detail either.
        done_block = default_out.split("done-not-current", 1)[1].split("in-progress-not-current", 1)[0]
        if "deps:" in done_block or "repo_work" in done_block or "done-1" in done_block:
            raise AssertionError(f"done slice should be header-only by default:\n{default_out}")

        started_out = run(str(PI_JOB), "--task", str(task), "show", "--started").stdout
        assert_contains(started_out, "ip-1")
        assert_contains(started_out, "bl-1")
        if "ns-1" in started_out:
            raise AssertionError(f"still-planned slice should NOT expand with --started:\n{started_out}")
        if "done-1" in started_out:
            raise AssertionError(f"done slice should NOT expand with --started:\n{started_out}")
        started_done_block = started_out.split("done-not-current", 1)[1].split("in-progress-not-current", 1)[0]
        if "deps:" in started_done_block or "repo_work" in started_done_block:
            raise AssertionError(f"done slice should stay header-only with --started:\n{started_out}")

        all_out = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(all_out, "ip-1")
        assert_contains(all_out, "bl-1")
        assert_contains(all_out, "ns-1")
        assert_contains(all_out, "done-1")
        assert_contains(all_out, "deps:")


def test_show_color_always_tints_glyphs_never_stays_plain() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "color.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        plain = run(str(PI_JOB), "--task", str(task), "show", "--color", "never").stdout
        colored = run(str(PI_JOB), "--task", str(task), "show", "--color", "always").stdout
        if "\033[" in plain:
            raise AssertionError(f"--color never must not emit ANSI escapes:\n{plain!r}")
        assert_contains(colored, "\033[32m✓\033[0m")  # done green
        assert_contains(colored, "\033[36m▸\033[0m")  # current / in_progress cyan
        assert_contains(colored, "\033[1;35m← current\033[0m")  # cursor marker bold magenta
        assert_contains(plain, "← current")
        if "\033[1;35m" in plain:
            raise AssertionError("--color never must not tint ← current")


def test_show_slice_prints_goal_notes_steps_repo_work() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "slice-read.yaml"
        write_task_yaml(task, {
            "title": "Scoped slice read test",
            "status": "in_progress",
            "context": "SECRET CONTEXT",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("target-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "target-slice",
                    "kind": "implement",
                    "title": "Target slice",
                    "goal": "Implement scoped read",
                    "status": "in_progress",
                    "note": "Slice-level guidance",
                    "repos": ["graphius"],
                    "repo_work": {
                        "graphius": {
                            "worktree": "/tmp/wt-scoped",
                            "prs": [
                                {"url": "https://github.com/example/pr/99", "status": "open", "note": "Review me"},
                            ],
                        },
                    },
                    "steps": [
                        {"key": "step-with-note", "title": "With note", "status": "in_progress", "note": "Step-level detail"},
                        {
                            "key": "step-with-model",
                            "title": "With model",
                            "status": "planned",
                            "note": "",
                            "execution": {"model": "anthropic/claude-test", "started": "2026-07-01T10:00:00Z"},
                        },
                    ],
                    "final_steps": [{"key": "finish", "title": "Finish", "status": "planned", "note": ""}],
                }],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "show", "--slice", "target-slice").stdout
        assert_contains(out, "Scoped slice read test")
        assert_contains(out, "slice: target-slice [implement] — Target slice [in_progress]")
        assert_contains(out, "goal: Implement scoped read")
        assert_contains(out, "note: Slice-level guidance")
        assert_contains(out, "step-with-note")
        assert_contains(out, "[in_progress]")
        assert_contains(out, "step-with-model")
        assert_contains(out, "[planned]")
        assert_contains(out, "[anthropic/claude-test]")
        assert_contains(out, "note: Step-level detail")
        assert_contains(out, "repo_work[graphius]: worktree=/tmp/wt-scoped")
        assert_contains(out, "pr open https://github.com/example/pr/99")
        assert_not_contains(out, "Review me")
        assert_contains(out, "← current")
        for forbidden in ("SECRET CONTEXT", "decisions", "Shared context", "— toolbelt —"):
            if forbidden in out:
                raise AssertionError(f"scoped read must not include {forbidden!r}:\n{out}")


def test_show_tree_unchanged_without_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "tree-unchanged.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        for args in (("show",), ("show", "--all")):
            out = run(str(PI_JOB), "--task", str(task), *args).stdout
            for goal_text in ("Already done", "Find next planned step"):
                if goal_text in out:
                    raise AssertionError(
                        f"tree view must not dump slice goals ({goal_text!r}) with {args}:\n{out}"
                    )
            assert_contains(out, "— toolbelt —")


def test_show_slice_unknown_key_dies() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        res = run(
            str(PI_JOB), "--task", str(task), "show", "--slice", "no-such-slice", check=False
        )
        if res.returncode == 0:
            raise AssertionError("expected non-zero exit for unknown slice")
        assert_contains(res.stderr, "slice not found")
        assert_contains(res.stderr, "first")
        assert_contains(res.stderr, "second-slice")


def test_show_slice_marks_current_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "current-step.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        out = run(str(PI_JOB), "--task", str(task), "show", "--slice", "second-slice").stdout
        assert_contains(out, "← current")
        current_lines = [line for line in out.splitlines() if "← current" in line]
        if len(current_lines) != 1 or "s2" not in current_lines[0]:
            raise AssertionError(f"← current must appear only on cursor step s2:\n{out}")
        if any("s1" in line and "← current" in line for line in out.splitlines()):
            raise AssertionError(f"← current must not appear on s1:\n{out}")


def test_show_slice_omits_empty_fields() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty-fields.yaml"
        write_task_yaml(task, {
            "title": "Empty fields test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("sparse")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "sparse",
                    "kind": "implement",
                    "title": "Sparse",
                    "goal": "",
                    "status": "planned",
                    "note": "",
                    "steps": [{"key": "only-step", "title": "Only", "status": "planned", "note": ""}],
                    "final_steps": [],
                }],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "show", "--slice", "sparse").stdout
        for line in out.splitlines():
            if line.startswith("goal:"):
                raise AssertionError(f"empty goal must be omitted:\n{out}")
            if line.startswith("note:"):
                raise AssertionError(f"empty slice note must be omitted:\n{out}")
            if line.startswith("        note:"):
                raise AssertionError(f"empty step note must be omitted:\n{out}")


def test_show_slice_includes_deps_when_present() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "slice_includes_deps_when_present.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Slice deps test", cursor=('only-slice', 'create-plan')))

        waiting = run(
            str(PI_JOB), "--task", str(task), "show", "--slice", "blocked-dependent"
        ).stdout
        assert_contains(waiting, "deps:")
        assert_contains(waiting, "not-yet-done")
        assert_not_contains(waiting, "base:done")

        satisfied = run(
            str(PI_JOB), "--task", str(task), "show", "--slice", "ready-dependent"
        ).stdout
        if "deps:" in satisfied:
            raise AssertionError(f"satisfied deps must be omitted from show:\n{satisfied}")


def test_show_slice_multiline_note_indents_continuation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "multiline-note.yaml"
        write_task_yaml(task, {
            "title": "Multiline note test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("notes")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "notes",
                    "kind": "implement",
                    "title": "Notes",
                    "goal": "g",
                    "status": "in_progress",
                    "note": "line one\nline two",
                    "steps": [{"key": "s1", "title": "S1", "status": "planned", "note": "alpha\nbeta"}],
                    "final_steps": [],
                }],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "show", "--slice", "notes").stdout
        assert_contains(out, "note: line one")
        assert_contains(out, "    line two")
        assert_contains(out, "        note: alpha")
        assert_contains(out, "            beta")


def test_scaffold_mirrors_implement_template() -> None:
    """The scaffold example slice must be generated from the implement step_template so
    it never drifts from the contract. It should carry every template step (including
    wait-for-feedback) and must NOT carry retired keys like reconcile-artifacts."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "new" / "task.yaml"
        dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        for key in (
            "create-plan", "grill-plan", "edit-code", "verify",
            "vulnerability-scan", "share-with-team", "update-task-file", "wait-for-feedback",
            "e2e-evidence", "ready-for-release",
        ):
            assert_contains(dry, f"key: {key}")
        i_wait = dry.index("key: wait-for-feedback")
        i_e2e = dry.index("key: e2e-evidence")
        i_ready = dry.index("key: ready-for-release")
        if not (i_wait < i_e2e < i_ready):
            raise AssertionError(
                f"implement tail order wrong: wait-for-feedback={i_wait} e2e-evidence={i_e2e} ready-for-release={i_ready}"
            )
        if "reconcile-artifacts" in dry:
            raise AssertionError("scaffold still emits retired step key reconcile-artifacts")


def test_scaffold_includes_create_plan_and_grill_plan_before_edit_code() -> None:
    """The scaffold's steps must lead with create-plan then grill-plan, before edit-code -
    modeling the per-slice planning convention for anyone reading a fresh scaffold."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "new" / "task.yaml"
        dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        assert_contains(dry, "key: create-plan")
        assert_contains(dry, "key: grill-plan")
        i_plan = dry.index("create-plan")
        i_grill = dry.index("grill-plan")
        i_edit = dry.index("edit-code")
        if not (i_plan < i_grill < i_edit):
            raise AssertionError(f"steps order wrong: create-plan={i_plan} grill-plan={i_grill} edit-code={i_edit}")


def test_derived_position_walks_create_plan_then_grill_plan_before_edit_code() -> None:
    """The claim's derived position must land on create-plan first, then grill-plan once
    create-plan is done, then edit-code only once both are done - proving the ordering
    guardrail is actually enforced, not just documented."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.yaml"
        write_task_yaml(task, {
            "title": "Plan-and-grill gating test",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "only-slice",
                    "kind": "implement",
                    "title": "Only slice",
                    "goal": "Exercise the gate",
                    "status": "in_progress",
                    "note": "",
                    "steps": [
                        {"key": "create-plan", "title": "Plan", "status": "planned", "note": ""},
                        {"key": "grill-plan", "title": "Grill", "status": "planned", "note": ""},
                        {"key": "edit-code", "title": "Edit", "status": "planned", "note": ""},
                    ],
                    "final_steps": [],
                }],
            },
        })

        instruction = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(instruction, "Step: create-plan")

        mutate_step_status(task, "only-slice", "create-plan", "done")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(instruction, "Step: grill-plan")

        mutate_step_status(task, "only-slice", "grill-plan", "done")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(instruction, "Step: edit-code")


def test_status_ready_skips_unready_head_of_array() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "skips_unready_head_of_array.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Dependency test", cursor=('only-slice', 'create-plan')))

        # blocked-dependent is first but has unmet dep; ready-dependent should be Ready
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: ready-dependent")
        assert_not_contains(status, "Ready: blocked-dependent")


def test_show_ready_tag_lists_only_ready_slices() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "all_lists_only_ready_slices.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Dependency test all", cursor=('only-slice', 'create-plan')))

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        ready_line = next(
            (line for line in show.splitlines() if "ready-dependent" in line),
            "",
        )
        if " ready" not in ready_line:
            raise AssertionError(f"expected ready tag on ready-dependent line in show:\n{show}")
        if any("blocked-dependent" in line and " ready" in line for line in show.splitlines()):
            raise AssertionError(f"blocked-dependent should not be marked ready in show:\n{show}")
        if any("blocked-status-slice" in line and " ready" in line for line in show.splitlines()):
            raise AssertionError(f"blocked-status-slice should not be marked ready in show:\n{show}")


def test_status_ready_line_matches_ready_slices() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "ready_line_matches_ready_slices.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Status ready line test", cursor=('only-slice', 'create-plan')))

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        # Should have a Ready: line with ready-dependent
        if "Ready:" not in status:
            raise AssertionError(f"expected 'Ready:' line in status:\n{status}")
        if "ready-dependent" not in status:
            raise AssertionError(f"expected 'ready-dependent' in Ready: line:\n{status}")


def test_blocked_status_slice_is_skipped() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "blocked-status.yaml"
        write_task_yaml(task, {
            "title": "Blocked status test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "only-blocked",
                    "kind": "implement",
                    "title": "Only Blocked",
                    "goal": "Just a blocked slice",
                    "status": "blocked",
                    "note": "",
                    "steps": [],
                    "final_steps": [],
                }],
            },
        })

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: none")

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        if "only-blocked ready" in show:
            raise AssertionError(f"blocked slice should not be marked ready in show:\n{show}")

        pick_next = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(
                f"instruction should exit 0 with pick-next when Ready is empty:\n{pick_next.stderr}"
            )
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next.stdout, "Ready slices: none")


def test_pick_next_when_nothing_ready() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "nothing-ready.yaml"
        write_task_yaml(task, {
            "title": "Nothing ready test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "unmet-dep",
                    "kind": "implement",
                    "title": "Unmet Dependency",
                    "goal": "Depends on something",
                    "status": "planned",
                    "note": "",
                    "depends_on": ["nonexistent"],
                    "steps": [],
                    "final_steps": [],
                }],
            },
        })

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: none")

        pick_next = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(
                f"instruction should exit 0 with pick-next when nothing is ready:\n{pick_next.stderr}"
            )
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next.stdout, "Ready slices: none")
        assert_not_contains(pick_next.stdout, "all slices done")


def test_status_warns_when_cursor_slice_not_ready() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "warns_when_cursor_slice_not_ready.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Not-ready cursor test", cursor=('blocked-dependent', None)))

        # Cursor points to blocked-dependent (unmet deps), but ready-dependent is Ready
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: ready-dependent")
        if "not Ready" not in status and "⚠" not in status:
            raise AssertionError(f"expected not-Ready cursor warning in:\n{status}")


def test_status_no_warning_when_cursor_slice_is_ready() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "no_warning_when_cursor_slice_is_ready.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Consistent cursor test", cursor=('ready-dependent', None)))

        # Cursor points to ready-dependent, which is Ready (deps are met)
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: ready-dependent")
        if "not Ready" in status:
            raise AssertionError(f"should not warn about not-Ready cursor when slice is Ready:\n{status}")
        if "cursor slice missing" in status:
            raise AssertionError(f"should not warn about missing slice when cursor is valid:\n{status}")


def test_derived_position_walks_steps_then_pick_next_on_exhausted_slice() -> None:
    """Derived position walks unfinished steps as they complete; once the claimed slice is
    exhausted (no unfinished step left), instruction emits pick-next."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "within-slice-pick-next.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        mutate_step_status(task, "second-slice", "s2", "done")
        within = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(within, "Step: finish")

        mutate_step_status(task, "second-slice", "finish", "done")
        pick_next = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(
                f"instruction should exit 0 with pick-next when slice is exhausted:\n{pick_next.stderr}"
            )
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next.stdout, "second-slice")


def test_status_warns_on_unknown_dependency_key() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-dep.yaml"
        write_task_yaml(task, {
            "title": "Unknown dependency test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "bad-dep",
                    "kind": "implement",
                    "title": "Bad Dependency",
                    "goal": "Has typo in dep",
                    "status": "planned",
                    "note": "",
                    "depends_on": ["nonexistent-slice"],
                    "steps": [],
                    "final_steps": [],
                }],
            },
        })

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        if "depends_on unknown slice key" not in status and "⚠" not in status:
            raise AssertionError(f"expected unknown dependency warning in:\n{status}")


def test_show_renders_deps_with_mixed_statuses() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "renders_deps_with_mixed_statuses.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Show deps test", cursor=('only-slice', 'create-plan')))

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        if "deps:" not in show:
            raise AssertionError(f"expected 'deps:' line in show output:\n{show}")
        # Satisfied deps are omitted; only open/missing deps remain.
        if "base:done" in show:
            raise AssertionError(f"done deps must be omitted from show:\n{show}")
        if "not-yet-done" not in show:
            raise AssertionError(f"expected open dep 'not-yet-done' in deps line:\n{show}")
        # blocked-dependent should show not ready annotation
        if "not ready" not in show:
            raise AssertionError(f"expected '(not ready)' annotation:\n{show}")


def test_show_omits_deps_line_when_absent() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-no-deps.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        # Existing fixture has no depends_on, should not show deps lines
        if "deps:" in show:
            raise AssertionError(f"should not show deps: line when no depends_on:\n{show}")


def test_show_graph_emits_mermaid_dependency_flowchart() -> None:
    """--graph prints Mermaid for termaid stdin: classDefs, nodes, dep→dependent edges."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-graph.yaml"
        write_task_yaml(
            task,
            fixture_with_dependencies_mapping(
                title="Show graph test",
                cursor=("ready-dependent", "create-plan"),
            ),
        )

        out = run(str(PI_JOB), "--task", str(task), "show", "--graph").stdout
        assert_contains(out, "flowchart TD")
        assert_contains(out, "classDef done fill:")
        assert_contains(out, "classDef in_progress fill:")
        assert_contains(out, 'base["base"]:::done')
        # Cursor on a non-done slice paints as in_progress (blue), matching show glyphs.
        assert_contains(out, 'ready_dependent["ready-dependent"]:::in_progress')
        assert_contains(out, 'blocked_dependent["blocked-dependent"]:::planned')
        assert_contains(out, "base --> ready_dependent")
        assert_contains(out, "not_yet_done --> blocked_dependent")
        # Clean pipe: no tree chrome.
        assert_not_contains(out, "toolbelt")
        assert_not_contains(out, "cursor →")


def test_show_graph_status_filter_drops_nonmatching_nodes() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-graph-filter.yaml"
        write_task_yaml(task, fixture_with_dependencies_mapping(title="Graph filter"))

        out = run(
            str(PI_JOB), "--task", str(task), "show", "--graph", "--status", "done"
        ).stdout
        assert_contains(out, 'base["base"]:::done')
        assert_not_contains(out, "ready-dependent")
        assert_not_contains(out, "blocked-dependent")
        # Edge to a filtered-out dependent is omitted (no invented missing node for in-plan keys).
        assert_not_contains(out, "base -->")


def test_show_graph_unknown_depends_on_emits_missing_node() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-graph-missing.yaml"
        write_task_yaml(task, {
            "title": "Missing dep graph",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [claim_dict("orphan")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [{
                    "key": "orphan",
                    "kind": "implement",
                    "title": "Orphan",
                    "goal": "Depends on missing",
                    "status": "planned",
                    "note": "",
                    "depends_on": ["no-such-slice"],
                    "steps": [{"key": "s1", "title": "S1", "status": "planned", "note": ""}],
                    "final_steps": [],
                }],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "show", "--graph").stdout
        assert_contains(out, 'no_such_slice["no-such-slice"]:::missing')
        assert_contains(out, "no_such_slice --> orphan")


def test_show_graph_rejects_slice_flag() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-graph-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        res = run(
            str(PI_JOB), "--task", str(task), "show", "--graph", "--slice", "first",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("expected --graph --slice to fail")
        assert_contains(res.stderr, "omit --slice")


def test_init_rejects_forward_reference_dependency() -> None:
    """create seeds cursors:[] (no auto-claim); forward-ref deps leave Ready empty and print guidance."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "forward-ref" / "task.yaml"
        write_task_yaml(task, {
            "title": "Forward reference dependency test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "plan": {
                "note": "",
                "slices": [{
                    "key": "first-slice",
                    "kind": "implement",
                    "title": "First",
                    "goal": "Depends on nonexistent slice",
                    "status": "planned",
                    "note": "",
                    "depends_on": ["nonexistent-slice"],
                    "steps": [],
                    "final_steps": [],
                }],
            },
        })

        init_res = run(str(PI_JOB), "--task", str(task), "create", check=False)
        if init_res.returncode != 0:
            raise AssertionError(
                f"create should succeed with empty cursors even when Ready is empty:\n{init_res.stderr}"
            )
        assert_contains(init_res.stdout, "claim → <none>")
        assert_contains(init_res.stdout, "no slice is dependency-satisfied yet")
        claim = run(
            str(PI_JOB), "--task", str(task), "claim",
            "--slice", "first-slice", "--owner", DEFAULT_OWNER, check=False,
        )
        if claim.returncode == 0:
            raise AssertionError("claim should refuse a non-Ready forward-ref slice")
        assert_contains(claim.stderr, "not Ready")




def test_scaffold_output_still_validates_via_shared_schema() -> None:
    """Real (non-dry-run) scaffold, then pi-job status/show succeed against it."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "schema-validate" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create")

        # status and show should work without errors
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Task:")

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        assert_contains(show, "do-the-change")


def test_add_slice_happy_path_no_repos() -> None:
    """Dry-run and real add-slice on a no-repos fixture; verify output and final state."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        # dry-run should show the literal
        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New Slice", "--goal", "Do work", "--kind", "implement", "--dry-run").stdout
        assert_contains(dry, "key: new-slice")
        assert_contains(dry, "title: New Slice")
        assert_contains(dry, "goal: Do work")

        # real write
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New Slice", "--goal", "Do work", "--kind", "implement")

        # show should list the new slice
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new-slice")


def test_add_slice_rejects_duplicate_key() -> None:
    """add-slice with duplicate key dies."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "dup-key.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "first", "--title", "Duplicate", "--goal", "Should fail", "--kind", "implement", check=False)
        if res.returncode == 0:
            raise AssertionError("add-slice should reject duplicate key")
        assert_contains(res.stderr, "already exists")


def test_add_slice_after_inserts_in_correct_order() -> None:
    """add-slice --after places slice after existing one."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "after.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "between", "--title", "Between", "--goal", "In middle", "--kind", "implement", "--after", "first")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        lines = show.split("\n")
        first_idx = next((i for i, line in enumerate(lines) if "first" in line), -1)
        between_idx = next((i for i, line in enumerate(lines) if "between" in line), -1)
        second_idx = next((i for i, line in enumerate(lines) if "second-slice" in line), -1)
        if not (0 <= first_idx < between_idx < second_idx):
            raise AssertionError(f"order wrong: first={first_idx}, between={between_idx}, second={second_idx}")


def test_add_slice_rejects_unknown_after_slice() -> None:
    """add-slice --after with unknown slice dies."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-after.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new", "--title", "New", "--goal", "Work", "--kind", "implement", "--after", "nonexistent", check=False)
        if res.returncode == 0:
            raise AssertionError("add-slice should reject unknown --after slice")
        assert_contains(res.stderr, "not found")


def test_add_slice_works_on_empty_plan_slices() -> None:
    """add-slice works even on plan.slices: [] fixture (shared schema)."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty-slices.yaml"
        write_task_yaml(task, {
            "title": "Empty plan",
            "status": "in_progress",
            "project": {"name": "Empty"},
            "orchestration": {
                "cursors": [claim_dict("only-slice")],
                "policy": _orchestration_policy(),
            },
            "plan": {"note": "", "slices": []},
        })

        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "first-slice", "--title", "First", "--goal", "Initial work", "--kind", "implement")

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        assert_contains(show, "first-slice")




def test_add_step_happy_path() -> None:
    """add-step dry-run and real write, verify final state."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-step.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        # dry-run
        dry = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "new-step", "--title", "New Step", "--dry-run").stdout
        assert_contains(dry, "key: new-step")
        assert_contains(dry, "title: New Step")

        # real write
        run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "new-step", "--title", "New Step")

        # show should list it
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new-step")


def test_add_step_final_flag() -> None:
    """add-step --final places step in final_steps."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "final-step.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "final-new", "--title", "Final Step", "--final")

        module = load_pi_job_module()
        sl = next(s for s in module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"] if s["key"] == "second-slice")
        final_keys = [step["key"] for step in sl.get("final_steps") or []]
        if "final-new" not in final_keys:
            raise AssertionError(f"expected final-new in final_steps, got {final_keys!r}")
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "final-new")


def test_add_step_rejects_duplicate_key() -> None:
    """add-step rejects duplicate key in same slice."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "dup-step.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        res = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "s1", "--title", "Duplicate", check=False)
        if res.returncode == 0:
            raise AssertionError("add-step should reject duplicate key")
        assert_contains(res.stderr, "already exists")


def test_add_step_rejects_unknown_slice() -> None:
    """add-step with unknown slice dies."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        res = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "nonexistent", "--key", "step", "--title", "Step", check=False)
        if res.returncode == 0:
            raise AssertionError("add-step should reject unknown slice")
        assert_contains(res.stderr, "slice not found")


def test_add_step_after_inserts_in_correct_order() -> None:
    """add-step --after places step after existing one."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "step-after.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "s1b", "--title", "Between", "--after", "s1")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        text = show
        idx_s1 = text.index("s1")
        idx_s1b = text.index("s1b")
        idx_s2 = text.index("s2")
        if not (idx_s1 < idx_s1b < idx_s2):
            raise AssertionError(f"step order wrong: s1={idx_s1}, s1b={idx_s1b}, s2={idx_s2}")




def test_add_slice_happy_path_with_repos() -> None:
    """add-slice with repos field when schema declares it as optional."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "with-repos.yaml"
        write_task_yaml(task, standard_fixture_mapping(title="Fixture with repos"))

        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "repo-slice", "--title", "Repo Slice", "--goal", "Work on repos", "--kind", "implement", "--repos", "graphius,darius", "--dry-run").stdout
        assert_contains(dry, "key: repo-slice")
        assert_contains(dry, "graphius")

        # real write
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "repo-slice", "--title", "Repo Slice", "--goal", "Work on repos", "--kind", "implement", "--repos", "graphius,darius")

        # Verify show lists it
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo-slice")

        # Verify the file is still valid (can export)
        run(str(PI_JOB), "--task", str(task), "show")


def test_add_slice_requires_repos_when_schema_requires_it() -> None:
    """YAML tasks use profile slice shape: repos is optional unless --repos is passed."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "repos-required.yaml"
        write_task_yaml(task, standard_fixture_mapping(title="Repos optional on YAML"))

        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new", "--title", "New", "--goal", "Work", "--kind", "implement")
        if res.returncode != 0:
            raise AssertionError(
                "YAML add-slice should succeed without --repos (profile does not require repos on task schema)\n"
                f"STDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}"
            )
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new")


def test_add_slice_rejects_unsupported_required_field() -> None:
    """YAML add-slice ignores unsupported local required fields."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unsupported-field.yaml"
        write_task_yaml(task, standard_fixture_mapping(title="Unsupported field"))

        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new", "--title", "New", "--goal", "Work", "--kind", "implement")
        if res.returncode != 0:
            raise AssertionError(
                "YAML add-slice should not consult unsupported local #Slice fields\n"
                f"STDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}"
            )
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new")




def test_validate_warns_when_persisted_slice_predates_template_addition() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "older-template" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create")
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        sl = task_data["plan"]["slices"][0]
        sl["steps"] = [s for s in sl["steps"] if s["key"] != "vulnerability-scan"]
        store.replace(task_data)
        result = run(str(PI_JOB), "--task", str(task), "validate")
        assert_contains(result.stdout, "ok:")
        assert_contains(result.stdout, "predates template step(s) vulnerability-scan")


def _scaffolded_task_with_long_step_note(task: Path, *, note_len: int = 2001) -> None:
    run(str(PI_JOB), "--task", str(task), "create")
    module = load_pi_job_module()
    store = module.open_task_store(task)
    task_data = store.read()
    task_data["plan"]["slices"][0]["steps"][0]["note"] = "x" * note_len
    store.replace(task_data)


def test_validate_warns_on_long_note() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "long_note_validate" / "task.yaml"
        _scaffolded_task_with_long_step_note(task)
        result = run(str(PI_JOB), "--task", str(task), "validate")
        assert_contains(result.stdout, "warning:")
        assert_contains(result.stdout, "oversized note")
        assert_contains(result.stdout, "do-the-change/create-plan")


def test_status_warns_on_long_note() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "long_note_status" / "task.yaml"
        _scaffolded_task_with_long_step_note(task)
        result = run(str(PI_JOB), "--task", str(task), "status")
        assert_contains(result.stdout, "warning:")
        assert_contains(result.stdout, "oversized note")
        assert_contains(result.stdout, "do-the-change/create-plan")


def test_validate_warns_on_large_task_file() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "large_task" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create")
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        task_data["context"] = "x" * 101_000
        store.replace(task_data)
        result = run(str(PI_JOB), "--task", str(task), "validate")
        assert_contains(result.stdout, "warning:")
        assert_contains(result.stdout, "task file size")
        assert_contains(result.stdout, "100000")


def test_finish_note_not_refused_when_long() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish_long_note.yaml"
        write_task_yaml(task, lifecycle_mapping())
        run(str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer")
        long_note = "x" * 5000
        res = run(str(PI_JOB), "--task", str(task), "finish", "--note", long_note)
        if res.returncode != 0:
            raise AssertionError(f"finish with long note should succeed:\n{res.stderr}")
        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "implementation", "vulnerability-scan")
        assert long_note in step["note"]





def test_validate_fails_when_slice_missing_template_steps() -> None:
    """Structural lint: an implement slice missing template steps (here everything past
    create-plan, including edit-code) must fail validate, naming the missing keys."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "missing-steps.yaml"
        write_task_yaml(
            task,
            structure_task_yaml({
                "key": "only",
                "kind": "implement",
                "title": "Only",
                "goal": "g",
                "status": "planned",
                "note": "",
                "steps": [{"key": "create-plan", "title": "Plan", "status": "planned", "note": ""}],
                "final_steps": [],
            }),
        )
        res = run(str(PI_JOB), "--task", str(task), "validate", check=False)
        if res.returncode == 0:
            raise AssertionError("validate should fail when a slice omits its kind's template steps")
        assert_contains(res.stderr, "missing required step(s)")
        assert_contains(res.stderr, "edit-code")


def test_validate_allows_extra_steps_beyond_template() -> None:
    """The template is a minimum, not an exact set: a slice may carry additional steps
    (here a domain step between grill-plan and edit-code) and still validate."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "extra-steps.yaml"
        write_task_yaml(
            task,
            structure_task_yaml({
                "key": "only",
                "kind": "implement",
                "title": "Only",
                "goal": "g",
                "status": "planned",
                "note": "",
                "steps": [
                    {"key": "create-plan", "title": "Plan", "status": "planned", "note": ""},
                    {"key": "grill-plan", "title": "Grill", "status": "planned", "note": ""},
                    {"key": "extra-domain-step", "title": "Extra", "status": "planned", "note": ""},
                    {"key": "edit-code", "title": "Edit", "status": "planned", "note": ""},
                    {"key": "verify", "title": "Verify", "status": "planned", "note": ""},
                ],
                "final_steps": [
                    {"key": "e2e-evidence", "title": "Evidence", "status": "planned", "note": ""},
                    {"key": "vulnerability-scan", "title": "Scan", "status": "planned", "note": ""},
                    {"key": "share-with-team", "title": "Share", "status": "planned", "note": ""},
                    {"key": "update-task-file", "title": "Update", "status": "planned", "note": ""},
                    {"key": "wait-for-feedback", "title": "Wait", "status": "planned", "note": ""},
                ],
            }),
        )
        out = run(str(PI_JOB), "--task", str(task), "validate").stdout
        assert_contains(out, "ok:")


def test_validate_fails_on_unknown_slice_kind() -> None:
    """A slice whose kind is not in the contract's slice_kinds must fail validate."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bad-kind.yaml"
        write_task_yaml(
            task,
            structure_task_yaml({
                "key": "only",
                "kind": "banana",
                "title": "Only",
                "goal": "g",
                "status": "planned",
                "note": "",
                "steps": [{"key": "create-plan", "title": "Plan", "status": "planned", "note": ""}],
                "final_steps": [],
            }),
        )
        res = run(str(PI_JOB), "--task", str(task), "validate", check=False)
        if res.returncode == 0:
            raise AssertionError("validate should fail on an unknown slice kind")
        assert_contains(res.stderr, "unknown kind")


def _mixed_legacy_validate_fixture(module, task_path: Path) -> None:
    """YAML task with one conformant implement slice and two legacy-debt slices."""
    required = module.get_slice_kind("implement").get("required_steps") or []
    good_steps = [
        {"key": key, "title": title, "status": "planned", "note": ""}
        for key, title in module.steps_from_kind_template("implement")
        if str(key) in {str(step_key) for step_key in required}
    ]
    task = module.example_task_mapping(title="Mixed legacy structure")
    task["plan"]["slices"] = [
        {
            "key": "good",
            "kind": "implement",
            "title": "Conformant slice",
            "goal": "Passes scoped validate",
            "status": "planned",
            "note": "",
            "steps": good_steps,
            "final_steps": [],
        },
        {
            "key": "legacy-unknown-kind",
            "kind": "banana",
            "title": "Legacy unknown kind",
            "goal": "Old slice shape",
            "status": "done",
            "note": "",
            "steps": [{"key": "create-plan", "title": "Plan", "status": "done", "note": ""}],
            "final_steps": [],
        },
        {
            "key": "legacy-missing-steps",
            "kind": "implement",
            "title": "Legacy implement slice",
            "goal": "Only create-plan",
            "status": "done",
            "note": "",
            "steps": [{"key": "create-plan", "title": "Plan", "status": "done", "note": ""}],
            "final_steps": [],
        },
    ]
    module.YamlTaskStore(module.YamlTaskLayout(task_path)).replace(task)


def test_validate_slice_passes_when_only_that_slice_is_conformant() -> None:
    """Scoped validate succeeds for a conformant slice and notes full-task legacy debt."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "mixed-legacy.yaml"
        _mixed_legacy_validate_fixture(module, task_path)

        res = run(str(PI_JOB), "--task", str(task_path), "validate", "--slice", "good", check=False)
        if res.returncode != 0:
            raise AssertionError(
                "validate --slice good should pass when that slice is conformant\n"
                f"STDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}"
            )
        assert_contains(res.stdout, "ok:")
        assert_contains(res.stdout, "title: Mixed legacy structure")
        assert_contains(res.stdout, "full-task: 2 legacy structure issue(s); use validate without --slice")


def test_validate_slice_fails_for_nonconformant_slice() -> None:
    """Scoped validate fails closed when the selected slice violates profile structure."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "mixed-legacy.yaml"
        _mixed_legacy_validate_fixture(module, task_path)

        res = run(
            str(PI_JOB), "--task", str(task_path), "validate", "--slice", "legacy-missing-steps", check=False,
        )
        if res.returncode == 0:
            raise AssertionError("validate --slice legacy-missing-steps should fail")
        assert_contains(res.stderr, "missing required step(s)")
        assert_contains(res.stderr, "legacy-missing-steps")


def test_validate_slice_rejects_unknown_slice() -> None:
    """Unknown --slice key dies listing known slice keys."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "mixed-legacy.yaml"
        _mixed_legacy_validate_fixture(module, task_path)

        res = run(str(PI_JOB), "--task", str(task_path), "validate", "--slice", "missing", check=False)
        if res.returncode == 0:
            raise AssertionError("validate --slice missing should fail")
        assert_contains(res.stderr, "slice not found: 'missing'")
        assert_contains(res.stderr, "known slice keys: good, legacy-unknown-kind, legacy-missing-steps")


def test_validate_without_slice_still_fails_on_legacy_debt() -> None:
    """Full validate still dies on any structure issues (regression)."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "mixed-legacy.yaml"
        _mixed_legacy_validate_fixture(module, task_path)

        res = run(str(PI_JOB), "--task", str(task_path), "validate", check=False)
        if res.returncode == 0:
            raise AssertionError("full validate should fail when any slice has structure issues")
        assert_contains(res.stderr, "slice structure invalid")
        assert_contains(res.stderr, "legacy-unknown-kind")
        assert_contains(res.stderr, "legacy-missing-steps")


def _initialized_mixed_legacy_fixture(module, task_path: Path) -> None:
    """Mixed legacy YAML task with orchestration so status reports Initialization: ok."""
    _mixed_legacy_validate_fixture(module, task_path)
    task = module.YamlTaskStore(module.YamlTaskLayout(task_path)).read()
    task["orchestration"] = {
        "cursors": [claim_dict("good")],
        "policy": {
            "coding_execution": {
                "subagent_required": True,
                "lower_power_model_preferred": True,
                "orchestrator_reviews_subagent": True,
            }
        },
    }
    module.YamlTaskStore(module.YamlTaskLayout(task_path)).replace(task)


def test_status_reports_structure_ok_for_conformant_task() -> None:
    """Healthy initialized task reports Structure: ok."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "ok.yaml"
        module = load_pi_job_module()
        mapping = module.example_task_mapping(title="Validate ok")
        mapping["project"] = {"name": "Fixture"}
        mapping["orchestration"] = {
            "cursors": [claim_dict("only")],
            "policy": _orchestration_policy(),
        }
        mapping["plan"]["slices"] = [{
            "key": "only",
            "kind": "implement",
            "title": "Only",
            "goal": "g",
            "status": "planned",
            "note": "",
            "steps": [
                {"key": key, "title": title, "status": "planned", "note": ""}
                for key, title in module.steps_from_kind_template("implement")
            ],
            "final_steps": [],
        }]
        write_task_yaml(task, mapping)
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: ok")
        assert_contains(status, "Structure: ok")


def test_status_reports_structure_invalid_without_failing() -> None:
    """Mixed legacy fixture reports Structure invalid but status still succeeds."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "mixed-legacy.yaml"
        _initialized_mixed_legacy_fixture(module, task_path)

        res = run(str(PI_JOB), "--task", str(task_path), "status", check=False)
        if res.returncode != 0:
            raise AssertionError(
                "status must not fail on structure issues\n"
                f"STDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}"
            )
        assert_contains(res.stdout, "Initialization: ok")
        assert_contains(res.stdout, "Structure: invalid (2 issues; try validate or validate --slice <key>)")
















def test_set_worktree_happy_path() -> None:
    """set-worktree dry-run shows path; real run and show renders it."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        # dry-run shows the literal
        dry = run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1", "--dry-run").stdout
        assert_contains(dry, "worktree: /tmp/wt1")

        # real write
        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")

        # show should render it
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo_work[graphius]")
        assert_contains(show, "worktree=/tmp/wt1")


def test_set_worktree_upserts_existing_path() -> None:
    """set-worktree twice with different paths; show contains only the latest."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-upsert.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        # first set
        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")
        # second set with different path
        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt2")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        # Should contain wt2, not wt1
        if "worktree=/tmp/wt1" in show:
            raise AssertionError(f"old worktree path still present in show:\n{show}")
        assert_contains(show, "worktree=/tmp/wt2")


def test_set_worktree_rejects_unknown_slice() -> None:
    """set-worktree dies when slice doesn't exist."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-bad-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        res = run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "nonexistent", "--repo", "graphius", "--path", "/tmp/wt", check=False)
        if res.returncode == 0:
            raise AssertionError("set-worktree should reject unknown slice")
        assert_contains(res.stderr, "slice not found")


def test_set_worktree_clear_rejects_missing_repo() -> None:
    """set-worktree --clear fails closed when repo_work entry was never created."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-missing-repo.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        res = run(
            str(PI_JOB), "--task", str(task), "set-worktree",
            "--slice", "second-slice", "--repo", "graphius", "--clear",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("set-worktree --clear should reject missing repo entry")
        assert_contains(res.stderr, "repo work not found")


def test_set_worktree_clear_rejects_path_and_clear() -> None:
    """set-worktree rejects --path and --clear together."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-both-modes.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        res = run(
            str(PI_JOB), "--task", str(task), "set-worktree",
            "--slice", "second-slice", "--repo", "graphius",
            "--path", "/tmp/wt", "--clear",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("set-worktree should reject --path and --clear together")
        stderr = res.stderr.lower()
        if "mutually exclusive" not in stderr and "not allowed with argument" not in stderr:
            raise AssertionError(f"expected argparse mutual-exclusion error, got:\n{res.stderr}")


def test_set_worktree_clear_rejects_missing_path_and_clear() -> None:
    """set-worktree without --path/--clear prints a recommendation and fails non-zero
    (not a bare argparse mode error) instead of recording anything."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-no-mode.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        before = task.read_text(encoding="utf-8")

        res = run(
            str(PI_JOB), "--task", str(task), "set-worktree",
            "--slice", "second-slice", "--repo", "graphius",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("set-worktree should require --path or --clear to record")
        assert_contains(res.stdout, "recommended worktree path:")
        assert_contains(res.stderr, "set-worktree requires --path to record")
        if task.read_text(encoding="utf-8") != before:
            raise AssertionError("set-worktree must not mutate the task file on recommend-only")


def test_set_worktree_clear_happy_path() -> None:
    """set-worktree --clear removes the recorded worktree path from an existing repo entry."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-happy.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")
        clear = run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--clear")
        assert_contains(clear.stdout, "cleared worktree: second-slice/graphius")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo_work[graphius]")
        assert_contains(show, "worktree=not set")

        module = load_pi_job_module()
        repo_entry = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][1]["repo_work"]["graphius"]
        if "worktree" in repo_entry:
            raise AssertionError(f"worktree key still present after clear: {repo_entry!r}")


def test_set_worktree_clear_leaves_prs() -> None:
    """set-worktree --clear removes worktree but keeps PR records."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-leaves-prs.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")
        run(
            str(PI_JOB), "--task", str(task), "add-pr",
            "--slice", "second-slice", "--repo", "graphius",
            "--url", "https://github.com/example/pr/1", "--status", "open",
        )
        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--clear")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "worktree=not set")
        assert_contains(show, "pr open https://github.com/example/pr/1")


def test_set_worktree_clear_idempotent_without_worktree() -> None:
    """set-worktree --clear succeeds when repo entry exists but worktree is already absent."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-idempotent.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(
            str(PI_JOB), "--task", str(task), "add-pr",
            "--slice", "second-slice", "--repo", "graphius",
            "--url", "https://github.com/example/pr/1", "--status", "open",
        )
        clear = run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--clear")
        assert_contains(clear.stdout, "cleared worktree: second-slice/graphius")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "worktree=not set")
        assert_contains(show, "pr open https://github.com/example/pr/1")


def test_set_worktree_clear_dry_run_no_mutation() -> None:
    """set-worktree --clear --dry-run validates and previews without mutating the task file."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-dry-run.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")
        before = task.read_bytes()
        dry = run(
            str(PI_JOB), "--task", str(task), "set-worktree",
            "--slice", "second-slice", "--repo", "graphius", "--clear", "--dry-run",
        )
        assert_contains(dry.stdout, "would clear worktree: second-slice/graphius")
        after = task.read_bytes()
        if after != before:
            raise AssertionError("dry-run --clear mutated the task file")


def test_yaml_store_clear_worktree() -> None:
    """YamlTaskStore.clear_worktree removes worktree key without touching PRs."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "clear-worktree.yaml"
        store = module.YamlTaskStore(module.YamlTaskLayout(task_path))
        store.replace(module.example_task_mapping(title="Clear worktree unit"))
        store.set_worktree(slice_key="do-the-change", repo="repo-a", path="/tmp/worktree")
        store.add_pr(
            slice_key="do-the-change", repo="repo-a", url="https://example.com/pr/1",
            status="open", note="keep me",
        )

        store.clear_worktree(slice_key="do-the-change", repo="repo-a")
        repo_entry = store.read()["plan"]["slices"][0]["repo_work"]["repo-a"]
        if "worktree" in repo_entry:
            raise AssertionError(f"worktree key should be absent after clear: {repo_entry!r}")
        assert repo_entry["prs"][0]["url"] == "https://example.com/pr/1"
        assert repo_entry["prs"][0]["note"] == "keep me"


def test_add_pr_happy_path_creates_repo_work() -> None:
    """add-pr with no prior set-worktree auto-creates repo entry with worktree absent."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pr-happy.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        # add-pr without prior set-worktree
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", "https://github.com/example/pr/1", "--status", "open")

        # show should render repo_work with worktree=not set and PR
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo_work[graphius]")
        assert_contains(show, "worktree=not set")
        assert_contains(show, "pr open https://github.com/example/pr/1")


def test_add_pr_upsert_by_url_keeps_latest_status() -> None:
    """add-pr twice with same URL, different status; show contains URL once with latest status."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pr-upsert.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        url = "https://github.com/example/pr/1"
        # first PR with status open
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", url, "--status", "open")
        # second PR with same URL, status merged
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", url, "--status", "merged")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        # Should contain merged, and URL should appear once
        if show.count(url) != 1:
            raise AssertionError(f"expected URL to appear exactly once, got {show.count(url)}:\n{show}")
        assert_contains(show, "pr merged")


def test_add_pr_rejects_unknown_slice() -> None:
    """add-pr dies when slice doesn't exist."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pr-bad-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        res = run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "nonexistent", "--repo", "graphius", "--url", "https://github.com/example/pr/1", "--status", "open", check=False)
        if res.returncode == 0:
            raise AssertionError("add-pr should reject unknown slice")
        assert_contains(res.stderr, "slice not found")


def test_add_pr_after_set_worktree_preserves_worktree() -> None:
    """set-worktree then add-pr on same slice/repo; both survive in show."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pr-with-worktree.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", "https://github.com/example/pr/1", "--status", "open")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo_work[graphius]")
        assert_contains(show, "worktree=/tmp/wt1")
        assert_contains(show, "pr open https://github.com/example/pr/1")


def test_show_renders_repo_work_worktree_and_prs() -> None:
    """show renders both worktree path and PR status/url/note substrings."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-repo-work.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/home/user/worktrees/graphius")
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", "https://github.com/emed/graphius/pull/123", "--status", "open", "--note", "WIP schema changes")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        # Verify all the rendering parts are present
        assert_contains(show, "repo_work[graphius]: worktree=/home/user/worktrees/graphius")
        assert_contains(show, "pr open https://github.com/emed/graphius/pull/123")
        assert_not_contains(show, "WIP schema changes")


def test_show_status_done_prints_set_worktrees_without_full_expand() -> None:
    """--status done keeps done slices collapsed (no deps/steps) but prints set
    worktrees; default show stays header-only; unset worktrees stay hidden.
    show --help documents the agent inventory path."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "status-done-worktrees.yaml"
        write_task_yaml(task, {
            "title": "Status-done worktree inventory",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursors": [],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [
                    {
                        "key": "pr-only",
                        "kind": "implement",
                        "title": "PR without worktree",
                        "goal": "Repo entry but no path",
                        "status": "done",
                        "note": "",
                        "repos": ["graphius"],
                        "repo_work": {
                            "graphius": {
                                "prs": [
                                    {
                                        "url": "https://github.com/example/pr/8",
                                        "status": "merged",
                                        "note": "",
                                    }
                                ],
                            }
                        },
                        "steps": [{"key": "p1", "title": "Step", "status": "done", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "with-worktree",
                        "kind": "implement",
                        "title": "Has worktree",
                        "goal": "Finished with path",
                        "status": "done",
                        "note": "",
                        "repos": ["graphius"],
                        "depends_on": ["pr-only"],
                        "repo_work": {
                            "graphius": {
                                "worktree": "/tmp/wt-done-inventory",
                                "prs": [
                                    {
                                        "url": "https://github.com/example/pr/9",
                                        "status": "merged",
                                        "note": "",
                                    }
                                ],
                            }
                        },
                        "steps": [{"key": "w1", "title": "Step", "status": "done", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "still-open",
                        "kind": "implement",
                        "title": "Not done",
                        "goal": "Filtered out",
                        "status": "planned",
                        "note": "",
                        "steps": [{"key": "o1", "title": "Step", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                ],
            },
        })

        default_out = run(str(PI_JOB), "--task", str(task), "show").stdout
        if "repo_work" in default_out or "/tmp/wt-done-inventory" in default_out:
            raise AssertionError(f"default show must keep done slices header-only:\n{default_out}")

        status_out = run(str(PI_JOB), "--task", str(task), "show", "--status", "done").stdout
        assert_contains(status_out, "repo_work[graphius]: worktree=/tmp/wt-done-inventory")
        assert_contains(status_out, "with-worktree")
        assert_contains(status_out, "pr-only")
        if "still-open" in status_out:
            raise AssertionError(f"--status done must filter out planned slices:\n{status_out}")
        if "deps:" in status_out:
            raise AssertionError(f"--status done must not expand deps on done slices:\n{status_out}")
        if "w1" in status_out or "p1" in status_out:
            raise AssertionError(f"--status done must not expand steps on done slices:\n{status_out}")
        if "worktree=not set" in status_out:
            raise AssertionError(f"--status done must omit unset worktrees:\n{status_out}")
        if "pr merged" in status_out:
            raise AssertionError(f"--status done must not dump PR lines for inventory:\n{status_out}")

        help_out = run(str(PI_JOB), "show", "--help").stdout
        assert_contains(help_out, "repo_work.worktree")
        assert_contains(help_out, "list recorded worktrees")
        assert_contains(help_out, "--status")


def test_add_slice_still_works_with_repo_work_in_schema() -> None:
    """Regression: add-slice --dry-run doesn't mention repo_work, and real add-slice succeeds."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice-regression.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        # dry-run should not include repo_work
        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New", "--goal", "Work", "--kind", "implement", "--dry-run").stdout
        if "repo_work" in dry:
            raise AssertionError(f"add-slice dry-run should not mention repo_work:\n{dry}")

        # real add-slice should still succeed
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New", "--goal", "Work", "--kind", "implement")

        # show should include the new slice
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new-slice")



def test_sync_default_selection_and_status_override() -> None:
    """Default sync selection: in_progress/blocked slices, plus any slice carrying an
    open PR even if its own status is done. --status overrides to an exact status set."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "sync.yaml"
        write_task_yaml(task, sync_mapping())

        default_out = run(str(PI_JOB), "--task", str(task), "sync").stdout
        assert_contains(default_out, "active-slice")
        assert_contains(default_out, "blocked-slice")
        assert_contains(default_out, "done-with-open-pr")
        assert_contains(default_out, "ACTION REQUIRED")
        assert_contains(default_out, "never calls GitHub or Jira")
        assert_contains(default_out, "LAST-RECORDED")
        if "planned-slice" in default_out:
            raise AssertionError(f"planned-slice (no PR, not in_progress/blocked) should be excluded by default:\n{default_out}")

        status_out = run(str(PI_JOB), "--task", str(task), "sync", "--status", "planned").stdout
        assert_contains(status_out, "planned-slice")
        for excluded in ("active-slice", "blocked-slice", "done-with-open-pr"):
            if excluded in status_out:
                raise AssertionError(f"{excluded} should be excluded by --status planned override:\n{status_out}")


LEGACY_MIGRATE_PREAMBLE = """
package task

#Status: "planned" | "in_progress" | "blocked" | "done" | "skipped"
#Step: {
    key: string
    title: string
    status: #Status
    note: string
}
#Slice: {
    key: string
    kind: string
    title: string
    goal: string
    status: #Status
    note: string
    depends_on?: [...string]
    steps: [...#Step]
    final_steps: [...#Step]
}
"""

LEGACY_MIGRATE_PLAN_BODY = """
    plan: {
        slices: [
            #Slice & {
                key: "first"
                kind: "implement"
                title: "First"
                goal: "Already done"
                status: "done"
                note: ""
                steps: []
                final_steps: []
            },
            #Slice & {
                key: "second-slice"
                kind:   "implement"
                title: "Second"
                goal: "Find next planned step"
                status: "in_progress"
                note: ""
                steps: [
                    #Step & {key: "s1", title: "Done", status: "done", note: ""},
                    #Step & {key: "s2", title: "Next", status: "planned", note: ""},
                ]
                final_steps: [
                    #Step & {key: "finish", title: "Finish", status: "planned", note: ""},
                ]
            },
        ]
    }
"""




def test_fs_task_store_round_trip() -> None:
    """Build a small directory task purely via FsTaskStore mutation methods, then read()
    it back and check the reconstructed dict's shape/values: required fields present,
    optional fields with no data omitted entirely (not None/empty)."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp) / "task"
        base.mkdir()
        (base / "title").write_text("FS round-trip task\n")
        (base / "status").write_text("in_progress\n")

        store = module.FsTaskStore(base)
        store.init_orchestration()

        store.add_slice(
            key="alpha",
            kind="implement",
            title="Alpha",
            goal="Alpha goal",
            extra_fields={"repos": ["repo-a", "repo-b"]},
            steps=[("edit-code", "Edit code")],
            final_steps=[("wrap-up", "Wrap up")],
            after=None,
        )
        store.set_execution(
            slice_key="alpha",
            step_key="edit-code",
            status="done",
            note="implemented",
            execution={
                "model": "anthropic/claude-test",
                "started": "2026-07-01T10:00:00Z",
                "ended": "2026-07-01T10:05:00Z",
            },
        )
        store.add_slice(key="beta", kind="implement", title="Beta", goal="Beta goal", extra_fields={}, steps=[], final_steps=[], after="alpha")

        store.set_worktree(slice_key="alpha", repo="repo-a", path="/tmp/worktrees/alpha")

        action = store.add_pr(slice_key="alpha", repo="repo-a", url="https://example.com/pr/1", status="open", note="first")
        assert action == "added", action

        store.write_artifact("share_with_team", status="planned", path=None, note="registered")

        task = store.read()

        assert task["title"] == "FS round-trip task"
        assert task["status"] == "in_progress"
        assert task["source"] == {}
        assert task["project"] == {}
        assert task["context"] == ""
        assert task["decisions"] == []

        orch = task["orchestration"]
        assert "profile" not in orch
        assert orch["cursors"] == []
        assert orch["policy"]["coding_execution"] == {
            "subagent_required": True,
            "lower_power_model_preferred": True,
            "orchestrator_reviews_subagent": True,
        }
        assert orch["artifacts"]["share_with_team"]["status"] == "planned"
        assert "path" not in orch["artifacts"]["share_with_team"]
        assert orch["artifacts"]["share_with_team"]["note"] == "registered"

        slices = task["plan"]["slices"]
        assert [s["key"] for s in slices] == ["alpha", "beta"]

        alpha = slices[0]
        assert alpha["kind"] == "implement"
        assert alpha["title"] == "Alpha"
        assert alpha["goal"] == "Alpha goal"
        assert alpha["status"] == "planned"
        assert alpha["note"] == ""
        assert alpha["repos"] == ["repo-a", "repo-b"]
        assert "depends_on" not in alpha
        assert [s["key"] for s in alpha["steps"]] == ["edit-code"]
        assert alpha["steps"][0]["execution"]["model"] == "anthropic/claude-test"
        assert alpha["steps"][0]["execution"]["ended"] == "2026-07-01T10:05:00Z"
        assert [s["key"] for s in alpha["final_steps"]] == ["wrap-up"]
        assert alpha["repo_work"]["repo-a"]["worktree"] == "/tmp/worktrees/alpha"
        assert len(alpha["repo_work"]["repo-a"]["prs"]) == 1
        assert alpha["repo_work"]["repo-a"]["prs"][0]["url"] == "https://example.com/pr/1"
        assert alpha["repo_work"]["repo-a"]["prs"][0]["status"] == "open"

        beta = slices[1]
        assert beta["title"] == "Beta"
        assert "repos" not in beta
        assert "depends_on" not in beta
        assert "repo_work" not in beta
        assert beta["steps"] == []
        assert beta["final_steps"] == []


def test_fs_task_store_ordering() -> None:
    """Slices/steps inserted with after= land at the right position in .order / among
    step_dirs()."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp) / "task"
        base.mkdir()
        (base / "title").write_text("Ordering task\n")
        (base / "status").write_text("in_progress\n")
        store = module.FsTaskStore(base)

        store.add_slice(key="one", kind="implement", title="One", goal="g", extra_fields={}, steps=[], final_steps=[], after=None)
        store.add_slice(key="three", kind="implement", title="Three", goal="g", extra_fields={}, steps=[], final_steps=[], after=None)
        store.add_slice(key="two", kind="implement", title="Two", goal="g", extra_fields={}, steps=[], final_steps=[], after="one")

        order_file = base / "plan" / "slices" / ".order"
        assert order_file.read_text().splitlines() == ["one", "two", "three"]

        task = store.read()
        assert [s["key"] for s in task["plan"]["slices"]] == ["one", "two", "three"]

        store.add_step(slice_key="one", key="a", title="A", note="", terminal=False, after=None)
        store.add_step(slice_key="one", key="c", title="C", note="", terminal=False, after=None)
        store.add_step(slice_key="one", key="b", title="B", note="", terminal=False, after="a")

        task = store.read()
        one = next(s for s in task["plan"]["slices"] if s["key"] == "one")
        assert [s["key"] for s in one["steps"]] == ["a", "b", "c"]


def test_fs_task_store_depends_on_symlink() -> None:
    """depends_on round-trips through read() and is an actual symlink on disk, not a
    text file."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp) / "task"
        base.mkdir()
        (base / "title").write_text("Deps task\n")
        (base / "status").write_text("in_progress\n")
        store = module.FsTaskStore(base)

        store.add_slice(key="base-slice", kind="implement", title="Base", goal="g", extra_fields={}, steps=[], final_steps=[], after=None)
        store.add_slice(
            key="dependent",
            kind="implement",
            title="Dependent",
            goal="g",
            extra_fields={"depends_on": ["base-slice"]},
            steps=[],
            final_steps=[],
            after=None,
        )

        link = base / "plan" / "slices" / "dependent" / "depends_on" / "base-slice"
        assert link.is_symlink(), "depends_on entry should be an actual symlink"
        assert os.readlink(link) == "../../base-slice"
        target_slice_dir = base / "plan" / "slices" / "base-slice"
        assert link.resolve() == target_slice_dir.resolve(), (
            "depends_on symlink must resolve to the sibling slice directory, not dangle"
        )
        assert link.is_dir(), "resolved depends_on symlink should point at a real directory"

        task = store.read()
        dependent = next(s for s in task["plan"]["slices"] if s["key"] == "dependent")
        assert dependent["depends_on"] == ["base-slice"]
        base_slice = next(s for s in task["plan"]["slices"] if s["key"] == "base-slice")
        assert "depends_on" not in base_slice


def test_fs_task_store_invalid_status_dies_on_read() -> None:
    """A hand-corrupted status file makes read() die instead of passing the bad value
    through to callers."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp) / "task"
        base.mkdir()
        (base / "title").write_text("Bad status task\n")
        (base / "status").write_text("in_progress\n")
        store = module.FsTaskStore(base)
        store.add_slice(key="one", kind="implement", title="One", goal="g", extra_fields={}, steps=[], final_steps=[], after=None)

        (base / "plan" / "slices" / "one" / "status").write_text("not-a-real-status\n")

        raised = False
        try:
            store.read()
        except SystemExit:
            raised = True
        assert raised, "read() should die on an invalid status value instead of passing it through"












def test_persisted_models_document_every_field() -> None:
    module = load_pi_job_module()
    model_names = (
        "ExecutionDocument", "StepDocument", "DecisionDocument", "ArtifactDocument",
        "PullRequestDocument", "RepositoryWorkDocument", "SliceDocument", "SourceDocument",
        "ProjectDocument", "CodingExecutionPolicyDocument", "OrchestrationPolicyDocument",
        "OwnedCursorDocument", "OrchestrationDocument", "PlanDocument", "TaskDocument",
        "BootstrapSliceDocument", "BootstrapDocument",
        "ConfigLayeringDocument", "ArtifactGateDocument", "ArtifactRuleDocument",
        "ToolbeltAidDocument", "StepKindDocument", "SlicePoliciesDocument",
        "SliceKindDocument", "InstructionPacketsDocument", "CliHelpDocument",
        "CliHelpAddDecisionDocument", "CliHelpFinishDocument", "ProfileDocument",
        "OrchestrationDefaultsDocument",
    )
    missing = [
        f"{model_name}.{field_name}"
        for model_name in model_names
        for field_name, field in getattr(module, model_name).model_fields.items()
        if not field.description
    ]
    if missing:
        raise AssertionError(f"persisted model fields without descriptions: {missing}")


def test_yaml_task_store_round_trip_and_atomic_mutations() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "task.yaml"
        store = module.YamlTaskStore(module.YamlTaskLayout(task_path))
        store.replace(module.example_task_mapping(title="YAML round trip"))
        task_path.chmod(0o600)
        store.init_orchestration()
        store.claim_slice(owner="orchestrator", slice_key="do-the-change", now=_now_iso())
        store.add_step(
            slice_key="do-the-change", key="custom", title="Custom", note="evidence",
            terminal=True, after=None,
        )
        store.set_worktree(slice_key="do-the-change", repo="repo-a", path="/tmp/worktree")
        store.add_pr(
            slice_key="do-the-change", repo="repo-a", url="https://example.com/pr/1",
            status="open", note="awaiting review",
        )
        store.write_artifact("test-case-table", status="planned", path="plans/tests.md", note="selected")

        task = store.read()
        task_slice = task["plan"]["slices"][0]
        assert task["title"] == "YAML round trip"
        assert task["orchestration"]["cursors"][0]["slice"] == "do-the-change"
        assert task_slice["final_steps"][0]["key"] == "custom"
        assert task_slice["repo_work"]["repo-a"]["worktree"] == "/tmp/worktree"
        assert task_slice["repo_work"]["repo-a"]["prs"][0]["status"] == "open"
        assert task["orchestration"]["artifacts"]["test-case-table"]["path"] == "plans/tests.md"
        text = task_path.read_text()
        assert text.startswith("# Managed by pi-job.")
        assert task_path.stat().st_mode & 0o777 == 0o600
        if list(task_path.parent.glob(f".{task_path.name}.tmp-*")):
            raise AssertionError("atomic YAML mutation left a temporary file behind")


def test_yaml_mutations_serialize_concurrent_writers() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "concurrent.yaml"
        module.YamlTaskStore(module.YamlTaskLayout(task_path)).replace(module.example_task_mapping(title="Concurrent"))

        def add_decision(index: int) -> None:
            module.YamlTaskStore(module.YamlTaskLayout(task_path)).add_decision(
                date="2026-07-22",
                note=f"decision-{index}",
                source="concurrency-test",
            )

        with ThreadPoolExecutor(max_workers=8) as executor:
            list(executor.map(add_decision, range(24)))

        notes = {decision["note"] for decision in module.YamlTaskStore(module.YamlTaskLayout(task_path)).read()["decisions"]}
        assert notes == {f"decision-{index}" for index in range(24)}


def test_yaml_task_lock_lives_under_xdg_cache_not_task_dir() -> None:
    """Advisory lock files must not sit beside the task YAML in the project tree."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        tmp_path = Path(tmp)
        cache_home = tmp_path / "cache"
        task_dir = tmp_path / "tasks"
        task_dir.mkdir()
        task_path = task_dir / "demo.yaml"
        os.environ["XDG_CACHE_HOME"] = str(cache_home)
        try:
            expected_lock = module.yaml_task_lock_path(task_path)
            assert expected_lock.is_relative_to(cache_home / "pi-job" / "locks")
            store = module.YamlTaskStore(module.YamlTaskLayout(task_path))
            store.replace(module.example_task_mapping(title="Cache lock"))
            store.add_decision(date="2026-07-30", note="touch lock", source="lock-path-test")
            assert expected_lock.is_file(), f"expected lock at {expected_lock}"
            assert expected_lock.stat().st_size == 0
            sibling = task_dir / f".{task_path.name}.lock"
            if sibling.exists():
                raise AssertionError(f"sibling lock must not be created: {sibling}")
            assert list(task_dir.glob(".*.lock")) == []
            assert list(task_dir.glob("*.lock")) == []
        finally:
            os.environ.pop("XDG_CACHE_HOME", None)


def test_yaml_task_lock_path_resolves_aliases_to_same_inode_key() -> None:
    """Different path names for the same file must share one lock key."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        tmp_path = Path(tmp)
        cache_home = tmp_path / "cache"
        os.environ["XDG_CACHE_HOME"] = str(cache_home)
        try:
            real = tmp_path / "real" / "task.yaml"
            real.parent.mkdir(parents=True)
            real.write_text("title: x\n", encoding="utf-8")
            alias_dir = tmp_path / "alias"
            alias_dir.mkdir()
            alias = alias_dir / "task.yaml"
            alias.symlink_to(real)
            assert module.yaml_task_lock_path(real) == module.yaml_task_lock_path(alias)
            assert module.yaml_task_lock_path(real) == module.yaml_task_lock_path(real.parent / ".." / "real" / "task.yaml")
        finally:
            os.environ.pop("XDG_CACHE_HOME", None)


def test_yaml_lifecycle_lock_preserves_first_executor() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "lifecycle.yaml"
        store = module.YamlTaskStore(module.YamlTaskLayout(task_path))
        store.replace(module.example_task_mapping(title="Concurrent lifecycle"))
        store.init_orchestration()
        store.claim_slice(owner=DEFAULT_OWNER, slice_key="do-the-change", now=_now_iso())
        barrier = threading.Barrier(2)

        def start(model: str) -> subprocess.CompletedProcess[str]:
            barrier.wait()
            return run(
                str(PI_JOB), "--task", str(task_path), "start", "--model", model,
                check=False,
            )

        with ThreadPoolExecutor(max_workers=2) as executor:
            results = list(executor.map(start, ("provider/one", "provider/two")))

        assert sorted(result.returncode for result in results) == [0, 1]
        rejected = next(result for result in results if result.returncode != 0)
        assert_contains(rejected.stderr, "execution already belongs to model")
        execution = store.read()["plan"]["slices"][0]["steps"][0]["execution"]
        assert execution["model"] in {"provider/one", "provider/two"}


def test_yaml_lock_serializes_concurrent_finish_and_release() -> None:
    """The exclusive lock must serialize concurrent mutations of different orchestration
    fields (plan.slices via finish, orchestration.cursors via release) so neither is lost."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "finish-release-race.yaml"
        store = module.YamlTaskStore(module.YamlTaskLayout(task_path))
        store.replace(module.example_task_mapping(title="Finish/release race"))
        store.init_orchestration()
        store.claim_slice(owner=DEFAULT_OWNER, slice_key="do-the-change", now=_now_iso())
        run(
            str(PI_JOB), "--task", str(task_path), "start", "--model", "provider/writer"
        )
        barrier = threading.Barrier(2)

        def finish() -> subprocess.CompletedProcess[str]:
            barrier.wait()
            return run(
                str(PI_JOB), "--task", str(task_path), "finish",
                "--slice", "do-the-change", "--step", "create-plan", "--note", "completed",
                check=False,
            )

        def release() -> subprocess.CompletedProcess[str]:
            barrier.wait()
            return run(
                str(PI_JOB), "--task", str(task_path), "release", "--owner", DEFAULT_OWNER,
                check=False,
            )

        with ThreadPoolExecutor(max_workers=2) as executor:
            finish_future = executor.submit(finish)
            release_future = executor.submit(release)
            finish_result = finish_future.result()
            release_result = release_future.result()

        assert finish_result.returncode == 0, finish_result.stderr
        assert release_result.returncode == 0, release_result.stderr
        task = store.read()
        step = task["plan"]["slices"][0]["steps"][0]
        assert step["status"] == "done"
        assert "completed" in step["note"]
        assert step["execution"]["ended"]
        assert task["orchestration"]["cursors"] == []


def test_yaml_rejects_duplicate_and_unknown_fields() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        duplicate = Path(tmp) / "duplicate.yaml"
        duplicate.write_text("title: one\ntitle: two\nstatus: planned\n")
        result = run(str(PI_JOB), "--task", str(duplicate), "validate", check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "duplicate key 'title'")

        unknown = Path(tmp) / "unknown.yaml"
        unknown.write_text("title: one\nstatus: planned\nunknown: value\n")
        result = run(str(PI_JOB), "--task", str(unknown), "validate", check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "Extra inputs are not permitted")

        cross_list_duplicate = Path(tmp) / "cross-list-duplicate.yaml"
        cross_list_duplicate.write_text(
            "title: duplicate step\n"
            "status: planned\n"
            "plan:\n"
            "  slices:\n"
            "    - key: one\n"
            "      kind: implement\n"
            "      title: One\n"
            "      goal: Test duplicates\n"
            "      status: planned\n"
            "      note: ''\n"
            "      steps: [{key: repeated, title: First, status: planned, note: ''}]\n"
            "      final_steps: [{key: repeated, title: Second, status: planned, note: ''}]\n"
        )
        result = run(str(PI_JOB), "--task", str(cross_list_duplicate), "validate", check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "repeats keys across steps and final_steps")


def test_profile_rejects_required_steps_absent_from_template() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    profile["slice_kinds"]["setup"]["required_steps"] = ["verify"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "requires steps absent from its creation template")
    else:
        raise AssertionError("profile accepted a required step absent from step_template")


def test_profile_requires_subagent_prompt_packet() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["instruction_packets"]["subagent_prompt"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "subagent_prompt")
    else:
        raise AssertionError("profile accepted instruction_packets without required subagent_prompt")


def test_profile_requires_task_record_discipline_packet() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["instruction_packets"]["task_record_discipline"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "task_record_discipline")
    else:
        raise AssertionError("profile accepted instruction_packets without required task_record_discipline")


def test_profile_requires_out_of_band_edit_warning_packet() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["instruction_packets"]["out_of_band_edit_warning"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "out_of_band_edit_warning")
    else:
        raise AssertionError("profile accepted instruction_packets without required out_of_band_edit_warning")


def test_profile_requires_next_action_packet() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["instruction_packets"]["next_action"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "next_action")
    else:
        raise AssertionError("profile accepted instruction_packets without required next_action")


def test_profile_requires_pick_next_slice_packet() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["instruction_packets"]["pick_next_slice"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "pick_next_slice")
    else:
        raise AssertionError("profile accepted instruction_packets without required pick_next_slice")


def test_warn_if_content_dirty_uses_profile_packet() -> None:
    module = load_pi_job_module()
    packets = module.load_profile_contract()["instruction_packets"]
    body = packets["out_of_band_edit_warning"]
    assert_contains(body, "{task_file}")
    assert_contains(body, "acknowledge-edit --reason")
    formatted = body.format(task_file="/tmp/example.yaml")
    assert_contains(formatted, "pi-job --task /tmp/example.yaml acknowledge-edit --reason")
    assert_not_contains(formatted, "{task_file}")


def test_profile_requires_sync_pipeline_instructions() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["sync_pipeline_instructions"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "sync_pipeline_instructions")
    else:
        raise AssertionError("profile accepted missing sync_pipeline_instructions")


def test_profile_requires_cli_help() -> None:
    module = load_pi_job_module()
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["cli_help"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "cli_help")
    else:
        raise AssertionError("profile accepted missing cli_help")







def test_lifecycle_records_model_and_timestamps() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "lifecycle.yaml"
        write_task_yaml(task, lifecycle_mapping())

        started = run(
            str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer"
        ).stdout
        assert_contains(started, "started: implementation/vulnerability-scan")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Ask the user whether to run this step")
        assert_contains(instruction, "anthropic/claude-writer")
        assert_contains(instruction, "google/gemini-reviewer")

        finished = run(
            str(PI_JOB), "--task", str(task), "finish", "--note", "No vulnerabilities found."
        ).stdout
        assert_contains(finished, "[done]")
        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "implementation", "vulnerability-scan")
        assert step["execution"]["model"] == "google/gemini-reviewer"
        assert step["execution"]["started"]
        assert step["execution"]["ended"]
        assert step["status"] == "done"


def test_finish_reconcile_succeeds_on_in_progress_without_start() -> None:
    """finish --reconcile can close an in_progress step that was never started via pi-job."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "reconcile.yaml"
        write_task_yaml(task, lifecycle_mapping(status="in_progress"))

        out = run(
            str(PI_JOB), "--task", str(task), "finish", "--reconcile",
            "--model", "google/gemini-reviewer",
            "--note", "Synced completion from external session.",
        ).stdout
        assert_contains(out, "[done]")

        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "implementation", "vulnerability-scan")
        assert step["execution"]["model"] == "google/gemini-reviewer"
        assert step["note"] == "Synced completion from external session."
        assert step["execution"]["ended"]


def test_finish_reconcile_refuses_planned_status() -> None:
    """Reconcile fails when the target is still planned."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "reconcile-planned.yaml"
        write_task_yaml(task, lifecycle_mapping())

        res = run(
            str(PI_JOB), "--task", str(task), "finish", "--reconcile",
            "--model", "google/gemini-reviewer",
            "--note", "Should not apply.",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("reconcile on planned step unexpectedly succeeded")
        assert_contains(res.stderr, "reconcile refused")
        assert_contains(res.stderr, "planned")


def test_finish_reconcile_refuses_done_status() -> None:
    """Reconcile fails when the target step is already done (explicit --step; derived
    position would skip past done steps to the slice)."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "reconcile-done.yaml"
        write_task_yaml(task, lifecycle_mapping(
            status="done",
            note="already",
            execution={"model": "google/gemini-reviewer", "started": "2026-07-01T10:05:00Z", "ended": "2026-07-01T10:10:00Z"},
        ))

        res = run(
            str(PI_JOB), "--task", str(task), "finish", "--reconcile",
            "--slice", "implementation", "--step", "vulnerability-scan",
            "--model", "google/gemini-reviewer",
            "--note", "Should not apply.",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("reconcile on done step unexpectedly succeeded")
        assert_contains(res.stderr, "reconcile refused")
        assert_contains(res.stderr, "done")


def test_finish_without_start_still_fails_without_reconcile() -> None:
    """Normal finish without prior start remains fail-closed."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "no-reconcile.yaml"
        write_task_yaml(task, lifecycle_mapping(status="in_progress"))

        res = run(
            str(PI_JOB), "--task", str(task), "finish",
            "--model", "google/gemini-reviewer",
            "--note", "Should fail.",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("finish without start unexpectedly succeeded")
        assert_contains(res.stderr, "work was not started")


def test_finish_reconcile_requires_note() -> None:
    """Reconcile without --note fails closed."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "reconcile-no-note.yaml"
        write_task_yaml(task, lifecycle_mapping(status="in_progress"))

        res = run(
            str(PI_JOB), "--task", str(task), "finish", "--reconcile",
            "--model", "google/gemini-reviewer",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("reconcile without --note unexpectedly succeeded")
        assert_contains(res.stderr, "--reconcile requires --note")


def test_finish_note_appends_with_blank_line() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-append.yaml"
        mapping = lifecycle_mapping(note="Existing evidence")
        write_task_yaml(task, mapping)
        run(str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer")
        run(
            str(PI_JOB), "--task", str(task), "finish",
            "--note", "No vulnerabilities found.",
        )
        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "implementation", "vulnerability-scan")
        assert step["note"] == "Existing evidence\n\nNo vulnerabilities found."


def test_finish_note_replace_overwrites() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-replace.yaml"
        mapping = lifecycle_mapping(note="Old evidence")
        write_task_yaml(task, mapping)
        run(str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer")
        run(
            str(PI_JOB), "--task", str(task), "finish",
            "--replace", "--note", "Replacement evidence.",
        )
        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "implementation", "vulnerability-scan")
        assert step["note"] == "Replacement evidence."


def test_finish_replace_requires_note() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-replace-no-note.yaml"
        write_task_yaml(task, lifecycle_mapping())
        run(str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer")
        res = run(str(PI_JOB), "--task", str(task), "finish", "--replace", check=False)
        assert res.returncode != 0
        assert_contains(res.stderr, "finish --replace requires --note")


def test_finish_note_append_with_slice_only() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-slice-append.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["note"] = "Slice baseline"
        write_task_yaml(task, mapping)
        run(
            str(PI_JOB), "--task", str(task), "finish", "--skip",
            "--model", "openai/gpt-orchestrator", "--reason", "Not required for this slice",
        )
        run(
            str(PI_JOB), "--task", str(task), "start", "--slice-only",
            "--model", "openai/gpt-orchestrator",
        )
        run(
            str(PI_JOB), "--task", str(task), "finish", "--slice-only",
            "--note", "Slice completion evidence.",
        )
        module = load_pi_job_module()
        task_slice = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert task_slice["note"] == "Slice baseline\n\nSlice completion evidence."


def test_finish_replace_refused_with_skip() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-replace-skip.yaml"
        write_task_yaml(task, lifecycle_mapping())
        res = run(
            str(PI_JOB), "--task", str(task), "finish", "--skip",
            "--replace", "--note", "Should fail.",
            "--model", "openai/gpt-orchestrator", "--reason", "Skip reason",
            check=False,
        )
        assert res.returncode != 0
        assert_contains(res.stderr, "finish --replace cannot be combined with --skip")


def test_finish_bare_refuses_when_multiple_unfinished_steps() -> None:
    """When ≥2 unfinished steps exist, bare finish (cursor defaults) fails closed."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-ambiguous.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        write_task_yaml(task, mapping)
        run(str(PI_JOB), "--task", str(task), "start", "--model", "openai/gpt-orchestrator")
        res = run(
            str(PI_JOB), "--task", str(task), "finish",
            "--note", "Should refuse - other unfinished steps exist.",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("bare finish with multiple unfinished steps unexpectedly succeeded")
        assert_contains(res.stderr, "finish target ambiguous")
        assert_contains(res.stderr, "--slice KEY --step KEY")


def test_finish_explicit_slice_step_ok_when_multiple_unfinished() -> None:
    """Explicit --slice/--step succeeds even when other unfinished steps remain."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-explicit.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        write_task_yaml(task, mapping)
        run(
            str(PI_JOB), "--task", str(task), "start",
            "--slice", "second-slice", "--step", "s2",
            "--model", "openai/gpt-orchestrator",
        )
        out = run(
            str(PI_JOB), "--task", str(task), "finish",
            "--slice", "second-slice", "--step", "s2",
            "--note", "Evidence on the named step.",
        ).stdout
        assert_contains(out, "finished: second-slice/s2")
        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "second-slice", "s2")
        assert step["status"] == "done"


def test_finish_bare_ok_when_exactly_one_unfinished_step() -> None:
    """Bare finish remains allowed when the task has only one unfinished step."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "finish-single.yaml"
        write_task_yaml(task, lifecycle_mapping())
        run(str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer")
        out = run(
            str(PI_JOB), "--task", str(task), "finish",
            "--note", "Only one unfinished step.",
        ).stdout
        assert_contains(out, "finished: implementation/vulnerability-scan")


def test_set_slice_updates_title_and_goal() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "set-slice.yaml"
        write_task_yaml(task, lifecycle_mapping())
        out = run(
            str(PI_JOB), "--task", str(task), "set-slice",
            "--key", "implementation", "--title", "New title", "--goal", "New goal",
        ).stdout
        assert_contains(out, "updated slice:")
        assert_contains(out, "title=New title")
        assert_contains(out, "goal=New goal")
        module = load_pi_job_module()
        task_slice = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert task_slice["title"] == "New title"
        assert task_slice["goal"] == "New goal"


def test_set_slice_requires_one_field() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "set-slice-no-fields.yaml"
        write_task_yaml(task, lifecycle_mapping())
        res = run(
            str(PI_JOB), "--task", str(task), "set-slice", "--key", "implementation",
            check=False,
        )
        assert res.returncode != 0
        assert_contains(res.stderr, "at least one of --title or --goal is required")


def test_set_slice_refuses_done_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "set-slice-done.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["status"] = "done"
        write_task_yaml(task, mapping)
        res = run(
            str(PI_JOB), "--task", str(task), "set-slice",
            "--key", "implementation", "--title", "Too late",
            check=False,
        )
        assert res.returncode != 0
        assert_contains(res.stderr, "cannot update completed slice")


def test_block_slice_sets_status_and_appends_note() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "block-slice.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["note"] = "Existing blocker context"
        write_task_yaml(task, mapping)
        run(
            str(PI_JOB), "--task", str(task), "block-slice",
            "--key", "implementation", "--reason", "Waiting on upstream API",
        )
        module = load_pi_job_module()
        task_slice = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert task_slice["status"] == "blocked"
        assert task_slice["note"] == "Existing blocker context\n\nWaiting on upstream API"
        run(
            str(PI_JOB), "--task", str(task), "block-slice",
            "--key", "implementation", "--reason", "Still blocked",
        )
        task_slice = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert task_slice["note"].endswith("Still blocked")
        assert "Waiting on upstream API" in task_slice["note"]


def test_block_slice_refuses_done() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "block-slice-done.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["status"] = "done"
        write_task_yaml(task, mapping)
        res = run(
            str(PI_JOB), "--task", str(task), "block-slice",
            "--key", "implementation", "--reason", "Too late",
            check=False,
        )
        assert res.returncode != 0
        assert_contains(res.stderr, "cannot block completed slice")


def test_unblock_slice_restores_planned() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unblock-slice.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["status"] = "blocked"
        mapping["plan"]["slices"][0]["note"] = "Blocker note"
        write_task_yaml(task, mapping)
        out = run(
            str(PI_JOB), "--task", str(task), "unblock-slice", "--key", "implementation",
        ).stdout
        assert_contains(out, "unblocked slice: implementation [planned]")
        module = load_pi_job_module()
        task_slice = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert task_slice["status"] == "planned"
        assert task_slice["note"] == "Blocker note"


def test_unblock_slice_refuses_non_blocked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unblock-slice-planned.yaml"
        write_task_yaml(task, lifecycle_mapping())
        res = run(
            str(PI_JOB), "--task", str(task), "unblock-slice", "--key", "implementation",
            check=False,
        )
        assert res.returncode != 0
        assert_contains(res.stderr, "slice is not blocked")


def test_start_refuses_blocked_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "start-blocked-slice.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["status"] = "blocked"
        write_task_yaml(task, mapping)
        res = run(
            str(PI_JOB), "--task", str(task), "start",
            "--model", "google/gemini-reviewer",
            check=False,
        )
        assert res.returncode != 0
        assert_contains(res.stderr, "slice is blocked")
        assert_contains(res.stderr, "unblock-slice")


def test_vulnerability_scan_rejects_writer_model() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "same-model.yaml"
        write_task_yaml(task, lifecycle_mapping())
        result = run(
            str(PI_JOB), "--task", str(task), "start", "--model", "anthropic/claude-writer",
            check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "must differ from edit-code model")


def test_vulnerability_scan_instruction_prefers_higher_reasoning_model() -> None:
    """Scan packets must recommend a stronger review model, not only a different ID."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "scan-instruction.yaml"
        write_task_yaml(task, lifecycle_mapping())
        instruction = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(instruction, "vulnerability-scan")
        assert_contains(instruction, "higher-reasoning")
        assert_contains(instruction, "Model recorded on edit-code")
        assert_contains(instruction, "anthropic/claude-writer")


def test_vulnerability_scan_rejects_unqualified_author_model() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unqualified-author.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["steps"][0]["execution"]["model"] = "claude-writer"
        write_task_yaml(task, mapping)
        result = run(
            str(PI_JOB), "--task", str(task), "start", "--model", "google/gemini-reviewer",
            check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "not fully qualified as provider/model")
        assert_contains(result.stderr, "openai/gpt-5.6-sol")


def test_start_unqualified_model_error_includes_example() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unqualified-start" / "task.yaml"
        # create seeds cursors:[]; claim before start so the model-id check is reached.
        run(str(PI_JOB), "--task", str(task), "create", "--kind", "implement", "--force", "--title", "Model id")
        run(str(PI_JOB), "--task", str(task), "claim", "--slice", "implement-slice", "--owner", DEFAULT_OWNER)
        res = run(
            str(PI_JOB), "--task", str(task), "start", "--model", "composer-2",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("unqualified start model unexpectedly succeeded")
        assert_contains(res.stderr, "fully qualified as provider/model")
        assert_contains(res.stderr, "openai/gpt-5.6-sol")


def test_finish_slice_only_rejects_malformed_scan_timestamps() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "malformed-time.yaml"
        write_task_yaml(task, lifecycle_mapping(
            status="done",
            note="No findings",
            execution={"model": "google/gemini-reviewer", "started": "not-a-time", "ended": "2026-07-01T10:10:00Z"},
        ))
        result = run(
            str(PI_JOB), "--task", str(task), "finish", "--slice-only",
            "--slice", "implementation",
            "--reconcile", "--note", "close slice",
            "--model", "google/gemini-reviewer", check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "timestamps must be valid UTC ISO 8601")


def test_vulnerability_scan_can_record_user_declined_skip() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "declined.yaml"
        write_task_yaml(task, lifecycle_mapping())
        run(
            str(PI_JOB), "--task", str(task), "finish", "--skip",
            "--model", "openai/gpt-orchestrator", "--reason", "Not required for this slice",
        )
        module = load_pi_job_module()
        step = find_step(module.YamlTaskStore(module.YamlTaskLayout(task)).read(), "implementation", "vulnerability-scan")
        assert step["status"] == "skipped"
        assert "User declined vulnerability-scan" in step["note"]
        assert step["execution"]["model"] == "openai/gpt-orchestrator"
        assert step["execution"]["ended"]


def test_slice_lifecycle_records_orchestrator_after_steps_finish() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "slice-lifecycle.yaml"
        write_task_yaml(task, lifecycle_mapping())
        run(
            str(PI_JOB), "--task", str(task), "finish", "--skip",
            "--model", "openai/gpt-orchestrator", "--reason", "Not required for this slice",
        )
        run(
            str(PI_JOB), "--task", str(task), "start", "--slice-only",
            "--model", "openai/gpt-orchestrator",
        )
        run(str(PI_JOB), "--task", str(task), "finish", "--slice-only")

        module = load_pi_job_module()
        task_slice = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert task_slice["status"] == "done"
        assert task_slice["execution"]["model"] == "openai/gpt-orchestrator"
        assert task_slice["execution"]["ended"].endswith("Z")


def test_slice_skip_cannot_bypass_policy_governed_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "slice-policy-bypass.yaml"
        write_task_yaml(task, lifecycle_mapping())
        result = run(
            str(PI_JOB), "--task", str(task), "finish", "--slice-only", "--skip",
            "--model", "openai/gpt-orchestrator", "--reason", "Skip whole slice",
            check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "vulnerability-scan is policy-governed")


def test_execution_issues_warn_for_legacy_completed_work() -> None:
    module = load_pi_job_module()
    issues = module.execution_issues({
        "plan": {
            "slices": [{
                "key": "implementation", "status": "in_progress",
                "steps": [{"key": "edit-code", "status": "done"}], "final_steps": [],
            }]
        }
    })
    if not any("edit-code" in issue and "no execution metadata" in issue for issue in issues):
        raise AssertionError(f"expected legacy execution warning, got: {issues}")


def test_lifecycle_policy_is_step_key_agnostic() -> None:
    module = load_pi_job_module()
    original = module.try_get_step_kind
    module.try_get_step_kind = lambda key: {
        "key": key,
        "requires_user_decision": True,
        "different_model_from_step": "generate-assets",
    } if key == "independent-audit" else original(key)
    try:
        task_slice = module.TaskSlice.from_mapping({
            "key": "custom",
            "steps": [
                {
                    "key": "generate-assets",
                    "execution": {"model": "provider/writer"},
                },
                {"key": "independent-audit"},
            ],
            "final_steps": [],
        })
        audit = task_slice.find_step("independent-audit")
        assert audit is not None
        assert isinstance(task_slice, module.TaskSlice)
        assert isinstance(task_slice.find_step("generate-assets").execution, module.ExecutionRecord)
        same_model = module.step_policy_issue(
            task_slice, audit, model="provider/writer", status="in_progress", note=""
        )
        assert "independent-audit model must differ from generate-assets model" in same_model
        declined = module.step_policy_issue(
            task_slice, audit, model="provider/reviewer", status="skipped", note="ordinary skip"
        )
        assert "explicit user-declined reason" in declined
    finally:
        module.try_get_step_kind = original


def test_scaffold_empty_plan_has_no_slices() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty" / "task.yaml"
        out = run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--dry-run").stdout
        assert "slices: []" in out
        assert "do-the-change" not in out


def test_scaffold_initial_kind_setup_seeds_setup_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "initial-kind" / "task.yaml"
        out = run(str(PI_JOB), "--task", str(task), "create", "--kind", "setup", "--dry-run").stdout
        assert "setup-slice" in out
        assert "explore-context" in out
        assert "do-the-change" not in out


def test_bootstrap_creates_initialized_task() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("""
title: Bootstrap test
initial_slice_kind: setup
source:
  discovered: "2026-07-27"
  context: A test task.
project:
  key: test-project
  name: Test Project
  route: test.html
  context: Test context.
context: Background context.
decisions:
  - date: "2026-07-27"
    note: Keep the URL.
    source: chat:2026-07-27
slices:
  - key: implement-one
    kind: implement
    title: Implement one
    goal: Ship the change.
    depends_on:
      - task-setup
""", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input))
        assert_contains(result.stdout, "created:")
        assert_contains(result.stdout, "claim \u2192 <none>")
        assert_contains(result.stdout, "pi-job claim --slice task-setup --owner ID")
        assert task.exists()
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: ok")
        assert_contains(status, "Cursors: <none>")
        assert_contains(status, "Ready: task-setup")
        validate = run(str(PI_JOB), "--task", str(task), "validate").stdout
        assert_contains(validate, "ok:")


def test_bootstrap_dry_run_prints_diff_and_does_not_write() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-dry" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Dry run test\nslices:\n  - key: only-slice\n    kind: implement\n    title: Only\n    goal: Test\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        out = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), "--dry-run").stdout
        assert "+++" in out
        assert "PI-JOB EXECUTION INSTRUCTION" not in out
        assert not task.exists()


def test_bootstrap_refuses_overwrite_without_force() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-exists" / "task.yaml"
        task.parent.mkdir(parents=True, exist_ok=True)
        task.write_text("title: existing\nstatus: planned\n", encoding="utf-8")
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Bootstrap test\nslices:\n  - key: only-slice\n    kind: implement\n    title: Only\n    goal: Test\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "already exists")


def test_bootstrap_rejects_unknown_kind() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-unknown-kind" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Bad kind\nslices:\n  - key: slice\n    kind: nonexistent\n    title: Bad\n    goal: Fail\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "unknown slice kind")


def test_bootstrap_rejects_unresolved_dependency() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-bad-dep" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Bad dep\nslices:\n  - key: slice\n    kind: implement\n    title: Slice\n    goal: Test\n    depends_on: [missing]\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "unresolved dependency")


def test_profile_show_json() -> None:
    result = run(str(PI_JOB), "profile", "--json")
    assert_contains(result.stdout, "config_layering")
    assert_contains(result.stdout, "slice_kinds")
    assert_contains(result.stdout, "step_kinds")


def test_schema_show_json() -> None:
    result = run(str(PI_JOB), "schema", "--json")
    assert_contains(result.stdout, "task")
    assert_contains(result.stdout, "create")


def test_kinds_list_json() -> None:
    result = run(str(PI_JOB), "kinds", "list", "--json")
    assert_contains(result.stdout, "setup")
    assert_contains(result.stdout, "implement")


def test_kinds_show_json() -> None:
    result = run(str(PI_JOB), "kinds", "show", "implement", "--json")
    assert_contains(result.stdout, "step_template")
    assert_contains(result.stdout, "create-plan")


FOLLOW_WORK_TEMPLATE_STEPS = [
    "clarify-scope",
    "collect-references",
    "wait-for-landing",
    "review-capture",
    "follow-up-gate",
    "update-task-file",
    "pi-job-feedback",
]

FOLLOW_WORK_REQUIRED_STEPS = [
    "clarify-scope",
    "collect-references",
    "wait-for-landing",
    "review-capture",
    "follow-up-gate",
    "update-task-file",
]


def test_follow_work_kind_list_and_show() -> None:
    """kinds list/show includes follow-work with the contract template and required_steps."""
    listed = run(str(PI_JOB), "kinds", "list", "--json").stdout
    assert_contains(listed, "follow-work")

    shown = run(str(PI_JOB), "kinds", "show", "follow-work", "--json").stdout
    assert_contains(shown, "no_code_changes")
    for step_key in FOLLOW_WORK_TEMPLATE_STEPS:
        assert_contains(shown, step_key)
    for step_key in FOLLOW_WORK_REQUIRED_STEPS:
        assert_contains(shown, step_key)
    assert "pi-job-feedback" in shown
    assert '"required_steps"' in shown

    module = load_pi_job_module()
    kind = module.get_slice_kind("follow-work")
    assert kind.get("policies", {}).get("no_code_changes") is True
    assert [str(k) for k in (kind.get("step_template") or [])] == FOLLOW_WORK_TEMPLATE_STEPS
    assert [str(k) for k in (kind.get("required_steps") or [])] == FOLLOW_WORK_REQUIRED_STEPS
    template_titles = [key for key, _title in module.steps_from_kind_template("follow-work")]
    assert template_titles == FOLLOW_WORK_TEMPLATE_STEPS


def test_add_slice_follow_work_seeds_template_steps() -> None:
    """add-slice --kind follow-work seeds every step from the slice kind template."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "follow-work-add-slice.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        dry = run(
            str(PI_JOB), "--task", str(task), "add-slice",
            "--key", "observe-peer",
            "--title", "Observe peer delivery",
            "--goal", "Capture understanding after their ticket lands.",
            "--kind", "follow-work",
            "--dry-run",
        ).stdout
        for step_key in FOLLOW_WORK_TEMPLATE_STEPS:
            assert_contains(dry, step_key)

        run(
            str(PI_JOB), "--task", str(task), "add-slice",
            "--key", "observe-peer",
            "--title", "Observe peer delivery",
            "--goal", "Capture understanding after their ticket lands.",
            "--kind", "follow-work",
        )

        module = load_pi_job_module()
        store = module.YamlTaskStore(module.YamlTaskLayout(task))
        task_data = store.read()
        slice_entry = next(sl for sl in task_data["plan"]["slices"] if sl["key"] == "observe-peer")
        assert slice_entry["kind"] == "follow-work"
        present = [step["key"] for step in (slice_entry.get("steps") or []) + (slice_entry.get("final_steps") or [])]
        assert present == FOLLOW_WORK_TEMPLATE_STEPS


def _follow_work_conformant_slice_yaml() -> dict:
    steps = [
        {"key": key, "title": key.replace("-", " ").title(), "status": "planned", "note": ""}
        for key in FOLLOW_WORK_TEMPLATE_STEPS
    ]
    return {
        "key": "follow-peer",
        "kind": "follow-work",
        "title": "Follow peer ticket",
        "goal": "Observe until landing and capture understanding.",
        "status": "planned",
        "note": "",
        "steps": steps,
        "final_steps": [],
    }


def test_validate_accepts_conformant_follow_work_fixture() -> None:
    """validate accepts a follow-work slice that satisfies required_steps."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "follow-work-validate.yaml"
        write_task_yaml(task, structure_task_yaml(_follow_work_conformant_slice_yaml()))
        out = run(str(PI_JOB), "--task", str(task), "validate").stdout
        assert_contains(out, "ok:")


def test_set_project_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "project-mutation" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "set-project", "--key", "new-key", "--name", "New Name")
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        assert task_data["project"]["key"] == "new-key"
        assert task_data["project"]["name"] == "New Name"


def test_set_project_title_updates_task_title() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "project-title" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force", "--title", "Old title")
        out = run(
            str(PI_JOB), "--task", str(task), "set-project", "--title", "Widened scope title",
        ).stdout
        assert_contains(out, "title=Widened scope title")
        module = load_pi_job_module()
        assert module.open_task_store(task).read()["title"] == "Widened scope title"
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Widened scope title")


def test_set_project_title_refuses_empty() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "project-title-empty" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        res = run(str(PI_JOB), "--task", str(task), "set-project", "--title", "   ", check=False)
        if res.returncode == 0:
            raise AssertionError("empty set-project --title unexpectedly succeeded")
        assert_contains(res.stderr, "title must be non-empty")


def test_set_context_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "context-mutation" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "set-context", "--context", "New context")
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        assert task_data["context"] == "New context"


def test_add_decision_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "decision-mutation" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "add-decision", "--date", "2026-07-27", "--note", "Test decision", "--source", "test")
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        assert len(task_data["decisions"]) == 1
        assert task_data["decisions"][0]["note"] == "Test decision"


def test_set_plan_note_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "plan-note-mutation" / "task.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "set-plan-note", "--note", "Plan note text")
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        assert task_data["plan"]["note"] == "Plan note text"


def test_remove_slice_removes_and_guards() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "remove-slice" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Remove test\ninitial_slice_kind: setup\nslices:\n  - key: dependent\n    kind: implement\n    title: Dependent\n    goal: Test\n    depends_on: [task-setup]\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input))
        # Refuse to remove a slice that others depend on
        result = run(str(PI_JOB), "--task", str(task), "remove-slice", "--key", "task-setup", check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "other slices depend on it")
        # Removing a slice with no dependents works
        result = run(str(PI_JOB), "--task", str(task), "remove-slice", "--key", "dependent")
        assert result.returncode == 0
        # Verify it's gone
        module = load_pi_job_module()
        store = module.open_task_store(task)
        task_data = store.read()
        assert len(task_data["plan"]["slices"]) == 1




def test_create_from_requires_intent_path() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.yaml"
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(Path(tmp) / "missing-intent.yaml"), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "create intent not found")


def test_bootstrap_prints_seed_slice_plans_for_implement_not_setup() -> None:
    """After bootstrap, stdout lists qualifying implement slices in the seed block, not setup."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-seed" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(
            minimal_bootstrap_input_yaml(
                slices_yaml="""
slices:
  - key: implement-one
    kind: implement
    title: Implement one
    goal: Ship the change.
    depends_on:
      - task-setup
""",
            ),
            encoding="utf-8",
        )
        out = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input)).stdout
        seed = seed_block_after_marker(out)
        assert_contains(seed, "plans/implement-one.md")
        assert_contains(seed, "Depends on: task-setup")
        if "- task-setup [" in seed:
            raise AssertionError(f"setup slice must not appear as a seed entry:\n{seed}")
        assert "task-setup.plans/" not in seed


def test_add_slice_implement_prints_seed_block_for_new_slice_only() -> None:
    """add-slice for a qualifying kind prints the seed block for that slice only."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice-seed" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(minimal_bootstrap_input_yaml(), encoding="utf-8")
        run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input))
        out = run(
            str(PI_JOB),
            "--task",
            str(task),
            "add-slice",
            "--key",
            "wire-api",
            "--title",
            "Wire API",
            "--goal",
            "Connect the endpoints.",
            "--kind",
            "implement",
        ).stdout
        seed = seed_block_after_marker(out)
        assert_contains(seed, "plans/wire-api.md")
        if "- task-setup [" in seed:
            raise AssertionError(f"seed block must list only the new slice:\n{seed}")
        assert "task-setup.plans/" not in seed


def test_add_slice_setup_prints_no_seed_block() -> None:
    """add-slice for setup (no create-plan in template) must not print a seed block."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice-no-seed" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(minimal_bootstrap_input_yaml(), encoding="utf-8")
        run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input))
        out = run(
            str(PI_JOB),
            "--task",
            str(task),
            "add-slice",
            "--key",
            "extra-setup",
            "--title",
            "Extra setup",
            "--goal",
            "More exploration.",
            "--kind",
            "setup",
        ).stdout
        if "SEED SLICE PLAN FILES NOW" in out:
            raise AssertionError(f"setup add-slice must not print seed block:\n{out}")


def test_bootstrap_dry_run_prints_no_seed_block() -> None:
    """bootstrap --dry-run must not print the seed block."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-dry-seed" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(
            minimal_bootstrap_input_yaml(
                slices_yaml="""
slices:
  - key: only-implement
    kind: implement
    title: Only implement
    goal: Test dry-run seed omission.
    depends_on:
      - task-setup
""",
            ),
            encoding="utf-8",
        )
        out = run(
            str(PI_JOB),
            "--task",
            str(task),
            "create",
            "--from",
            str(bootstrap_input),
            "--dry-run",
        ).stdout
        if "SEED SLICE PLAN FILES NOW" in out:
            raise AssertionError(f"create dry-run must not print seed block:\n{out}")


def test_seed_block_uses_task_placeholder_not_absolute_path() -> None:
    """Seed block plan paths are relative; may use <TASK>; must not repeat absolute task path."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "my-task" / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(
            minimal_bootstrap_input_yaml(
                slices_yaml="""
slices:
  - key: feature-a
    kind: implement
    title: Feature A
    goal: Build feature A.
    depends_on:
      - task-setup
""",
            ),
            encoding="utf-8",
        )
        out = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input)).stdout
        seed = seed_block_after_marker(out)
        assert_contains(seed, "plans/feature-a.md")
        absolute_task = str(task.resolve())
        if absolute_task in seed:
            raise AssertionError(
                f"seed block must not contain absolute task path {absolute_task!r}:\n{seed}"
            )


def test_pi_job_write_stores_content_digest() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "digest-write.yaml"
        write_task_yaml(task_path, standard_fixture_mapping())
        run(str(PI_JOB), "--task", str(task_path), "set-plan-note", "--note", "machine-owned")
        task = module.YamlTaskStore(module.YamlTaskLayout(task_path)).read()
        digest = task["orchestration"]["content_digest"]
        if not digest:
            raise AssertionError("expected content_digest after pi-job write")
        if digest != module.compute_content_digest(task):
            raise AssertionError("stored digest does not match computed digest")


def test_hand_edit_warns_on_next_read() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "hand-edit.yaml"
        write_task_yaml(task_path, standard_fixture_mapping())
        run(str(PI_JOB), "--task", str(task_path), "set-plan-note", "--note", "before hand edit")
        raw = yaml.safe_load(task_path.read_text())
        raw["context"] = "edited outside pi-job"
        task_path.write_text(yaml.safe_dump(raw, sort_keys=False, allow_unicode=True), encoding="utf-8")
        status = run(str(PI_JOB), "--task", str(task_path), "status", check=False)
        assert_contains(status.stderr, "does not match the last pi-job write digest")
        assert_contains(status.stderr, "acknowledge-edit --reason")


def test_acknowledge_edit_clears_warning() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "ack-edit.yaml"
        write_task_yaml(task_path, standard_fixture_mapping(cursor=("second-slice", "s2")))
        run(str(PI_JOB), "--task", str(task_path), "set-plan-note", "--note", "before hand edit")
        raw = yaml.safe_load(task_path.read_text())
        raw["context"] = "edited outside pi-job"
        task_path.write_text(yaml.safe_dump(raw, sort_keys=False, allow_unicode=True), encoding="utf-8")
        run(
            str(PI_JOB), "--task", str(task_path), "acknowledge-edit",
            "--reason", "fixed context typo by hand",
        )
        status = run(str(PI_JOB), "--task", str(task_path), "status")
        assert_not_contains(status.stderr, "does not match the last pi-job write digest")
        task = module.YamlTaskStore(module.YamlTaskLayout(task_path)).read()
        note = next(s["note"] for s in task["plan"]["slices"] if s["key"] == "second-slice")
        assert_contains(note, "Hand-edit acknowledged: fixed context typo by hand")
        # Must not pollute the decisions channel.
        for decision in task.get("decisions") or []:
            if "acknowledge-edit" in str(decision.get("source") or "") or "Hand-edit" in str(
                decision.get("note") or ""
            ):
                raise AssertionError(f"acknowledge-edit must not write a decision: {decision}")


def test_finish_while_dirty_does_not_clear_digest() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "finish-dirty.yaml"
        mapping = standard_fixture_mapping(cursor=("second-slice", "s2"))
        write_task_yaml(task_path, mapping)
        run(str(PI_JOB), "--task", str(task_path), "set-plan-note", "--note", "baseline")
        task = module.YamlTaskStore(module.YamlTaskLayout(task_path)).read()
        digest_before = task["orchestration"]["content_digest"]
        raw = yaml.safe_load(task_path.read_text())
        raw["context"] = "edited outside pi-job"
        task_path.write_text(yaml.safe_dump(raw, sort_keys=False, allow_unicode=True), encoding="utf-8")
        run(
            str(PI_JOB), "--task", str(task_path), "start", "--model", "provider/test",
        )
        # Fixture has two unfinished steps (s2 + final finish); bare finish is ambiguous.
        run(
            str(PI_JOB),
            "--task",
            str(task_path),
            "finish",
            "--slice",
            "second-slice",
            "--step",
            "s2",
            "--note",
            "finished while dirty",
        )
        task_after = module.YamlTaskStore(module.YamlTaskLayout(task_path)).read()
        digest_after = task_after["orchestration"]["content_digest"]
        if digest_after != digest_before:
            raise AssertionError("finish must not refresh digest while dirty")
        status = run(str(PI_JOB), "--task", str(task_path), "status", check=False)
        assert_contains(status.stderr, "does not match the last pi-job write digest")


def test_missing_digest_does_not_warn() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "legacy-no-digest.yaml"
        write_task_yaml(task_path, standard_fixture_mapping())
        status = run(str(PI_JOB), "--task", str(task_path), "status")
        assert_not_contains(status.stderr, "does not match the last pi-job write digest")


def test_bootstrap_rejects_initial_slice_key_without_kind() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Fail\ninitial_slice_key: my-key\nslices:\n  - key: slice\n    kind: implement\n    title: Slice\n    goal: Fail\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "initial_slice_key requires initial_slice_kind")
    module = load_pi_job_module()
    original = module.try_get_step_kind
    module.try_get_step_kind = lambda key: {
        "key": key,
        "requires_user_decision": True,
        "different_model_from_step": "generate-assets",
    } if key == "independent-audit" else original(key)
    try:
        task_slice = module.TaskSlice.from_mapping({
            "key": "custom",
            "steps": [
                {
                    "key": "generate-assets",
                    "execution": {"model": "provider/writer"},
                },
                {"key": "independent-audit"},
            ],
            "final_steps": [],
        })
        audit = task_slice.find_step("independent-audit")
        assert audit is not None
        assert isinstance(task_slice, module.TaskSlice)
        assert isinstance(task_slice.find_step("generate-assets").execution, module.ExecutionRecord)
        same_model = module.step_policy_issue(
            task_slice, audit, model="provider/writer", status="in_progress", note=""
        )
        assert "independent-audit model must differ from generate-assets model" in same_model
        declined = module.step_policy_issue(
            task_slice, audit, model="provider/reviewer", status="skipped", note="ordinary skip"
        )
        assert "explicit user-declined reason" in declined
    finally:
        module.try_get_step_kind = original


def _markdown_representative_mapping(*, chronological_cursor: tuple[str, str] = ("active-slice", "edit-code")) -> dict:
    return {
        "title": "Markdown representative task",
        "status": "in_progress",
        "context": "Shared background for the preview.",
        "source": {
            "jira": "PROJ-123",
            "discovered": "2026-08-01",
            "context": "Found during harness work.",
        },
        "project": {
            "key": "pi-job-harness",
            "name": "pi-job harness",
            "route": "dot_local/share/pi-job-harness",
            "context": "Chezmoi-managed harness copy.",
        },
        "decisions": [
            {
                "date": "2026-08-03",
                "note": "Prefer **Markdown** in decision notes.",
                "source": "chat:2026-08-03",
            },
            {
                "date": "2026-08-02",
                "note": "Decisions stay near the top.",
                "source": "review",
            },
        ],
        "orchestration": {
            "cursors": [claim_dict(chronological_cursor[0])],
            "policy": _orchestration_policy(),
            "artifacts": {
                "test-case-table": {
                    "status": "done",
                    "path": "plans/test-case-table.md",
                    "note": "Coverage matrix for preview journeys.",
                },
            },
        },
        "plan": {
            "note": "High-level plan context for the representative fixture.",
            "slices": [
                {
                    "key": "done-slice",
                    "kind": "implement",
                    "title": "Already done",
                    "goal": "Finished earlier",
                    "status": "done",
                    "note": "",
                    "execution": {
                        "model": "cursor/test",
                        "started": "2026-08-01T09:00:00Z",
                        "ended": "2026-08-01T10:00:00Z",
                    },
                    "steps": [
                        {
                            "key": "create-plan",
                            "title": "Create plan",
                            "status": "done",
                            "note": "",
                            "execution": {
                                "model": "cursor/test",
                                "started": "2026-08-01T09:00:00Z",
                                "ended": "2026-08-01T09:30:00Z",
                            },
                        },
                    ],
                    "final_steps": [],
                },
                {
                    "key": "active-slice",
                    "kind": "implement",
                    "title": "Active work",
                    "goal": "Exercise the preview renderer",
                    "status": "in_progress",
                    "note": "Slice note with evidence.",
                    "repos": ["graphius"],
                    "depends_on": ["done-slice"],
                    "repo_work": {
                        "graphius": {
                            "worktree": "/tmp/wt-markdown",
                            "prs": [
                                {
                                    "url": "https://github.com/example/pr/42",
                                    "status": "open",
                                    "note": "Needs review",
                                },
                            ],
                        },
                    },
                    "execution": {
                        "model": "cursor/composer",
                        "started": "2026-08-03T12:00:00Z",
                    },
                    "steps": [
                        {
                            "key": "create-plan",
                            "title": "Create plan",
                            "status": "done",
                            "note": "",
                            "execution": {
                                "model": "cursor/composer",
                                "started": "2026-08-03T10:00:00Z",
                                "ended": "2026-08-03T11:00:00Z",
                            },
                        },
                        {
                            "key": "edit-code",
                            "title": "Edit code",
                            "status": "in_progress",
                            "note": "Step note for the cursor step.",
                            "execution": {
                                "model": "cursor/composer",
                                "started": "2026-08-03T12:00:00Z",
                            },
                        },
                    ],
                    "final_steps": [
                        {"key": "verify", "title": "Verify", "status": "planned", "note": ""},
                    ],
                },
            ],
        },
    }


def test_markdown_representative_full_dump() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "representative.yaml"
        write_task_yaml(task, _markdown_representative_mapping())

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        decisions_idx = out.index("## Decisions")
        slices_idx = out.index("## Slices")
        assert decisions_idx < slices_idx, out
        assert_contains(out, "# Markdown representative task")
        assert_contains(out, "**Status:** in_progress")
        assert_contains(out, "## Project")
        assert_contains(out, "**key:** pi-job-harness")
        assert_contains(out, "- **2026-08-03** (chat:2026-08-03)")
        assert_contains(out, "> Prefer **Markdown** in decision notes.")
        assert_contains(out, "> Decisions stay near the top.")
        decisions_block = out.split("## Decisions", 1)[1].split("## Context", 1)[0]
        if "```" in decisions_block:
            raise AssertionError(f"decisions must be blockquotes, not fenced code:\n{out}")
        assert_contains(out, "## Context")
        assert_contains(out, "Shared background for the preview.")
        assert_contains(out, "## Source")
        assert_contains(out, "**jira:** PROJ-123")
        assert_contains(out, "## Plan note")
        assert_contains(out, "## Artifacts")
        assert_contains(out, "**test-case-table** [done]")
        assert_contains(out, "## Contents")
        assert_contains(out, "[active-slice (current) —")
        assert_contains(out, "](#slice-active-slice)")
        contents_idx = out.index("## Contents")
        assert contents_idx < slices_idx, out
        assert_contains(out, '<a id="slice-active-slice"></a>')
        assert_contains(out, "### active-slice (current)")
        assert_contains(out, "**edit-code** (current)")
        assert_contains(out, "**Repo work (graphius):**")
        assert_contains(out, "https://github.com/example/pr/42")
        assert_not_contains(out, "PI-JOB")
        assert_not_contains(out, "\033[")


def test_markdown_contents_lists_all_slices_in_order() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "toc.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        assert_contains(out, "## Contents")
        first_toc = out.index("[first —")
        second_toc = out.index("[second-slice —")
        contents_end = out.index("## Slices")
        assert first_toc < second_toc < contents_end, out
        chrono = run(str(PI_JOB), "--task", str(task), "markdown", "--chronological").stdout
        # Fixture first slice is done without timestamps; still listed in Contents.
        assert_contains(chrono, "## Contents")
        assert_contains(chrono, "](#slice-first)")
        assert_contains(chrono, "](#slice-second-slice)")


def test_markdown_minimal_omits_empty_decisions_none() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "minimal.yaml"
        write_task_yaml(task, {
            "title": "Minimal preview",
            "status": "planned",
            "plan": {
                "note": "",
                "slices": [{
                    "key": "only",
                    "kind": "implement",
                    "title": "Only slice",
                    "goal": "Do one thing",
                    "status": "planned",
                    "note": "",
                    "steps": [{"key": "create-plan", "title": "Create plan", "status": "planned", "note": ""}],
                    "final_steps": [],
                }],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        assert_contains(out, "## Decisions")
        assert_contains(out, "_none_")
        for forbidden in ("## Context", "## Source", "## Plan note", "## Artifacts", "**Repos:**", "#### Final steps"):
            assert_not_contains(out, forbidden)


def test_markdown_uninitialized_preview() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "uninitialized.yaml"
        write_task_yaml(
            task,
            standard_fixture_mapping(title="Uninitialized markdown preview", uninitialized=True),
        )

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        assert_contains(out, "# Uninitialized markdown preview")
        assert_contains(out, "## Slices")
        assert_not_contains(out, "(current)")


def test_markdown_escapes_and_quotes_metacharacters() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "escape.yaml"
        write_task_yaml(task, {
            "title": "# Not a heading",
            "status": "in_progress",
            "context": "## nested heading\n**bold context**",
            "decisions": [{
                "date": "2026-08-03",
                "note": "Use **bold** in decisions.\nSecond line.",
                "source": "test",
            }],
            "plan": {
                "note": "",
                "slices": [{
                    "key": "meta-slice",
                    "kind": "implement",
                    "title": "## Slice title",
                    "goal": "Keep structure intact",
                    "status": "in_progress",
                    "note": "# note heading\nwith **emphasis**",
                    "steps": [{
                        "key": "create-plan",
                        "title": "Create plan",
                        "status": "planned",
                        "note": "",
                    }],
                    "final_steps": [],
                }],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        assert_contains(out, "# \\# Not a heading")
        assert_contains(out, "### meta-slice [implement] — \\#\\# Slice title")
        assert_contains(out, "## nested heading")
        assert_contains(out, "**bold context**")
        assert_contains(out, "> Use **bold** in decisions.")
        assert_contains(out, "> Second line.")
        assert_contains(out, "> # note heading")
        assert_contains(out, "> with **emphasis**")
        context_block = out.split("## Context", 1)[1].split("## Slices", 1)[0]
        if context_block.lstrip().startswith("```"):
            raise AssertionError(f"context must be Markdown prose, not fenced:\n{out}")
        decisions_block = out.split("## Decisions", 1)[1].split("## Context", 1)[0]
        if "```" in decisions_block:
            raise AssertionError(f"decisions must be blockquotes, not fenced:\n{out}")


def test_markdown_current_badges() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "current.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        assert_contains(out, "### second-slice (current)")
        assert_contains(out, "**s2** (current)")
        assert_contains(out, "[second-slice (current) — Second](#slice-second-slice)")
        # Exactly three (current) markers: Contents entry, slice heading, and step.
        assert out.count(" (current)") == 3, out
        assert_not_contains(out, "### first (current)")
        assert_not_contains(out, "**s1** (current)")
        assert_not_contains(out, "**finish** (current)")


def test_markdown_default_slice_order() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "order.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        out = run(str(PI_JOB), "--task", str(task), "markdown").stdout
        first_idx = out.index("### first")
        second_idx = out.index("### second-slice")
        if first_idx > second_idx:
            raise AssertionError(f"expected plan order (first before second-slice):\n{out}")


def test_markdown_chronological_sort() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "chrono.yaml"
        write_task_yaml(task, {
            "title": "Chronological sort",
            "status": "in_progress",
            "orchestration": {
                "cursors": [claim_dict("middle")],
                "policy": _orchestration_policy(),
            },
            "plan": {
                "note": "",
                "slices": [
                    {
                        "key": "newest",
                        "kind": "implement",
                        "title": "Newest change",
                        "goal": "Latest timestamp",
                        "status": "done",
                        "note": "",
                        "execution": {
                            "model": "cursor/test",
                            "started": "2026-08-03T12:00:00Z",
                            "ended": "2026-08-03T13:00:00Z",
                        },
                        "steps": [],
                        "final_steps": [],
                    },
                    {
                        "key": "oldest",
                        "kind": "implement",
                        "title": "Oldest change",
                        "goal": "Earliest timestamp",
                        "status": "done",
                        "note": "",
                        "steps": [{
                            "key": "create-plan",
                            "title": "Create plan",
                            "status": "done",
                            "note": "",
                            "execution": {
                                "model": "cursor/test",
                                "started": "2026-08-01T08:00:00Z",
                                "ended": "2026-08-01T09:00:00Z",
                            },
                        }],
                        "final_steps": [],
                    },
                    {
                        "key": "middle",
                        "kind": "implement",
                        "title": "Middle change",
                        "goal": "Between oldest and newest",
                        "status": "in_progress",
                        "note": "",
                        "execution": {
                            "model": "cursor/test",
                            "started": "2026-08-02T10:00:00Z",
                        },
                        "steps": [{"key": "create-plan", "title": "Create plan", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                    {
                        "key": "no-timestamps",
                        "kind": "implement",
                        "title": "No timestamps",
                        "goal": "Sorts after timestamped slices",
                        "status": "planned",
                        "note": "",
                        "steps": [{"key": "create-plan", "title": "Create plan", "status": "planned", "note": ""}],
                        "final_steps": [],
                    },
                ],
            },
        })

        out = run(str(PI_JOB), "--task", str(task), "markdown", "--chronological").stdout
        oldest_idx = out.index("### oldest")
        middle_idx = out.index("### middle")
        newest_idx = out.index("### newest")
        none_idx = out.index("### no-timestamps")
        order = [oldest_idx, middle_idx, newest_idx, none_idx]
        if order != sorted(order):
            raise AssertionError(f"expected chronological order oldest, middle, newest, no-timestamps:\n{out}")


def test_markdown_summary_omits_steps_and_context() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "summary.yaml"
        write_task_yaml(task, _markdown_representative_mapping())

        out = run(str(PI_JOB), "--task", str(task), "markdown", "--summary").stdout
        assert_contains(out, "## Decisions")
        assert_contains(out, "## Contents")
        assert_contains(out, "### active-slice (current)")
        assert_contains(out, "**Goal:**")
        assert_not_contains(out, "## Context")
        assert_not_contains(out, "## Source")
        assert_not_contains(out, "## Artifacts")
        assert_not_contains(out, "#### Steps")
        assert_not_contains(out, "**edit-code**")
        assert_not_contains(out, "**Repo work")


def test_markdown_slice_scopes_to_one_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "one-slice.yaml"
        write_task_yaml(task, _markdown_representative_mapping())

        out = run(str(PI_JOB), "--task", str(task), "markdown", "--slice", "active-slice").stdout
        assert_not_contains(out, "## Decisions")
        assert_not_contains(out, "## Context")
        assert_not_contains(out, "## Contents")
        assert_contains(out, "### active-slice (current)")
        assert_contains(out, "#### Steps")
        assert_contains(out, "**edit-code** (current)")
        assert_not_contains(out, "### done-slice")

        with_dec = run(
            str(PI_JOB),
            "--task",
            str(task),
            "markdown",
            "--slice",
            "active-slice",
            "--with-decisions",
        ).stdout
        assert_contains(with_dec, "## Decisions")
        assert_contains(with_dec, "### active-slice (current)")

        missing = run(str(PI_JOB), "--task", str(task), "markdown", "--slice", "nope", check=False)
        if missing.returncode == 0:
            raise AssertionError("expected unknown --slice to fail")
        assert_contains(missing.stderr, "slice not found")


def test_markdown_summary_and_slice_are_mutually_exclusive() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "mutex.yaml"
        write_task_yaml(task, _markdown_representative_mapping())
        res = run(
            str(PI_JOB),
            "--task",
            str(task),
            "markdown",
            "--summary",
            "--slice",
            "active-slice",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("expected --summary with --slice to fail")
        assert_contains(res.stderr, "mutually exclusive")


def test_markdown_validation_failure() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "invalid.yaml"
        write_task_yaml(task, {
            "title": "Invalid preview",
            "status": "in_progress",
            "plan": {
                "note": "",
                "slices": [{
                    "key": "broken",
                    "kind": "implement",
                    "title": "Broken",
                    "goal": "Bad status",
                    "status": "planned",
                    "note": "",
                    "steps": [{"key": "create-plan", "title": "Create plan", "status": "not-a-status", "note": ""}],
                    "final_steps": [],
                }],
            },
        })

        res = run(str(PI_JOB), "--task", str(task), "markdown", check=False)
        if res.returncode == 0:
            raise AssertionError("expected markdown to fail validation")
        assert_contains(res.stderr, "task validation failed")
        if res.stdout.strip().startswith("# Invalid preview"):
            raise AssertionError(f"stdout must not look like successful markdown preview:\n{res.stdout}")


def test_markdown_read_only() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "readonly.yaml"
        write_task_yaml(task, _markdown_representative_mapping())
        before = hashlib.sha256(task.read_bytes()).hexdigest()

        run(str(PI_JOB), "--task", str(task), "markdown", "--chronological")

        after = hashlib.sha256(task.read_bytes()).hexdigest()
        if before != after:
            raise AssertionError("markdown preview must not mutate the task file")


def test_block_slice_gate_appends_depends_on() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "block-gate.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"].append(
            {
                "key": "fix-slice",
                "kind": "implement",
                "title": "Fix",
                "goal": "Unblock",
                "status": "planned",
                "note": "",
                "steps": [],
                "final_steps": [],
            }
        )
        write_task_yaml(task, mapping)
        run(
            str(PI_JOB),
            "--task",
            str(task),
            "block-slice",
            "--key",
            "implementation",
            "--reason",
            "Needs fix",
            "--gate",
            "fix-slice",
        )
        module = load_pi_job_module()
        blocked = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["plan"]["slices"][0]
        assert blocked["status"] == "blocked"
        assert "fix-slice" in blocked.get("depends_on", [])


def test_add_finding_appends_sidecar_not_yaml() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "findings.yaml"
        write_task_yaml(task, lifecycle_mapping())
        before = task.read_text(encoding="utf-8")
        run(
            str(PI_JOB),
            "--task",
            str(task),
            "add-finding",
            "--note",
            "Root cause: status Active too early",
            "--source",
            "test",
        )
        after = task.read_text(encoding="utf-8")
        assert before == after
        module = load_pi_job_module()
        store = module.YamlTaskStore(module.YamlTaskLayout(task))
        findings = store.layout.findings_file()
        assert findings == Path(tmp) / "findings.plans" / "_findings.md"
        assert findings.is_file()
        body = findings.read_text(encoding="utf-8")
        assert_contains(body, "Root cause: status Active too early")
        assert_contains(body, "(test)")
        assert store.layout.findings_pointer() == "findings.plans/_findings.md"


def test_yaml_task_layout_owns_plans_paths() -> None:
    module = load_pi_job_module()
    task = Path("/tmp/demo/my-task.yaml")
    layout = module.YamlTaskLayout(task)
    assert layout.plans_dir == Path("/tmp/demo/my-task.plans")
    assert layout.findings_file() == Path("/tmp/demo/my-task.plans/_findings.md")
    assert layout.slice_plan_file("alpha") == Path("/tmp/demo/my-task.plans/alpha.md")
    assert layout.findings_pointer() == "my-task.plans/_findings.md"
    assert layout.document_path == task
    assert layout.describe_store() == f"YAML task file {task}"


def test_bundle_task_layout_owns_plans_paths() -> None:
    module = load_pi_job_module()
    root = Path("/tmp/demo/my-task")
    layout = module.BundleTaskLayout(root)
    assert layout.document_path == Path("/tmp/demo/my-task/task.yaml")
    assert layout.plans_dir == Path("/tmp/demo/my-task/plans")
    assert layout.references_dir == Path("/tmp/demo/my-task/references")
    assert layout.findings_file() == Path("/tmp/demo/my-task/plans/_findings.md")
    assert layout.slice_plan_file("alpha") == Path("/tmp/demo/my-task/plans/alpha.md")
    assert layout.findings_pointer() == "plans/_findings.md"
    assert layout.slice_plan_pointer("alpha") == "Plan file: plans/alpha.md"
    assert layout.describe_store() == f"task bundle {root}"


def test_open_task_store_bundle_dir() -> None:
    """A directory containing `task.yaml` opens as a YamlTaskStore over BundleTaskLayout,
    never as the experimental FsTaskStore."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp) / "my-task"
        root.mkdir()
        write_task_yaml(root / "task.yaml", module.example_task_mapping())

        store = module.open_task_store(root)

        assert isinstance(store, module.YamlTaskStore)
        assert isinstance(store.layout, module.BundleTaskLayout)
        assert not isinstance(store, module.FsTaskStore)
        assert store.layout.bundle_root == root
        assert store.path == root / "task.yaml"


def test_open_task_store_bundle_task_yaml_path() -> None:
    """Pointing `--task` directly at a bundle's `task.yaml` resolves the same bundle root."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp) / "my-task"
        root.mkdir()
        task_yaml = root / "task.yaml"
        write_task_yaml(task_yaml, module.example_task_mapping())

        store = module.open_task_store(task_yaml)

        assert isinstance(store, module.YamlTaskStore)
        assert isinstance(store.layout, module.BundleTaskLayout)
        assert store.layout.bundle_root == root
        assert store.path == task_yaml


def test_open_task_store_non_bundle_dir_still_fs() -> None:
    """A plain directory without `task.yaml` keeps opening as FsTaskStore."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp) / "task"
        base.mkdir()
        (base / "title").write_text("No task.yaml here\n")

        store = module.open_task_store(base)

        assert isinstance(store, module.FsTaskStore)


def test_bundle_read_write_and_plan_stub() -> None:
    """Bundle round-trip: write via the layout's document_path, read it back, and
    confirm a slice plan stub lands under `plans/` (not `<stem>.plans/`)."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp) / "bundled-task"
        layout = module.BundleTaskLayout(root)
        store = module.YamlTaskStore(layout)

        store.replace(module.example_task_mapping(title="Bundle round-trip"))
        assert layout.document_path.is_file()
        assert layout.document_path == root / "task.yaml"

        task = store.read()
        assert task["title"] == "Bundle round-trip"

        stub = store.ensure_slice_plan_stub(key="alpha", kind="implement", goal="Alpha goal")
        assert stub == root / "plans" / "alpha.md"
        assert stub.is_file()
        assert not (root / "alpha.plans").exists()


def test_bundle_slug_under_home_pure() -> None:
    """`bundle_slug_under_home` returns the directory name only for an immediate child of
    the configured task home; a nested or outside-home bundle has no display slug."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            direct = module.BundleTaskLayout(home / "direct-child")
            assert module.bundle_slug_under_home(direct) == "direct-child"

            nested = module.BundleTaskLayout(home / "group" / "nested-child")
            assert module.bundle_slug_under_home(nested) is None

            outside = module.BundleTaskLayout(Path(tmp) / "elsewhere" / "outside-child")
            assert module.bundle_slug_under_home(outside) is None
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_task_display_ref_home_bundle() -> None:
    """`status`'s `Task:` line shows the bundle slug (not the absolute path) for a bundle
    opened by slug from directly under the configured task home."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            bundle = home / "ref-slug"
            write_task_yaml(bundle / "task.yaml", standard_fixture_mapping())

            status = run(str(PI_JOB), "--task", "ref-slug", "status").stdout
            assert_contains(status, "Task: ref-slug")
            assert_not_contains(status, str(bundle))
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_task_display_ref_loose_yaml() -> None:
    """A loose YAML task (never slug-addressable) shows its resolved path in `Task:`."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "loose-ref.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, f"Task: {task.resolve()}")


def test_task_display_ref_outside_bundle() -> None:
    """A bundle opened by path from outside the configured task home shows its resolved
    `task.yaml` path, not a slug, even though its directory name looks like a valid slug."""
    with tempfile.TemporaryDirectory() as tmp:
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(Path(tmp) / "not-the-home")
        try:
            bundle = Path(tmp) / "outside-bundle"
            task = bundle / "task.yaml"
            write_task_yaml(task, standard_fixture_mapping())

            status = run(str(PI_JOB), "--task", str(task), "status").stdout
            assert_contains(status, f"Task: {task.resolve()}")
            assert_not_contains(status, "Task: outside-bundle")
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_status_task_line_uses_display_ref() -> None:
    """`status` prints the title under `Task:` as before, then a second `Task:` line for the
    display ref; the old `File:` label is gone."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "status-ref.yaml"
        write_task_yaml(task, standard_fixture_mapping(title="Ref Line Task"))

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Task: Ref Line Task")
        assert_contains(status, f"Task: {task.resolve()}")
        assert_not_contains(status, "File:")


def test_instruction_plan_pointer_bundle() -> None:
    """create-plan's `Slice plan file:` pointer resolves to the bundle's `plans/<key>.md`,
    and the note-pointer backtick matches the same shape."""
    with tempfile.TemporaryDirectory() as tmp:
        bundle = Path(tmp) / "plan-pointer-bundle"
        task = bundle / "task.yaml"
        task.parent.mkdir(parents=True, exist_ok=True)
        task.write_text(subagent_create_plan_yaml_task(slice_key="plan-slice"), encoding="utf-8")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        resolved_bundle = task.resolve().parent
        assert_contains(instruction, f"Slice plan file: {resolved_bundle / 'plans' / 'plan-slice.md'}")
        assert_contains(instruction, "create-plan note must be only `Plan file: plans/plan-slice.md`")


def test_instruction_plan_pointer_loose() -> None:
    """create-plan's `Slice plan file:` pointer resolves to the loose `<stem>.plans/<key>.md`
    sibling directory, and the note-pointer backtick matches the same shape."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "plan-pointer-loose.yaml"
        task.write_text(subagent_create_plan_yaml_task(slice_key="plan-slice"), encoding="utf-8")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        resolved_task = task.resolve()
        plan_path = resolved_task.parent / f"{resolved_task.stem}.plans" / "plan-slice.md"
        assert_contains(instruction, f"Slice plan file: {plan_path}")
        assert_contains(
            instruction,
            "create-plan note must be only `Plan file: plan-pointer-loose.plans/plan-slice.md`",
        )


def test_markdown_plan_label_bundle() -> None:
    """`markdown --slice` labels a bundle's plan body `plans/<slice>.md`, not the previously
    hardcoded stem-based loose label."""
    with tempfile.TemporaryDirectory() as tmp:
        bundle = Path(tmp) / "bundle-label"
        task = bundle / "task.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        plans_dir = bundle / "plans"
        plans_dir.mkdir()
        (plans_dir / "second-slice.md").write_text("# Second slice plan\n", encoding="utf-8")

        out = run(str(PI_JOB), "--task", str(task), "markdown", "--slice", "second-slice").stdout
        assert_contains(out, "#### Plan file")
        assert_contains(out, "`plans/second-slice.md`")
        assert_contains(out, "# Second slice plan")
        assert_not_contains(out, "bundle-label.plans/second-slice.md")


def test_markdown_plan_label_loose() -> None:
    """`markdown --slice` labels a loose task's plan body `<stem>.plans/<slice>.md`."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "loose-label.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        plans_dir = task.parent / f"{task.stem}.plans"
        plans_dir.mkdir()
        (plans_dir / "second-slice.md").write_text("# Second slice plan\n", encoding="utf-8")

        out = run(str(PI_JOB), "--task", str(task), "markdown", "--slice", "second-slice").stdout
        assert_contains(out, "#### Plan file")
        assert_contains(out, "`loose-label.plans/second-slice.md`")
        assert_contains(out, "# Second slice plan")


def test_store_describe_uses_layout() -> None:
    """describe() delegates to the layout: loose YAML keeps 'YAML task file …';
    bundles say 'task bundle …' and never claim to be a bare file."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        loose = Path(tmp) / "loose.yaml"
        write_task_yaml(loose, module.example_task_mapping())
        loose_store = module.open_task_store(loose)
        assert loose_store.describe() == f"YAML task file {loose}"

        root = Path(tmp) / "bundle-task"
        root.mkdir()
        write_task_yaml(root / "task.yaml", module.example_task_mapping())
        bundle_store = module.open_task_store(root)
        assert bundle_store.describe() == f"task bundle {root}"
        assert "file" not in bundle_store.describe()


def test_task_tasks_home_default() -> None:
    """With `PI_JOB_TASKS` unset, the home defaults to `~/.local/share/pi-job/tasks`."""
    module = load_pi_job_module()
    saved = os.environ.pop("PI_JOB_TASKS", None)
    try:
        expected = Path("~/.local/share/pi-job/tasks").expanduser().resolve()
        assert module.task_tasks_home() == expected
    finally:
        if saved is not None:
            os.environ["PI_JOB_TASKS"] = saved


def test_task_tasks_home_override() -> None:
    """`PI_JOB_TASKS` overrides the default home, expanded and resolved."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        custom = Path(tmp) / "custom-tasks"
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(custom)
        try:
            assert module.task_tasks_home() == custom.resolve()
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_resolve_task_arg_slug() -> None:
    """A bare slug resolves to `$PI_JOB_TASKS/<slug>/task.yaml`; the CLI accepts the slug too."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        bundle = home / "demo-slug"
        bundle.mkdir(parents=True)
        write_task_yaml(bundle / "task.yaml", module.example_task_mapping())
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            resolved = module.resolve_task_arg("demo-slug")
            assert resolved == (bundle / "task.yaml").resolve()

            result = run(str(PI_JOB), "--task", "demo-slug", "status")
            assert_contains(result.stdout, "Task:")
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_resolve_task_arg_unknown_slug() -> None:
    """A well-formed but absent slug dies naming the slug and the expected `task.yaml` path."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        home.mkdir()
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            try:
                module.resolve_task_arg("no-such-slug")
                raise AssertionError("expected SystemExit for unknown slug")
            except SystemExit as exc:
                assert exc.code != 0

            result = run(str(PI_JOB), "--task", "no-such-slug", "status", check=False)
            assert result.returncode != 0
            assert_contains(result.stderr, "no-such-slug")
            assert_contains(result.stderr, str(home / "no-such-slug" / "task.yaml"))
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_resolve_task_arg_invalid_charset() -> None:
    """Relative non-slug tokens with no path separator die before ever touching `cwd`."""
    module = load_pi_job_module()
    for bad in ("Bad_Slug", "UPPER", "", "-leading", "my.task.yaml", "task.yaml"):
        try:
            module.resolve_task_arg(bad)
            raise AssertionError(f"expected SystemExit for invalid slug {bad!r}")
        except SystemExit as exc:
            assert exc.code != 0

    result = run(str(PI_JOB), "--task", "Bad_Slug", "status", check=False)
    assert result.returncode != 0
    assert_contains(result.stderr, "invalid task slug")


def test_resolve_task_arg_path_loose_yaml_unchanged() -> None:
    """A path (not a slug) to a loose YAML file still opens via `YamlTaskLayout`."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "legacy.task.yaml"
        write_task_yaml(task, module.example_task_mapping())

        resolved = module.resolve_task_arg(str(task))

        assert resolved == task.resolve()
        store = module.open_task_store(resolved)
        assert isinstance(store.layout, module.YamlTaskLayout)


def test_resolve_task_arg_path_bundle_dir_unchanged() -> None:
    """A path (not a slug) to a bundle directory still opens via `BundleTaskLayout`."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        bundle = Path(tmp) / "some-bundle"
        bundle.mkdir()
        write_task_yaml(bundle / "task.yaml", module.example_task_mapping())

        resolved = module.resolve_task_arg(str(bundle))

        assert resolved == bundle.resolve()
        store = module.open_task_store(resolved)
        assert isinstance(store.layout, module.BundleTaskLayout)


def test_resolve_task_arg_slug_ignores_loose_yaml_in_home() -> None:
    """A loose `<slug>.yaml` sitting directly under the home is not a slug target."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        home.mkdir()
        write_task_yaml(home / "demo-slug.yaml", module.example_task_mapping())
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            try:
                module.resolve_task_arg("demo-slug")
                raise AssertionError("expected SystemExit: loose yaml must not satisfy slug lookup")
            except SystemExit as exc:
                assert exc.code != 0
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_layout_for_document_path_bundle_and_loose() -> None:
    """A `task.yaml` document path resolves to `BundleTaskLayout`; anything else to `YamlTaskLayout`."""
    module = load_pi_job_module()
    bundle_layout = module.layout_for_document_path(Path("/tmp/demo/my-task/task.yaml"))
    assert isinstance(bundle_layout, module.BundleTaskLayout)
    assert bundle_layout.bundle_root == Path("/tmp/demo/my-task")

    loose_layout = module.layout_for_document_path(Path("/tmp/demo/legacy.yaml"))
    assert isinstance(loose_layout, module.YamlTaskLayout)
    assert loose_layout.task_path == Path("/tmp/demo/legacy.yaml")


def test_derive_bundle_root_task_yaml_parent_dir_self_and_loose_dies() -> None:
    """`derive_bundle_root`: `task.yaml` -> parent; directory -> itself; loose `*.yaml`/`*.yml` -> die."""
    module = load_pi_job_module()
    assert module.derive_bundle_root(Path("/tmp/demo/my-task/task.yaml")) == Path("/tmp/demo/my-task")

    with tempfile.TemporaryDirectory() as tmp:
        existing_dir = Path(tmp) / "bundle-root"
        existing_dir.mkdir()
        assert module.derive_bundle_root(existing_dir) == existing_dir

        # A non-existent path with no recognized YAML suffix is treated as the bundle root itself.
        fresh_dir = Path(tmp) / "not-yet-created"
        assert module.derive_bundle_root(fresh_dir) == fresh_dir

        for loose in (Path(tmp) / "legacy.yaml", Path(tmp) / "legacy.yml"):
            try:
                module.derive_bundle_root(loose)
                raise AssertionError(f"expected SystemExit for loose YAML target {loose}")
            except SystemExit as exc:
                assert exc.code != 0


def test_scaffold_bundle_dirs_idempotent_preserves_contents() -> None:
    """`scaffold_bundle_dirs` is idempotent and never touches existing `plans/`/`references/` contents."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        bundle_root = Path(tmp) / "bundle"
        module.scaffold_bundle_dirs(bundle_root)
        assert bundle_root.is_dir()
        assert (bundle_root / "plans").is_dir()
        assert (bundle_root / "references").is_dir()

        marker = bundle_root / "plans" / "keep-me.md"
        marker.write_text("keep this\n", encoding="utf-8")

        module.scaffold_bundle_dirs(bundle_root)
        assert marker.is_file()
        assert marker.read_text(encoding="utf-8") == "keep this\n"


def test_create_slug_scaffolds_bundle() -> None:
    """`create` with a bare slug scaffolds `$PI_JOB_TASKS/<slug>/{task.yaml,plans/,references/}`."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            resolved = module.resolve_create_task_arg("demo-slug")
            assert resolved == (home / "demo-slug" / "task.yaml").resolve()

            out = run(str(PI_JOB), "--task", "demo-slug", "create", "--kind", "setup").stdout
            assert_contains(out, "created:")
            bundle = home / "demo-slug"
            assert (bundle / "task.yaml").is_file()
            assert (bundle / "plans").is_dir()
            assert (bundle / "references").is_dir()

            status = run(str(PI_JOB), "--task", "demo-slug", "status").stdout
            assert_contains(status, "Initialization: ok")
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_create_path_scaffolds_bundle() -> None:
    """`create` with a path (not a slug) scaffolds a bundle at that directory; seed pointers
    use layout-relative `plans/…`, never a name-derived `<stem>.plans/…`."""
    with tempfile.TemporaryDirectory() as tmp:
        bundle = Path(tmp) / "out-of-home-bundle"
        bundle.mkdir()
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(
            minimal_bootstrap_input_yaml(
                slices_yaml="""
slices:
  - key: feature-x
    kind: implement
    title: Feature X
    goal: Build feature X.
    depends_on:
      - task-setup
""",
            ),
            encoding="utf-8",
        )
        out = run(str(PI_JOB), "--task", str(bundle), "create", "--from", str(bootstrap_input)).stdout
        assert (bundle / "task.yaml").is_file()
        assert (bundle / "plans").is_dir()
        assert (bundle / "references").is_dir()
        seed = seed_block_after_marker(out)
        assert_contains(seed, "plans/feature-x.md")
        if "out-of-home-bundle" in seed:
            raise AssertionError(f"seed pointer must be layout-relative, not name-derived:\n{seed}")


def test_create_duplicate_slug() -> None:
    """A second `create` for an initialized slug dies; `--force` overwrites only `task.yaml`."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        saved = os.environ.get("PI_JOB_TASKS")
        os.environ["PI_JOB_TASKS"] = str(home)
        try:
            run(str(PI_JOB), "--task", "dup-slug", "create", "--kind", "setup")
            bundle = home / "dup-slug"
            marker = bundle / "plans" / "keep-me.md"
            marker.write_text("keep this\n", encoding="utf-8")

            again = run(str(PI_JOB), "--task", "dup-slug", "create", check=False)
            assert again.returncode != 0
            assert_contains(again.stderr, "already exists")
            assert marker.is_file()

            forced = run(str(PI_JOB), "--task", "dup-slug", "create", "--force", "--title", "Forced retitle")
            assert forced.returncode == 0
            assert marker.is_file()
            assert marker.read_text(encoding="utf-8") == "keep this\n"
            status = run(str(PI_JOB), "--task", "dup-slug", "status").stdout
            assert_contains(status, "Forced retitle")
        finally:
            if saved is None:
                os.environ.pop("PI_JOB_TASKS", None)
            else:
                os.environ["PI_JOB_TASKS"] = saved


def test_create_invalid_slug() -> None:
    """`create` with a malformed bare slug dies with the same charset hint as non-create commands."""
    module = load_pi_job_module()
    for bad in ("Bad_Slug", "task.yaml"):
        try:
            module.resolve_create_task_arg(bad)
            raise AssertionError(f"expected SystemExit for invalid slug {bad!r}")
        except SystemExit as exc:
            assert exc.code != 0

    result = run(str(PI_JOB), "--task", "Bad_Slug", "create", check=False)
    assert result.returncode != 0
    assert_contains(result.stderr, "invalid task slug")

    result_bare_yaml = run(str(PI_JOB), "--task", "task.yaml", "create", check=False)
    assert result_bare_yaml.returncode != 0
    assert_contains(result_bare_yaml.stderr, "invalid task slug")


def test_create_rejects_loose_yaml_path() -> None:
    """`create` refuses a loose `*.yaml` target with a bundle hint, and never writes it."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        loose = Path(tmp) / "x.yaml"
        try:
            module.resolve_create_task_arg(str(loose))
            raise AssertionError("expected SystemExit for a loose YAML create target")
        except SystemExit as exc:
            assert exc.code != 0

        result = run(str(PI_JOB), "--task", str(loose), "create", check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "not a loose YAML file")
        assert not loose.exists()


def test_add_decision_spills_long_note_to_plan_file() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "spill.yaml"
        write_task_yaml(task, lifecycle_mapping())
        long_note = "x" * 2500
        run(
            str(PI_JOB),
            "--task",
            str(task),
            "add-decision",
            "--note",
            long_note,
            "--source",
            "spill-test",
        )
        module = load_pi_job_module()
        decisions = module.YamlTaskStore(module.YamlTaskLayout(task)).read().get("decisions") or []
        assert decisions
        yaml_note = decisions[-1]["note"]
        assert yaml_note.startswith("Plan file:")
        assert long_note not in yaml_note
        rel = yaml_note.removeprefix("Plan file: ").strip()
        spilled = (task.parent / rel).resolve()
        assert spilled.is_file()
        assert long_note in spilled.read_text(encoding="utf-8")


def test_add_slice_creates_plan_stub() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "stub.yaml"
        write_task_yaml(task, lifecycle_mapping())
        run(
            str(PI_JOB),
            "--task",
            str(task),
            "add-slice",
            "--key",
            "new-impl",
            "--kind",
            "implement",
            "--title",
            "New",
            "--goal",
            "Ship a stub",
            "--repos",
            "chezmoi",
        )
        stub = Path(tmp) / "stub.plans" / "new-impl.md"
        assert stub.is_file()
        body = stub.read_text(encoding="utf-8")
        module = load_pi_job_module()
        template = module.load_profile_contract()["instruction_packets"]["slice_plan_stub"]
        # Stub body must come from the profile template, not a Python hardcode.
        assert_contains(template, "## Types and composition")
        assert_contains(template, "## Call stacks")
        assert_contains(template, "## Intent")
        assert_contains(template, "## Open questions")
        assert_contains(body, "## Types and composition")
        assert_contains(body, "## Call stacks")
        assert_contains(body, "## Intent")
        assert_contains(body, "## Open questions")
        assert_contains(body, "## Goal")
        assert_contains(body, "Ship a stub")
        assert_contains(body, "# new-impl")


def test_profile_requires_slice_plan_stub_and_findings_header() -> None:
    module = load_pi_job_module()
    packets = module.load_profile_contract()["instruction_packets"]
    assert_contains(packets["slice_plan_stub"], "{key}")
    assert_contains(packets["slice_plan_stub"], "{goal}")
    assert_contains(packets["slice_plan_stub"], "{depends_on}")
    assert_contains(packets["findings_file_header"], "# Findings")
    assert_contains(packets["status_interrupt_hint"], "investigate")
    assert_contains(packets["investigate_interrupt"], "{topic}")
    assert_contains(packets["investigate_interrupt"], "{finding_status}")
    heartbeat = packets["orchestrator_heartbeat"]
    assert_contains(heartbeat, "TASK")
    assert "Manager metronome" in heartbeat
    assert "{interval}" not in heartbeat
    assert "{task_file}" not in heartbeat
    assert not heartbeat.lstrip().startswith("/loop")
    park = module.load_profile_contract()["interrupt_park_steps"]
    assert "grill-plan" in park
    assert "clarify-scope" in park
    for field in (
        "slice_plan_stub",
        "findings_file_header",
        "status_interrupt_hint",
        "investigate_interrupt",
        "orchestrator_heartbeat",
    ):
        profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
        del profile["instruction_packets"][field]
        try:
            module.ProfileDocument.model_validate(profile)
        except module.ValidationError as exc:
            assert_contains(str(exc), field)
        else:
            raise AssertionError(
                f"profile accepted instruction_packets without required {field}"
            )
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    del profile["interrupt_park_steps"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "interrupt_park_steps")
    else:
        raise AssertionError("profile accepted without required interrupt_park_steps")
    profile = module.load_yaml_mapping(module.PROFILE, label="execution profile")
    profile["interrupt_park_steps"] = ["grill-plan", "not-a-real-step"]
    try:
        module.ProfileDocument.model_validate(profile)
    except module.ValidationError as exc:
        assert_contains(str(exc), "not-a-real-step")
    else:
        raise AssertionError("profile accepted unknown interrupt_park_steps entry")


def test_status_shows_blocked_and_interrupt_hint() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "status-ux.yaml"
        mapping = lifecycle_mapping()
        mapping["plan"]["slices"][0]["steps"] = [
            {"key": "grill-plan", "title": "Grill", "status": "planned", "note": ""},
        ]
        mapping["plan"]["slices"][0]["status"] = "blocked"
        mapping["plan"]["slices"][0]["note"] = "GP PIN fails after treatment-change"
        # grill-plan requires user decision; but slice is blocked - still show hint from step kind
        # Use a second slice as the claim host that is not blocked
        mapping["plan"]["slices"].append(
            {
                "key": "other",
                "kind": "implement",
                "title": "Other",
                "goal": "Parked grill",
                "status": "in_progress",
                "note": "",
                "steps": [
                    {"key": "grill-plan", "title": "Grill plan", "status": "planned", "note": ""},
                ],
                "final_steps": [],
            }
        )
        mapping["orchestration"]["cursors"] = [claim_dict("other")]
        write_task_yaml(task, mapping)
        out = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(out, "Blocked:")
        assert_contains(out, "implementation")
        assert_contains(out, "parked on a user-decision step")
        assert_contains(out, "investigate")


def _normalized_orchestrator_heartbeat(module) -> str:
    body = module.load_profile_contract()["instruction_packets"]["orchestrator_heartbeat"]
    return " ".join(str(body).split())


def test_render_orchestrator_heartbeat() -> None:
    module = load_pi_job_module()
    rendered = module.render_orchestrator_heartbeat()
    expected = _normalized_orchestrator_heartbeat(module)
    assert rendered == expected
    assert_contains(rendered, "TASK")
    assert_not_contains(rendered, "{interval}")
    assert_not_contains(rendered, "{task_file}")
    assert not rendered.lstrip().startswith("/loop")
    assert len(rendered.splitlines()) == 1


def test_loop_command_prints_heartbeat_without_task() -> None:
    module = load_pi_job_module()
    expected = _normalized_orchestrator_heartbeat(module)
    res = run(str(PI_JOB), "loop")
    stdout = res.stdout.rstrip("\n")
    assert stdout == expected
    assert_contains(stdout, "TASK")
    assert not stdout.lstrip().startswith("/loop")
    assert len(stdout.splitlines()) == 1


def test_loop_rejects_interval_flag() -> None:
    res = run(str(PI_JOB), "loop", "--interval", "5m", check=False)
    assert res.returncode != 0
    assert_contains(res.stderr, "unrecognized arguments")


def test_investigate_does_not_move_claim() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "investigate.yaml"
        mapping = lifecycle_mapping()
        mapping["orchestration"]["cursors"] = [claim_dict("implementation")]
        write_task_yaml(task, mapping)
        out = run(
            str(PI_JOB),
            "--task",
            str(task),
            "investigate",
            "--topic",
            "already-active",
            "--note",
            "Evidence chain complete",
        ).stdout
        assert_contains(out, "PI-JOB INVESTIGATE")
        assert_contains(out, "do not finish/release it")
        module = load_pi_job_module()
        claims = module.YamlTaskStore(module.YamlTaskLayout(task)).read()["orchestration"]["cursors"]
        assert len(claims) == 1
        assert claims[0]["slice"] == "implementation"
        findings = Path(tmp) / "investigate.plans" / "_findings.md"
        assert findings.is_file()
        assert_contains(findings.read_text(encoding="utf-8"), "Evidence chain complete")


def test_list_home_bundles_only() -> None:
    """`pi-job list` shows bundle dirs under the task home; a loose `*.yaml` sibling is
    never listed (out of scope; use `project` to bundle it first)."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        home.mkdir()
        bundle = home / "demo-slug"
        write_task_yaml(bundle / "task.yaml", standard_fixture_mapping(title="Demo bundle"))
        write_task_yaml(home / "loose.yaml", standard_fixture_mapping(title="Loose task"))

        with _pi_job_tasks_home(home):
            out = run(str(PI_JOB), "list").stdout
            assert_contains(out, "demo-slug")
            assert_contains(out, "Demo bundle")
            assert_not_contains(out, "Loose task")
            assert_not_contains(out, "loose.yaml")


def test_derived_task_status_ignores_stored_field() -> None:
    """Overall status comes from slices; stale top-level task.status is ignored."""
    module = load_pi_job_module()
    mapping = standard_fixture_mapping()
    mapping["status"] = "planned"
    assert module.derived_task_status(mapping) == "in_progress"

    mapping["plan"]["slices"][1]["status"] = "done"
    for step in mapping["plan"]["slices"][1]["steps"]:
        step["status"] = "done"
    for step in mapping["plan"]["slices"][1]["final_steps"]:
        step["status"] = "done"
    mapping["status"] = "planned"
    assert module.derived_task_status(mapping) == "done"

    mapping["plan"]["slices"][0]["status"] = "skipped"
    mapping["plan"]["slices"][1]["status"] = "skipped"
    assert module.derived_task_status(mapping) == "skipped"

    mapping["plan"]["slices"][0]["status"] = "blocked"
    mapping["plan"]["slices"][1]["status"] = "done"
    assert module.derived_task_status(mapping) == "blocked"

    mapping["plan"]["slices"] = []
    assert module.derived_task_status(mapping) == "planned"

    mapping["plan"]["slices"] = [
        {
            "key": "only",
            "kind": "implement",
            "title": "Only",
            "goal": "g",
            "status": "planned",
            "note": "",
            "steps": [],
            "final_steps": [],
        },
        {
            "key": "done-one",
            "kind": "implement",
            "title": "Done",
            "goal": "g",
            "status": "done",
            "note": "",
            "steps": [],
            "final_steps": [],
        },
    ]
    assert module.derived_task_status(mapping) == "in_progress"


def test_status_and_list_use_derived_task_status() -> None:
    """status and list show derived overall status even when task.status is stale."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        mapping = standard_fixture_mapping(title="Derived Status Task")
        mapping["status"] = "planned"
        for task_slice in mapping["plan"]["slices"]:
            task_slice["status"] = "done"
            for step in task_slice.get("steps") or []:
                step["status"] = "done"
            for step in task_slice.get("final_steps") or []:
                step["status"] = "done"
        write_task_yaml(home / "derived-status" / "task.yaml", mapping)

        with _pi_job_tasks_home(home):
            status_out = run(str(PI_JOB), "--task", "derived-status", "status").stdout
            assert_contains(status_out, "Status: done")
            assert_not_contains(status_out, "Status: planned")

            list_out = run(str(PI_JOB), "list").stdout
            assert_contains(list_out, "derived-status")
            assert_contains(list_out, "[done]")
            assert_not_contains(list_out, "[planned]")


def test_list_row_fields() -> None:
    """A `pi-job list` block includes readable task metadata and cursor positions."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        bundle = home / "row-fields-slug"
        write_task_yaml(bundle / "task.yaml", standard_fixture_mapping(title="Row Fields Task"))

        with _pi_job_tasks_home(home):
            out = run(str(PI_JOB), "list").stdout
            assert_contains(out, "row-fields-slug [in_progress]")
            assert_contains(out, "  Title: Row Fields Task")
            assert_contains(out, "  Updated:")
            assert_contains(out, "  Cursor:")
            assert_contains(out, DEFAULT_OWNER)
            assert_contains(out, "old-slice")  # claim position from fixture
            assert_not_contains(out, "Ready:")


def test_list_orders_status_groups_then_cursor_activity() -> None:
    """Status groups are fixed; tasks within a group use newest cursor heartbeat first."""

    def with_status(status: str, *, last_seen: str) -> dict:
        mapping = standard_fixture_mapping()
        for task_slice in mapping["plan"]["slices"]:
            task_slice["status"] = status
        mapping["orchestration"]["cursors"][0]["last_seen"] = last_seen
        return mapping

    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        tasks = [
            ("active-old", "in_progress", "2099-01-01T00:00:00+00:00"),
            ("done-newest", "done", "2099-12-01T00:00:00+00:00"),
            ("planned", "planned", "2099-06-01T00:00:00+00:00"),
            ("blocked", "blocked", "2099-07-01T00:00:00+00:00"),
            ("skipped", "skipped", "2099-08-01T00:00:00+00:00"),
            ("active-new", "in_progress", "2099-02-01T00:00:00+00:00"),
        ]
        for slug, status, last_seen in tasks:
            mapping = with_status(status, last_seen=last_seen)
            write_task_yaml(home / slug / "task.yaml", mapping)

        with _pi_job_tasks_home(home):
            out = run(str(PI_JOB), "list").stdout

        headers = [line for line in out.splitlines() if line and not line.startswith(" ")]
        expected = [
            "active-new [in_progress]",
            "active-old [in_progress]",
            "blocked [blocked]",
            "planned [planned]",
            "skipped [skipped]",
            "done-newest [done]",
        ]
        if headers != expected:
            raise AssertionError(f"unexpected list ordering:\n{out}")


def test_list_activity_falls_back_to_mtime_and_ties_use_slug() -> None:
    """Missing or malformed cursor timestamps use mtime; slug breaks exact ties."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_file = Path(tmp) / "task.yaml"
        task_file.write_text("placeholder", encoding="utf-8")
        mtime = 1_700_000_000
        os.utime(task_file, (mtime, mtime))

        no_cursor = standard_fixture_mapping()
        no_cursor["orchestration"]["cursors"] = []
        malformed = standard_fixture_mapping()
        malformed["orchestration"]["cursors"][0]["last_seen"] = "not-a-timestamp"
        expected = datetime.fromtimestamp(mtime, UTC)
        assert module.task_list_updated(no_cursor, task_file) == expected
        assert module.task_list_updated(malformed, task_file) == expected

        alpha = module.TaskListEntry("alpha", "A", "planned", expected, ())
        beta = module.TaskListEntry("beta", "B", "planned", expected, ())
        assert sorted([beta, alpha], key=module.task_list_sort_key) == [alpha, beta]


def test_list_renders_each_cursor_on_its_own_line() -> None:
    mapping = standard_fixture_mapping()
    mapping["orchestration"]["cursors"] = [
        claim_dict("first", owner="owner-a", last_seen="2099-01-01T00:00:00+00:00"),
        claim_dict("second-slice", owner="owner-b", last_seen="2099-02-01T00:00:00+00:00"),
    ]
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        write_task_yaml(home / "multi-cursor" / "task.yaml", mapping)
        with _pi_job_tasks_home(home):
            out = run(str(PI_JOB), "list").stdout

        assert_contains(out, "  Cursor: owner-a")
        assert_contains(out, "  Cursor: owner-b")
        if out.count("  Cursor:") != 2:
            raise AssertionError(f"expected one line per cursor:\n{out}")


def test_list_empty_home() -> None:
    """`pi-job list` against an empty (or missing) task home prints nothing and exits 0."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        with _pi_job_tasks_home(home):
            out = run(str(PI_JOB), "list").stdout
            if out.strip():
                raise AssertionError(f"expected empty output for empty task home, got:\n{out}")

        missing = Path(tmp) / "does-not-exist"
        with _pi_job_tasks_home(missing):
            out = run(str(PI_JOB), "list").stdout
            if out.strip():
                raise AssertionError(f"expected empty output for missing task home, got:\n{out}")


def test_list_respects_PI_JOB_TASKS() -> None:
    """`pi-job list` only enumerates the currently configured `PI_JOB_TASKS` home."""
    with tempfile.TemporaryDirectory() as tmp:
        home_a = Path(tmp) / "home-a"
        home_b = Path(tmp) / "home-b"
        write_task_yaml(home_a / "slug-a" / "task.yaml", standard_fixture_mapping(title="Home A task"))
        write_task_yaml(home_b / "slug-b" / "task.yaml", standard_fixture_mapping(title="Home B task"))

        with _pi_job_tasks_home(home_a):
            out = run(str(PI_JOB), "list").stdout
            assert_contains(out, "slug-a")
            assert_not_contains(out, "slug-b")

        with _pi_job_tasks_home(home_b):
            out = run(str(PI_JOB), "list").stdout
            assert_contains(out, "slug-b")
            assert_not_contains(out, "slug-a")


def test_list_skips_unreadable_bundle_with_warning() -> None:
    """An unreadable/invalid bundle is skipped with a stderr warning; `list` still
    reports the other bundles instead of aborting."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        write_task_yaml(home / "good-slug" / "task.yaml", standard_fixture_mapping(title="Good task"))
        bad_doc = home / "bad-slug" / "task.yaml"
        bad_doc.parent.mkdir(parents=True)
        bad_doc.write_text("title: [unterminated\n", encoding="utf-8")

        with _pi_job_tasks_home(home):
            result = run(str(PI_JOB), "list")
            assert_contains(result.stdout, "good-slug")
            assert_contains(result.stdout, "Good task")
            assert_not_contains(result.stdout, "bad-slug")
            assert_contains(result.stderr, "bad-slug")


def test_set_worktree_recommend_missing_path() -> None:
    """set-worktree without `--path`/`--clear` prints a recommendation and dies non-zero
    without writing to the task file."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-recommend.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        before = task.read_text(encoding="utf-8")

        with _pi_job_worktrees_home(Path(tmp) / "worktrees"):
            res = run(
                str(PI_JOB), "--task", str(task), "set-worktree",
                "--slice", "second-slice", "--repo", "graphius",
                check=False,
            )
        if res.returncode == 0:
            raise AssertionError("set-worktree without --path/--clear should fail")
        assert_contains(res.stdout, "recommended worktree path:")
        assert_contains(res.stdout, str(Path(tmp) / "worktrees" / "second-slice" / "graphius"))
        assert_contains(res.stderr, "requires --path")
        if task.read_text(encoding="utf-8") != before:
            raise AssertionError("recommend-only set-worktree must not mutate the task file")


def test_set_worktree_recommend_under_home() -> None:
    """A bundle task's recommendation includes the bundle slug segment:
    `$PI_JOB_WORKTREES/<slug>/<slice>/<repo>`."""
    with tempfile.TemporaryDirectory() as tmp:
        tasks_home = Path(tmp) / "tasks-home"
        worktrees_home = Path(tmp) / "worktrees-home"
        write_task_yaml(tasks_home / "recommend-slug" / "task.yaml", standard_fixture_mapping())

        with _pi_job_tasks_home(tasks_home), _pi_job_worktrees_home(worktrees_home):
            res = run(
                str(PI_JOB), "--task", "recommend-slug", "set-worktree",
                "--slice", "second-slice", "--repo", "graphius",
                check=False,
            )
        if res.returncode == 0:
            raise AssertionError("set-worktree without --path/--clear should fail")
        expected = worktrees_home / "recommend-slug" / "second-slice" / "graphius"
        assert_contains(res.stdout, str(expected))


def test_set_worktree_recommend_loose_yaml() -> None:
    """A loose (non-bundle) task's recommendation omits the slug segment and adds a note
    about projecting into the central home for a slug-based path."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "loose-recommend.yaml"
        write_task_yaml(task, standard_fixture_mapping())
        worktrees_home = Path(tmp) / "worktrees-home"

        with _pi_job_worktrees_home(worktrees_home):
            res = run(
                str(PI_JOB), "--task", str(task), "set-worktree",
                "--slice", "second-slice", "--repo", "graphius",
                check=False,
            )
        if res.returncode == 0:
            raise AssertionError("set-worktree without --path/--clear should fail")
        expected = worktrees_home / "second-slice" / "graphius"
        assert_contains(res.stdout, str(expected))
        assert_not_contains(res.stdout, str(worktrees_home / "loose-recommend"))
        assert_contains(res.stdout, "note:")


def test_set_worktree_help_mentions_worktree_convention() -> None:
    """`set-worktree --help` documents the `$PI_JOB_WORKTREES/<slug>/<slice>/<repo>`
    recommendation convention and its default home."""
    out = run(str(PI_JOB), "set-worktree", "--help").stdout
    assert_contains(out, "PI_JOB_WORKTREES")
    assert_contains(out, "<slug>/<slice>/<repo>")
    assert_contains(out, "~/.local/share/pi-job/worktrees")


def main() -> None:
    test_profiled_task()
    test_uninitialized_task_requires_orchestration()
    test_init_with_kind_setup_seeds_setup_slice()
    test_setup_template_includes_wayfinder_step()
    test_fog_slice_kind_seeds_template()
    test_wayfinder_context_reports_frontier_and_fog()
    test_edit_code_owner_from_step_kinds()
    test_orchestrator_instruction_includes_record_results()
    test_instruction_includes_next_action_and_step_first_layout()
    test_bootstrap_then_claim_instruction_includes_next_action()
    test_subagent_instruction_includes_record_results()
    test_record_results_uses_task_file_and_slice_key_hints()
    test_subagent_instruction_prohibits_direct_task_store_inspection()
    test_subagent_instruction_includes_scoped_read_command()
    test_orchestrator_instruction_has_no_subagent_prompt()
    test_subagent_orchestrator_prompt_is_separate_from_execution_body()
    test_add_decision_and_finish_help_describe_channels()
    test_decision_document_schema_describes_channels_contract()
    test_update_task_file_guidance_names_mutation_commands()
    test_plan_output_omits_record_results()
    test_pick_next_packet_is_structural_only()
    test_channels_cli_prints_catalog_and_step_blurbs()
    test_profile_rejects_missing_record_channels_on_step_kind()
    test_profile_rejects_unknown_record_channel_id()
    test_instruction_collapses_long_slice_goal()
    test_execution_packet_budget_share_with_team()
    test_subagent_execution_packet_budget_excludes_prompt_body()
    test_profile_requires_record_results_intro_packet()
    test_create_plan_instruction_defines_constraint_and_behaviour_contract()
    test_grill_plan_instruction_defines_constraint_and_behaviour_contract()
    test_profile_yaml_aliases_shared_guidance_strings()
    test_pick_next_slice_reports_closing_slice_ready()
    test_status_shows_claim_and_ready_without_next_line()
    test_pick_next_reports_all_slices_done()
    test_advance_is_deprecated_regardless_of_flags()
    test_claim_release_and_one_claim_per_owner()
    test_claim_displaces_stale_and_refuses_fresh()
    test_owner_omit_when_sole_claim_and_ambiguous_refuse()
    test_finish_slice_only_auto_releases_claim()
    test_missing_task_points_to_scaffold()
    test_scaffold_creates_task_file()
    test_toolbelt_lists_for_slice_kinds()
    test_endpoint_status_map_catalog_has_build_example()
    test_toolbelt_add_records_artifact()
    test_select_toolbelt_step_and_instruction()
    test_toolbelt_block_in_plan()
    test_show_renders_tree_and_footer()
    test_show_work_first_puts_open_before_done_newest_completed_last_block()
    test_show_aligns_kind_counts_after_longest_key()
    test_show_omits_kind_counts_and_models_for_done_by_default()
    test_show_short_collapses_consecutive_done_slices()
    test_show_started_flag_expands_non_planned_slices()
    test_show_color_always_tints_glyphs_never_stays_plain()
    test_show_slice_prints_goal_notes_steps_repo_work()
    test_show_tree_unchanged_without_slice()
    test_show_slice_unknown_key_dies()
    test_show_slice_marks_current_step()
    test_show_slice_omits_empty_fields()
    test_show_slice_includes_deps_when_present()
    test_show_slice_multiline_note_indents_continuation()
    test_scaffold_mirrors_implement_template()
    test_scaffold_includes_create_plan_and_grill_plan_before_edit_code()
    test_derived_position_walks_create_plan_then_grill_plan_before_edit_code()
    test_status_ready_skips_unready_head_of_array()
    test_show_ready_tag_lists_only_ready_slices()
    test_status_ready_line_matches_ready_slices()
    test_blocked_status_slice_is_skipped()
    test_pick_next_when_nothing_ready()
    test_status_warns_when_cursor_slice_not_ready()
    test_status_no_warning_when_cursor_slice_is_ready()
    test_derived_position_walks_steps_then_pick_next_on_exhausted_slice()
    test_status_warns_on_unknown_dependency_key()
    test_show_renders_deps_with_mixed_statuses()
    test_show_omits_deps_line_when_absent()
    test_init_rejects_forward_reference_dependency()
    test_scaffold_output_still_validates_via_shared_schema()
    test_add_slice_happy_path_no_repos()
    test_add_slice_happy_path_with_repos()
    test_add_slice_rejects_duplicate_key()
    test_add_slice_requires_repos_when_schema_requires_it()
    test_add_slice_rejects_unsupported_required_field()
    test_add_slice_after_inserts_in_correct_order()
    test_add_slice_rejects_unknown_after_slice()
    test_add_slice_works_on_empty_plan_slices()
    test_add_step_happy_path()
    test_add_step_final_flag()
    test_add_step_rejects_duplicate_key()
    test_add_step_rejects_unknown_slice()
    test_add_step_after_inserts_in_correct_order()
    test_validate_warns_when_persisted_slice_predates_template_addition()
    test_validate_warns_on_long_note()
    test_status_warns_on_long_note()
    test_validate_warns_on_large_task_file()
    test_finish_note_not_refused_when_long()
    test_validate_fails_when_slice_missing_template_steps()
    test_validate_allows_extra_steps_beyond_template()
    test_validate_fails_on_unknown_slice_kind()
    test_validate_slice_passes_when_only_that_slice_is_conformant()
    test_validate_slice_fails_for_nonconformant_slice()
    test_validate_slice_rejects_unknown_slice()
    test_validate_without_slice_still_fails_on_legacy_debt()
    test_status_reports_structure_ok_for_conformant_task()
    test_status_reports_structure_invalid_without_failing()
    test_set_worktree_happy_path()
    test_set_worktree_upserts_existing_path()
    test_set_worktree_rejects_unknown_slice()
    test_set_worktree_clear_rejects_missing_repo()
    test_set_worktree_clear_rejects_path_and_clear()
    test_set_worktree_clear_rejects_missing_path_and_clear()
    test_set_worktree_clear_happy_path()
    test_set_worktree_clear_leaves_prs()
    test_set_worktree_clear_idempotent_without_worktree()
    test_set_worktree_clear_dry_run_no_mutation()
    test_yaml_store_clear_worktree()
    test_add_pr_happy_path_creates_repo_work()
    test_add_pr_upsert_by_url_keeps_latest_status()
    test_add_pr_rejects_unknown_slice()
    test_add_pr_after_set_worktree_preserves_worktree()
    test_show_renders_repo_work_worktree_and_prs()
    test_show_status_done_prints_set_worktrees_without_full_expand()
    test_sync_default_selection_and_status_override()
    test_add_slice_still_works_with_repo_work_in_schema()
    test_fs_task_store_round_trip()
    test_fs_task_store_ordering()
    test_fs_task_store_depends_on_symlink()
    test_fs_task_store_invalid_status_dies_on_read()
    test_persisted_models_document_every_field()
    test_yaml_task_store_round_trip_and_atomic_mutations()
    test_yaml_mutations_serialize_concurrent_writers()
    test_yaml_task_lock_lives_under_xdg_cache_not_task_dir()
    test_yaml_task_lock_path_resolves_aliases_to_same_inode_key()
    test_yaml_lifecycle_lock_preserves_first_executor()
    test_yaml_lock_serializes_concurrent_finish_and_release()
    test_yaml_rejects_duplicate_and_unknown_fields()
    test_profile_rejects_required_steps_absent_from_template()
    test_profile_requires_subagent_prompt_packet()
    test_profile_requires_task_record_discipline_packet()
    test_profile_requires_out_of_band_edit_warning_packet()
    test_profile_requires_next_action_packet()
    test_profile_requires_pick_next_slice_packet()
    test_warn_if_content_dirty_uses_profile_packet()
    test_profile_requires_sync_pipeline_instructions()
    test_profile_requires_cli_help()
    test_lifecycle_records_model_and_timestamps()
    test_finish_reconcile_succeeds_on_in_progress_without_start()
    test_finish_reconcile_refuses_planned_status()
    test_finish_reconcile_refuses_done_status()
    test_finish_without_start_still_fails_without_reconcile()
    test_finish_reconcile_requires_note()
    test_finish_note_appends_with_blank_line()
    test_finish_note_replace_overwrites()
    test_finish_replace_requires_note()
    test_finish_note_append_with_slice_only()
    test_finish_replace_refused_with_skip()
    test_finish_bare_refuses_when_multiple_unfinished_steps()
    test_finish_explicit_slice_step_ok_when_multiple_unfinished()
    test_finish_bare_ok_when_exactly_one_unfinished_step()
    test_set_slice_updates_title_and_goal()
    test_set_slice_requires_one_field()
    test_set_slice_refuses_done_slice()
    test_block_slice_sets_status_and_appends_note()
    test_block_slice_refuses_done()
    test_unblock_slice_restores_planned()
    test_unblock_slice_refuses_non_blocked()
    test_start_refuses_blocked_slice()
    test_vulnerability_scan_rejects_writer_model()
    test_vulnerability_scan_instruction_prefers_higher_reasoning_model()
    test_vulnerability_scan_rejects_unqualified_author_model()
    test_start_unqualified_model_error_includes_example()
    test_finish_slice_only_rejects_malformed_scan_timestamps()
    test_vulnerability_scan_can_record_user_declined_skip()
    test_slice_lifecycle_records_orchestrator_after_steps_finish()
    test_slice_skip_cannot_bypass_policy_governed_step()
    test_execution_issues_warn_for_legacy_completed_work()
    test_lifecycle_policy_is_step_key_agnostic()
    test_scaffold_empty_plan_has_no_slices()
    test_scaffold_initial_kind_setup_seeds_setup_slice()
    test_bootstrap_creates_initialized_task()
    test_bootstrap_prints_seed_slice_plans_for_implement_not_setup()
    test_add_slice_implement_prints_seed_block_for_new_slice_only()
    test_add_slice_setup_prints_no_seed_block()
    test_bootstrap_dry_run_prints_no_seed_block()
    test_seed_block_uses_task_placeholder_not_absolute_path()
    test_bootstrap_dry_run_prints_diff_and_does_not_write()
    test_bootstrap_refuses_overwrite_without_force()
    test_bootstrap_rejects_unknown_kind()
    test_bootstrap_rejects_unresolved_dependency()
    test_profile_show_json()
    test_schema_show_json()
    test_kinds_list_json()
    test_kinds_show_json()
    test_follow_work_kind_list_and_show()
    test_add_slice_follow_work_seeds_template_steps()
    test_validate_accepts_conformant_follow_work_fixture()
    test_set_project_mutation()
    test_set_project_title_updates_task_title()
    test_set_project_title_refuses_empty()
    test_set_context_mutation()
    test_add_decision_mutation()
    test_set_plan_note_mutation()
    test_remove_slice_removes_and_guards()
    test_create_from_requires_intent_path()
    test_bootstrap_rejects_initial_slice_key_without_kind()
    test_pi_job_write_stores_content_digest()
    test_hand_edit_warns_on_next_read()
    test_acknowledge_edit_clears_warning()
    test_finish_while_dirty_does_not_clear_digest()
    test_missing_digest_does_not_warn()
    test_markdown_representative_full_dump()
    test_markdown_contents_lists_all_slices_in_order()
    test_markdown_minimal_omits_empty_decisions_none()
    test_markdown_uninitialized_preview()
    test_markdown_escapes_and_quotes_metacharacters()
    test_markdown_current_badges()
    test_markdown_default_slice_order()
    test_markdown_chronological_sort()
    test_markdown_summary_omits_steps_and_context()
    test_markdown_slice_scopes_to_one_slice()
    test_markdown_summary_and_slice_are_mutually_exclusive()
    test_markdown_validation_failure()
    test_markdown_read_only()
    test_block_slice_gate_appends_depends_on()
    test_add_finding_appends_sidecar_not_yaml()
    test_yaml_task_layout_owns_plans_paths()
    test_add_decision_spills_long_note_to_plan_file()
    test_add_slice_creates_plan_stub()
    test_profile_requires_slice_plan_stub_and_findings_header()
    test_status_shows_blocked_and_interrupt_hint()
    test_render_orchestrator_heartbeat()
    test_loop_command_prints_heartbeat_without_task()
    test_loop_rejects_interval_flag()
    test_investigate_does_not_move_claim()
    print("pi-job tests passed")


def test_cue_task_path_is_rejected_without_cue() -> None:
    """A .cue task path must fail at storage selection without invoking CUE."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unsupported.cue"
        result = run(str(PI_JOB), "--task", str(task), "status", check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "unsupported task storage")
        assert_contains(result.stderr, "use a .yaml/.yml file or a directory")


@contextmanager
def _pi_job_tasks_home(home: Path) -> Iterator[None]:
    """Point `PI_JOB_TASKS` at `home` for the duration of the block, then restore it."""
    saved = os.environ.get("PI_JOB_TASKS")
    os.environ["PI_JOB_TASKS"] = str(home)
    try:
        yield
    finally:
        if saved is None:
            os.environ.pop("PI_JOB_TASKS", None)
        else:
            os.environ["PI_JOB_TASKS"] = saved


@contextmanager
def _pi_job_worktrees_home(home: Path) -> Iterator[None]:
    """Point `PI_JOB_WORKTREES` at `home` for the duration of the block, then restore it."""
    saved = os.environ.get("PI_JOB_WORKTREES")
    os.environ["PI_JOB_WORKTREES"] = str(home)
    try:
        yield
    finally:
        if saved is None:
            os.environ.pop("PI_JOB_WORKTREES", None)
        else:
            os.environ["PI_JOB_WORKTREES"] = saved


def test_project_loose_to_slug_bundle() -> None:
    """`project --to <slug>` converts a loose YAML task (+ stem-based `.plans/`) into a
    fresh `$PI_JOB_TASKS/<slug>/` bundle, keeping an extra sibling directory by name."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        legacy_dir = Path(tmp) / "legacy"
        legacy_dir.mkdir()
        source = legacy_dir / "pi-job-orchestrator-loop.yaml"
        write_task_yaml(source, standard_fixture_mapping(title="Orchestrator loop"))

        plans_dir = legacy_dir / "pi-job-orchestrator-loop.plans"
        plans_dir.mkdir()
        (plans_dir / "second-slice.md").write_text("# Plan\n", encoding="utf-8")
        (plans_dir / "_findings.md").write_text("# Findings\n", encoding="utf-8")

        attachments_dir = legacy_dir / "attachments"
        attachments_dir.mkdir()
        (attachments_dir / "diagram.png").write_bytes(b"fake-bytes")

        with _pi_job_tasks_home(home):
            out = run(
                str(PI_JOB), "--task", str(source), "project", "--to", "pi-job-orchestrator-loop",
            ).stdout
            assert_contains(out, "projected")
            assert_contains(out, "slug: pi-job-orchestrator-loop")

            bundle = home / "pi-job-orchestrator-loop"
            assert (bundle / "task.yaml").is_file()
            assert (bundle / "plans" / "second-slice.md").read_text(encoding="utf-8") == "# Plan\n"
            assert (bundle / "plans" / "_findings.md").is_file()
            assert (bundle / "references").is_dir()
            assert (bundle / "attachments" / "diagram.png").is_file()

            task = module.YamlTaskStore(module.BundleTaskLayout(bundle)).read()
            assert task["title"] == "Orchestrator loop"

            assert not source.exists()
            assert not plans_dir.exists()


def test_project_sibling_files_to_references() -> None:
    """Sibling files next to a loose YAML task land under the bundle's `references/`,
    preserving filenames."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        source = Path(tmp) / "legacy.task.yaml"
        write_task_yaml(source, standard_fixture_mapping())
        (Path(tmp) / "intent.md").write_text("# Intent\n", encoding="utf-8")
        (Path(tmp) / "notes.txt").write_text("notes\n", encoding="utf-8")

        with _pi_job_tasks_home(home):
            run(str(PI_JOB), "--task", str(source), "project", "--to", "ref-slug")
            bundle = home / "ref-slug"
            assert (bundle / "references" / "intent.md").read_text(encoding="utf-8") == "# Intent\n"
            assert (bundle / "references" / "notes.txt").read_text(encoding="utf-8") == "notes\n"


def test_project_other_dirs_keep_names() -> None:
    """Sibling directories other than `<stem>.plans/` are copied to the bundle root
    under their own name (not merged into `plans/` or `references/`)."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        source = Path(tmp) / "legacy.task.yaml"
        write_task_yaml(source, standard_fixture_mapping())
        extra_dir = Path(tmp) / "screenshots"
        extra_dir.mkdir()
        (extra_dir / "shot.png").write_bytes(b"data")
        nested = extra_dir / "nested"
        nested.mkdir()
        (nested / "deep.txt").write_text("deep\n", encoding="utf-8")

        with _pi_job_tasks_home(home):
            run(str(PI_JOB), "--task", str(source), "project", "--to", "dirs-slug")
            bundle = home / "dirs-slug"
            assert (bundle / "screenshots" / "shot.png").is_file()
            assert (bundle / "screenshots" / "nested" / "deep.txt").read_text(encoding="utf-8") == "deep\n"


def test_project_deletes_yaml_and_plans_only() -> None:
    """After a successful project, only the source yaml and its `<stem>.plans/` are
    deleted; other copied sibling dirs/files remain at the source parent."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        source = Path(tmp) / "legacy.task.yaml"
        write_task_yaml(source, standard_fixture_mapping())
        plans_dir = Path(tmp) / "legacy.task.plans"
        plans_dir.mkdir()
        (plans_dir / "notes.md").write_text("plan\n", encoding="utf-8")
        extra_dir = Path(tmp) / "keepme"
        extra_dir.mkdir()
        (extra_dir / "file.txt").write_text("keep\n", encoding="utf-8")
        extra_file = Path(tmp) / "sidecar.md"
        extra_file.write_text("sidecar\n", encoding="utf-8")

        with _pi_job_tasks_home(home):
            out = run(str(PI_JOB), "--task", str(source), "project", "--to", "del-slug").stdout
            assert_contains(out, f"removed {source.resolve()}")
            assert_contains(out, f"removed {plans_dir.resolve()}")

            assert not source.exists()
            assert not plans_dir.exists()
            assert extra_dir.is_dir()
            assert (extra_dir / "file.txt").is_file()
            assert extra_file.is_file()

            bundle = home / "del-slug"
            assert (bundle / "keepme" / "file.txt").is_file()
            assert (bundle / "references" / "sidecar.md").is_file()


def test_project_refuses_existing_dest() -> None:
    """`project` refuses when the destination bundle's `task.yaml` already exists (no
    `--force`); the existing bundle is left untouched and the source is not deleted."""
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        with _pi_job_tasks_home(home):
            run(str(PI_JOB), "--task", "existing-slug", "create", "--kind", "setup")
            source = Path(tmp) / "legacy.task.yaml"
            write_task_yaml(source, standard_fixture_mapping())

            result = run(
                str(PI_JOB), "--task", str(source), "project", "--to", "existing-slug", check=False,
            )
            assert result.returncode != 0
            assert_contains(result.stderr, "already exists")
            assert source.is_file()

            status = run(str(PI_JOB), "--task", "existing-slug", "status").stdout
            assert_contains(status, "Initialization: ok")


def test_project_refuses_non_loose_source() -> None:
    """`project` refuses a bundle or directory-store source; only loose YAML may be
    converted."""
    with tempfile.TemporaryDirectory() as tmp:
        bundle_source = Path(tmp) / "bundle-source"
        run(str(PI_JOB), "--task", str(bundle_source), "create", "--kind", "setup")
        result = run(
            str(PI_JOB), "--task", str(bundle_source), "project", "--to", "wont-happen", check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "loose YAML")

        fs_source = Path(tmp) / "fs-source"
        fs_source.mkdir()
        result2 = run(
            str(PI_JOB), "--task", str(fs_source), "project", "--to", "wont-happen-either", check=False,
        )
        assert result2.returncode != 0
        assert_contains(result2.stderr, "loose YAML")


def test_project_rejects_loose_yaml_as_to() -> None:
    """`--to` naming a loose `*.yaml` file (not `task.yaml`) is rejected with a slug or
    bundle-directory hint; nothing is created and the source is left in place."""
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "legacy.task.yaml"
        write_task_yaml(source, standard_fixture_mapping())
        bad_to = Path(tmp) / "legacy-target.yaml"

        result = run(
            str(PI_JOB), "--task", str(source), "project", "--to", str(bad_to), check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "loose YAML file")
        assert_contains(result.stderr, "legacy-target")
        assert not bad_to.exists()
        assert source.is_file()


def test_project_semantic_mismatch_rolls_back() -> None:
    """A forced semantic-equality failure after `project` rolls back the freshly
    scaffolded destination bundle entirely and leaves the loose source untouched."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        home = Path(tmp) / "tasks-home"
        source = Path(tmp) / "legacy.task.yaml"
        write_task_yaml(source, module.example_task_mapping())

        with _pi_job_tasks_home(home):
            original = module.semantic_task_mapping
            calls = {"n": 0}

            def flaky(value, *, source):
                calls["n"] += 1
                if calls["n"] == 2:
                    return {"forced": "mismatch"}
                return original(value, source=source)

            module.semantic_task_mapping = flaky
            try:
                args = argparse.Namespace(task=source.resolve(), to="mismatch-slug")
                try:
                    module.cmd_project(args)
                    raise AssertionError("expected SystemExit for forced semantic mismatch")
                except SystemExit as exc:
                    assert exc.code != 0
            finally:
                module.semantic_task_mapping = original

            bundle = home / "mismatch-slug"
            assert not bundle.exists()
            assert source.is_file()


if __name__ == "__main__":
    test_cue_task_path_is_rejected_without_cue()
    test_project_loose_to_slug_bundle()
    test_project_sibling_files_to_references()
    test_project_other_dirs_keep_names()
    test_project_deletes_yaml_and_plans_only()
    test_project_refuses_existing_dest()
    test_project_refuses_non_loose_source()
    test_project_rejects_loose_yaml_as_to()
    test_project_semantic_mismatch_rolls_back()
    test_task_tasks_home_default()
    test_task_tasks_home_override()
    test_resolve_task_arg_slug()
    test_resolve_task_arg_unknown_slug()
    test_resolve_task_arg_invalid_charset()
    test_resolve_task_arg_path_loose_yaml_unchanged()
    test_resolve_task_arg_path_bundle_dir_unchanged()
    test_resolve_task_arg_slug_ignores_loose_yaml_in_home()
    test_layout_for_document_path_bundle_and_loose()
    test_derive_bundle_root_task_yaml_parent_dir_self_and_loose_dies()
    test_scaffold_bundle_dirs_idempotent_preserves_contents()
    test_bundle_read_write_and_plan_stub()
    test_store_describe_uses_layout()
    test_bundle_slug_under_home_pure()
    test_task_display_ref_home_bundle()
    test_task_display_ref_loose_yaml()
    test_task_display_ref_outside_bundle()
    test_status_task_line_uses_display_ref()
    test_instruction_plan_pointer_bundle()
    test_instruction_plan_pointer_loose()
    test_markdown_plan_label_bundle()
    test_markdown_plan_label_loose()
    test_create_slug_scaffolds_bundle()
    test_create_path_scaffolds_bundle()
    test_create_duplicate_slug()
    test_create_invalid_slug()
    test_create_rejects_loose_yaml_path()
    test_list_home_bundles_only()
    test_derived_task_status_ignores_stored_field()
    test_status_and_list_use_derived_task_status()
    test_list_row_fields()
    test_list_orders_status_groups_then_cursor_activity()
    test_list_activity_falls_back_to_mtime_and_ties_use_slug()
    test_list_renders_each_cursor_on_its_own_line()
    test_list_empty_home()
    test_list_respects_PI_JOB_TASKS()
    test_list_skips_unreadable_bundle_with_warning()
    test_set_worktree_recommend_missing_path()
    test_set_worktree_recommend_under_home()
    test_set_worktree_recommend_loose_yaml()
    test_set_worktree_help_mentions_worktree_convention()
    main()
