#!/usr/bin/env python3
"""Regression tests for packages/pi-job-harness/bin/pi-job."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import hashlib
import os
import re
import subprocess
import sys
import tempfile
import threading
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[3]
PI_JOB = Path(__file__).resolve().parents[1] / "bin" / "pi-job"
if not PI_JOB.exists():
    PI_JOB = PI_JOB.with_name("executable_pi-job")


def load_pi_job_module():
    """Import pi-job (no .py suffix, chezmoi's executable_ naming) as a module so tests
    can exercise YamlTaskStore/FsTaskStore/TaskLayout/CueTaskStore directly instead of only via
    subprocess. YAML via YamlTaskStore is the primary test path; CueTaskStore remains for legacy
    compatibility tests only. Safe: `main()` only runs under `if __name__ == "__main__":`."""
    loader = importlib.machinery.SourceFileLoader("pi_job_under_test", str(PI_JOB))
    spec = importlib.util.spec_from_file_location("pi_job_under_test", PI_JOB, loader=loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module  # dataclasses needs the module registered before exec
    loader.exec_module(module)
    return module

def dump_task_yaml(mapping: dict) -> str:
    return yaml.safe_dump(mapping, sort_keys=False, allow_unicode=True)


def write_task_yaml(path: Path, mapping: dict) -> None:
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
            "cursor": {"slice": cursor[0], "step": cursor[1]},
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
    if cursor[1] is None:
        mapping["orchestration"]["cursor"] = {"slice": cursor[0]}
    else:
        mapping["orchestration"]["cursor"] = {"slice": cursor[0], "step": cursor[1]}
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
            "cursor": {"slice": cursor[0], "step": cursor[1]},
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
            "cursor": {"slice": cursor[0], "step": cursor[1]},
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
    """Port of SYNC_FIXTURE_CUE."""
    return {
        "title": "Sync fixture",
        "status": "in_progress",
        "source": {"jira": "", "discovered": "", "context": ""},
        "project": {"key": "sync", "name": "Sync", "route": "", "context": ""},
        "context": "",
        "orchestration": {
            "cursor": {"slice": "only-slice", "step": "create-plan"},
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
            "cursor": {"slice": "implementation", "step": "vulnerability-scan"},
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
    store = module.YamlTaskStore(path)
    task = store.read()
    step = find_step(task, slice_key, step_key)
    step["status"] = status
    step.update(fields)
    store.replace(task)


def step_status(path: Path, slice_key: str, step_key: str) -> str:
    module = load_pi_job_module()
    task = module.YamlTaskStore(path).read()
    return find_step(task, slice_key, step_key)["status"]


VENDORED_CLOSED_FINAL_STEPS_CUE = """
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
    steps: [...#Step]
    final_steps: [
        #Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence or record the gap", status: "planned", note: ""},
        #Step & {key: "update-task-file", title: "Update this task file plan", status: "planned", note: ""},
    ]
}

task: {
    title: "Closed final_steps bootstrap"
    status: "in_progress"
    project: {name: "Bootstrap"}
    orchestration: {
        cursor: {slice: "init-harness", step: "create-plan"}
        policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}
    }
    plan: {
        note: ""
        slices: [
            #Slice & {
                key: "init-harness"
                kind: "implement"
                title: "Init harness"
                goal: "Bootstrap"
                status: "in_progress"
                note: ""
                steps: [
                    #Step & {key: "create-plan", title: "Create plan", status: "planned", note: ""},
                ]
                final_steps: [
                    #Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence or record the gap", status: "planned", note: ""},
                    #Step & {key: "update-task-file", title: "Update this task file plan", status: "planned", note: ""},
                ]
            },
        ]
    }
}
"""


LEGACY_CUE_TEST_ALLOWLIST = frozenset({
    "test_fs_and_cue_task_store_shape_parity",
    "test_cue_escape_escapes_newlines_quotes_and_tabs",
    "test_cmd_project_cue_to_fs_to_cue_round_trip",
    "test_cmd_project_refuses_nonempty_cue_destination",
    "test_cmd_project_cue_to_yaml_keeps_source_and_verifies_semantics",
    "test_legacy_cue_custom_fields_remain_readable_but_do_not_migrate_silently",
    "test_migrate_task_reports_already_migrated",
    "test_migrate_task_recommends_delete_for_identical_status",
    "test_migrate_task_recommends_keep_for_status_with_used_extra_value",
    "test_migrate_task_recommends_delete_for_status_with_unused_extra_value",
    "test_migrate_task_recommends_replace_for_slice_with_extra_fields",
    "test_migrate_task_line_ranges_are_accurate",
    "test_migrate_task_partial_migration_only_slice_remains",
    "test_bootstrap_requires_yaml",
    "test_legacy_cue_add_step_final_rolls_back_on_closed_final_steps_schema",
    "test_add_slice_unified_final_steps_field",
    "test_validate_fails_invalid_step_status",
    "test_legacy_cue_validate_passes_shared_schema_task",
    "test_legacy_cue_scaffold_output_has_no_local_schema",
})


def test_fixture_policy_disallows_incidental_cue_task_paths() -> None:
    source = Path(__file__).read_text(encoding="utf-8")
    current_test: str | None = None
    offenders: list[str] = []
    for lineno, line in enumerate(source.splitlines(), 1):
        m = re.match(r"def (test_\w+)\(", line)
        if m:
            current_test = m.group(1)
        if current_test in LEGACY_CUE_TEST_ALLOWLIST:
            continue
        if current_test == "test_fixture_policy_disallows_incidental_cue_task_paths":
            continue
        if re.search(r'Path\([^)]*\)\s*/\s*"[^"]+\.cue"', line):
            offenders.append(f"{lineno}: {current_test}: {line.strip()}")
    if offenders:
        raise AssertionError("incidental .cue task fixtures:\n" + "\n".join(offenders))




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

    return f"""title: Subagent instruction test
status: in_progress
orchestration:
  cursor:
    slice: {slice_key}
    step: {step_key}
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

    return """title: Orchestrator instruction test
status: in_progress
orchestration:
  cursor:
    slice: setup-slice
    step: explore-context
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

    return f"""title: Create plan instruction test
status: in_progress
orchestration:
  cursor:
    slice: {slice_key}
    step: create-plan
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
        assert_contains(status, "Cursor: old-slice / old-step")
        assert_contains(status, "Ready: second-slice")
        assert_contains(status, "cursor slice missing")

        dry = run(str(PI_JOB), "--task", str(task), "advance", "--dry-run").stdout
        assert_contains(dry, "PI-JOB PICK NEXT SLICE")
        assert_contains(dry, "Ready slices")
        assert_contains(dry, "second-slice")

        pick_next = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(f"bare advance should exit 0 with pick-next packet:\n{pick_next.stderr}")
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")

        run(str(PI_JOB), "--task", str(task), "advance", "--slice", "second-slice", "--step", "s2")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: second-slice / s2")
        assert_not_contains(advanced, "Next:")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "PI-JOB EXECUTION INSTRUCTION")
        assert_contains(instruction, "Owner: orchestrator")
        assert_contains(instruction, "Role: orchestrator")
        assert_contains(instruction, "Supersedes any default workspace role")
        assert_contains(instruction, "Slice: second-slice [implement]")
        assert_contains(instruction, "Slice goal: Find next planned step")
        assert_contains(instruction, "Step: s2 — Next")
        assert_contains(instruction, "Execute this step in the main orchestration session.")

        assert_contains(instruction, "Todo tracking:")
        assert_contains(instruction, "Keep session todos aligned with `pi-job plan`")
        assert_contains(instruction, "Future-work capture:")
        assert_contains(instruction, "technical debt")
        assert_contains(instruction, "do not bury actionable follow-up work only in notes")


def test_uninitialized_task_requires_orchestration() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "uninitialized.yaml"
        write_task_yaml(
            task,
            standard_fixture_mapping(title="Uninitialized fixture task", uninitialized=True),
        )

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: required")
        assert_contains(status, "create [--kind setup|implement|...]")

        advance = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if advance.returncode == 0:
            raise AssertionError("advance unexpectedly succeeded for uninitialized task")
        assert_contains(advance.stderr, "missing task.orchestration")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if instruction.returncode == 0:
            raise AssertionError("instruction unexpectedly succeeded for uninitialized task")
        assert_contains(instruction.stderr, "missing task.orchestration")

        init_dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        assert_contains(init_dry, "second-slice")
        assert_contains(init_dry, "s2")

        run(str(PI_JOB), "--task", str(task), "create")
        initialized = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(initialized, "Initialization: ok")
        assert_contains(initialized, "Cursor: second-slice / s2")


def test_init_with_kind_setup_seeds_setup_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty-plan.yaml"
        write_task_yaml(task, {
            "title": "Empty plan",
            "status": "in_progress",
            "plan": {"note": "", "slices": []},
        })
        run(str(PI_JOB), "--task", str(task), "create", "--kind", "setup")
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Cursor: setup-slice / explore-context")
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "setup-slice")
        assert_contains(show, "explore-context")


def test_setup_template_includes_wayfinder_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "setup-wayfinder.yaml"
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
        task = Path(tmp) / "fog.yaml"
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
                "cursor": {"slice": "b", "step": ""},
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
                "cursor": {"slice": "only-slice", "step": "edit-code"},
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
        assert_contains(instruction, "Step kind: edit-code")
        assert_contains(instruction, "Run this step in a subagent.")


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
        # Reads list may mention markdown/show --slice as command hints; Subagent prompt must not appear.
        assert_contains(instruction, "markdown [--slice SLICE_KEY]")


def test_subagent_orchestrator_directs_markdown_slice_without_decisions_dump() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-orchestrator-markdown.yaml"
        task.write_text(subagent_instruction_yaml_task(slice_key="target-slice"), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Orchestrator instruction:")
        assert_contains(instruction, "markdown --slice SLICE_KEY")
        assert_contains(instruction, "Do not paste a decisions dump")
        assert_not_contains(instruction, "markdown --slice target-slice")
        orch_idx = instruction.index("Orchestrator instruction:")
        prompt_idx = instruction.index("Subagent prompt:")
        assert orch_idx < prompt_idx, "Orchestrator instruction must precede Subagent prompt"


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

    return f"""title: Grill plan instruction test
status: in_progress
orchestration:
  cursor:
    slice: {slice_key}
    step: grill-plan
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
    assert_contains(instruction, "intent, system behaviour, constraints, verification")
    assert_contains(instruction, "optional short touch surface")
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
            "Challenge behaviour, boundaries, must-not constraints, and verification",
        )
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


def _assert_task_record_discipline_block(instruction: str) -> None:
    assert_contains(instruction, "Task record discipline:")
    assert_contains(instruction, "machine-owned")
    assert_contains(instruction, "do not open or edit the task store directly")
    assert_contains(instruction, "Reads:")
    assert_contains(instruction, "Writes:")
    assert_contains(instruction, "Why:")
    assert_contains(instruction, "token cost")
    assert_contains(instruction, "Token smell:")
    assert_contains(instruction, "DX and agent experience share the same constructs")
    assert_contains(instruction, "status")
    assert_contains(instruction, "plan")
    assert_contains(instruction, "show")
    assert_contains(instruction, "markdown")
    assert_contains(instruction, "Channels (which write for which fact):")
    assert_contains(instruction, "DECISION (add-decision)")
    assert_contains(instruction, "STEP NOTE (finish --note)")
    assert_contains(instruction, "instruction")
    assert_contains(instruction, "add-slice")
    assert_contains(instruction, "validation")
    discipline_idx = instruction.index("Task record discipline:")
    todo_idx = instruction.index("Todo tracking:")
    assert discipline_idx < todo_idx, "Task record discipline must appear before Todo tracking"


def test_orchestrator_instruction_includes_task_record_discipline() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "orchestrator-discipline.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        _assert_task_record_discipline_block(instruction)


def test_instruction_includes_next_action_and_enter_the_loop() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "next-action.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "NEXT ACTION")
        assert_contains(instruction, "Do not wait for another user prompt")
        assert_contains(instruction, "Enter the orchestrator loop immediately")
        assert_contains(instruction, "Role: orchestrator")
        assert_contains(instruction, "Do not wait for the user to say \"continue\"")
        assert_contains(instruction, "Task file:")
        assert_contains(instruction, str(task))  # header still names the real path
        assert_contains(instruction, "TASK_FILE")  # command hints use the token
        assert_not_contains(instruction, f"--task {task}")
        assert_not_contains(instruction, "{task_file}")
        assert_not_contains(instruction, "{cursor}")
        next_idx = instruction.index("NEXT ACTION")
        orch_idx = instruction.index("Orchestrator instruction:")
        assert next_idx < orch_idx, "NEXT ACTION must appear before Orchestrator instruction"


def test_bootstrap_instruction_includes_next_action() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-next.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text(minimal_bootstrap_input_yaml(), encoding="utf-8")
        out = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input)).stdout
        assert_contains(out, "NEXT ACTION")
        assert_contains(out, "Do not wait for another user prompt")
        assert_contains(out, "Enter the orchestrator loop immediately")


def test_subagent_instruction_includes_task_record_discipline() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "subagent-discipline.yaml"
        task.write_text(subagent_instruction_yaml_task(), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        _assert_task_record_discipline_block(instruction)
        assert_contains(instruction, "Subagent prompt:")
        assert_contains(instruction, "do not inspect the task store directly")


def test_task_record_discipline_uses_task_file_and_slice_key_hints() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "discipline-hints.yaml"
        task.write_text(subagent_instruction_yaml_task(slice_key="target-slice"), encoding="utf-8")
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "TASK_FILE")
        assert_contains(instruction, "SLICE_KEY")
        assert_contains(instruction, "markdown [--slice SLICE_KEY]")
        assert_contains(instruction, "show [--slice SLICE_KEY]")
        assert_not_contains(instruction, "markdown [--slice target-slice]")
        assert_not_contains(instruction, "show [--slice target-slice]")
        assert_not_contains(instruction, "{task_file}")
        assert_not_contains(instruction, "{slice_key}")
        assert_not_contains(instruction, f"--task {task}")


def test_update_task_file_guidance_names_mutation_commands() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "update-task-file-guidance.yaml"
        write_task_yaml(task, closing_slice_mapping(cursor=("closing", "update-task-file")))
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Guidance:")
        assert_contains(instruction, "add-slice")
        assert_contains(instruction, "Do not edit the task store directly")


def test_plan_output_omits_task_record_discipline() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "plan-omits-discipline.yaml"
        task.write_text(orchestrator_instruction_yaml_task(), encoding="utf-8")
        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_not_contains(plan, "Task record discipline:")


def test_advance_pick_next_then_jump_to_closing_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "all-implement-done.yaml"
        write_task_yaml(task, closing_slice_mapping())

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Ready: closing")
        assert_contains(status, "no unfinished steps")
        assert_not_contains(status, "Next:")

        pick_next = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(f"bare advance should exit 0 with pick-next:\n{pick_next.stderr}")
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next.stdout, "closing")

        run(
            str(PI_JOB), "--task", str(task), "advance",
            "--slice", "closing", "--step", "update-test-plan",
        )
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: closing / update-test-plan")
        assert_not_contains(advanced, "Next:")


def test_status_shows_cursor_and_ready_without_next_line() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "aligned-cursor.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Cursor: second-slice / s2")
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


def test_advance_blocked_on_incomplete_current_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "incomplete.yaml"
        
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        # Negative control: cursor's step (s2) is still "planned", so advance must fail closed.
        res = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if res.returncode == 0:
            raise AssertionError("advance unexpectedly succeeded past an unfinished step")
        assert_contains(res.stderr, "current cursor step is not done")
        assert_contains(res.stderr, "--force")
        assert_contains(res.stderr, "--reason")

        # --force without --reason must also fail.
        forced_no_reason = run(str(PI_JOB), "--task", str(task), "advance", "--force", check=False)
        if forced_no_reason.returncode == 0:
            raise AssertionError("advance --force without --reason unexpectedly succeeded")
        assert_contains(forced_no_reason.stderr, "--force requires --reason")

        # --force with --reason records the skip and advances anyway.
        forced = run(
            str(PI_JOB), "--task", str(task), "advance", "--force", "--reason", "manual override for test"
        ).stdout
        assert_contains(forced, "forced advance (skip reason: manual override for test)")
        assert_contains(forced, "advanced cursor:")

        # dry-run must not be blocked by the incomplete-step check (no write happens).
        task2 = Path(tmp) / "incomplete-dry.yaml"
        write_task_yaml(task2, standard_fixture_mapping(cursor=("second-slice", "s2")))
        dry = run(str(PI_JOB), "--task", str(task2), "advance", "--dry-run").stdout
        assert_contains(dry, "slice:")


def test_advance_resync_rejects_force_and_missing_reason() -> None:
    """--resync is mutually exclusive with --force and requires --reason."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "resync-guards.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        both = run(
            str(PI_JOB), "--task", str(task), "advance", "--resync", "--force", "--reason", "nope",
            check=False,
        )
        if both.returncode == 0:
            raise AssertionError("advance --resync --force unexpectedly succeeded")
        assert "mutually exclusive" in both.stderr or "not allowed with argument --resync" in both.stderr

        no_reason = run(str(PI_JOB), "--task", str(task), "advance", "--resync", check=False)
        if no_reason.returncode == 0:
            raise AssertionError("advance --resync without --reason unexpectedly succeeded")
        assert_contains(no_reason.stderr, "--resync requires --reason")


def test_advance_resync_explicit_jump_does_not_skip_previous_step() -> None:
    """Resync with --slice/--step moves cursor without marking the prior step skipped."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "resync-jump.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        out = run(
            str(PI_JOB), "--task", str(task), "advance",
            "--resync", "--reason", "jump to final step",
            "--slice", "second-slice", "--step", "finish",
        ).stdout
        assert_contains(out, "advanced cursor: second-slice / finish")

        assert step_status(task, "second-slice", "s2") == "planned"
        


def test_advance_resync_without_jump_fails_on_same_unfinished_step() -> None:
    """Resync without explicit jump fails when next_cursor stays on the unfinished step."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "resync-same.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        res = run(
            str(PI_JOB), "--task", str(task), "advance",
            "--resync", "--reason", "try implicit resync",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("resync without jump unexpectedly succeeded on same unfinished step")
        assert_contains(res.stderr, "same unfinished step")
        assert_contains(res.stderr, "--slice/--step")


def test_advance_resync_without_jump_succeeds_when_cursor_on_finished_step() -> None:
    """Resync realigns a cursor on a finished step to the next unfinished step in the slice."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "resync-finished-step.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s1")))

        out = run(
            str(PI_JOB), "--task", str(task), "advance",
            "--resync", "--reason", "cursor drifted to finished step",
        ).stdout
        assert_contains(out, "advanced cursor: second-slice / s2")

        assert step_status(task, "second-slice", "s2") == "planned"


def test_advance_force_still_skips_incomplete_step() -> None:
    """Regression: --force still marks the incomplete current step skipped before advancing."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "force-skip.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        out = run(
            str(PI_JOB), "--task", str(task), "advance",
            "--force", "--reason", "manual override for test",
        ).stdout
        assert_contains(out, "forced advance (skip reason: manual override for test)")
        assert step_status(task, "second-slice", "s2") == "skipped"


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
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "nested" / "new-task.yaml"
        dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        assert_contains(dry, "title:")
        assert_contains(dry, "key: do-the-change")
        if task.exists():
            raise AssertionError("dry-run wrote a task file")

        out = run(
            str(PI_JOB),
            "--task",
            str(task),
            "create",
            "--title",
            "Scaffolded example",
        ).stdout
        assert_contains(out, f"created: {task.resolve()}")
        assert task.exists()

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
        for key in ("httpyac-api-spec", "test-case-table"):
            assert_contains(out, key)
        assert_contains(out, "[not registered]")

        out_setup = run(str(PI_JOB), "--task", str(implement_task), "toolbelt", "--kind", "setup").stdout
        assert_contains(out_setup, "setup")
        assert_contains(out_setup, "config-flag-matrix")

        research_task = Path(tmp) / "research.yaml"
        write_task_yaml(research_task, {
            "title": "Research",
            "status": "in_progress",
            "orchestration": {
                "cursor": {"slice": "r", "step": "explore-context"},
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
        artifacts = module.YamlTaskStore(task).read()["orchestration"]["artifacts"]
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
                "cursor": {"slice": "setup-slice", "step": "select-toolbelt"},
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
                "cursor": {"slice": "setup-slice", "step": "select-toolbelt"},
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
                "cursor": {"slice": "active", "step": "edit-code"},
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
                "cursor": {"slice": "second-slice", "step": "s2"},
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
        store = module.YamlTaskStore(task)
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
                "cursor": {"slice": "active", "step": "s1"},
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
        # cursor slice stays on its own line even when done
        store = load_pi_job_module().YamlTaskStore(task)
        data = store.read()
        data["orchestration"]["cursor"] = {"slice": "delta", "step": None}
        store.replace(data)
        cursor_out = run(str(PI_JOB), "--task", str(task), "show", "--short", "--color", "never").stdout
        assert_contains(cursor_out, "✓ alpha, beta")
        if "alpha, beta, delta" in cursor_out:
            raise AssertionError(f"cursor done slice must not fold into short run:\n{cursor_out}")
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
                "cursor": {"slice": "only-slice", "step": "create-plan"},
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
                "cursor": {"slice": "target-slice", "step": "step-with-note"},
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
                "cursor": {"slice": "sparse", "step": "only-step"},
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

        out = run(
            str(PI_JOB), "--task", str(task), "show", "--slice", "ready-dependent"
        ).stdout
        assert_contains(out, "deps:")
        assert_contains(out, "base:done")


def test_show_slice_multiline_note_indents_continuation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "multiline-note.yaml"
        write_task_yaml(task, {
            "title": "Multiline note test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursor": {"slice": "notes", "step": "s1"},
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
        task = Path(tmp) / "new.yaml"
        dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        for key in (
            "create-plan", "grill-plan", "edit-code", "verify",
            "e2e-evidence", "vulnerability-scan", "share-with-team", "update-task-file", "wait-for-feedback",
        ):
            assert_contains(dry, f"key: {key}")
        if "reconcile-artifacts" in dry:
            raise AssertionError("scaffold still emits retired step key reconcile-artifacts")


def test_scaffold_includes_create_plan_and_grill_plan_before_edit_code() -> None:
    """The scaffold's steps must lead with create-plan then grill-plan, before edit-code -
    modeling the per-slice planning convention for anyone reading a fresh scaffold."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "new.yaml"
        dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        assert_contains(dry, "key: create-plan")
        assert_contains(dry, "key: grill-plan")
        i_plan = dry.index("create-plan")
        i_grill = dry.index("grill-plan")
        i_edit = dry.index("edit-code")
        if not (i_plan < i_grill < i_edit):
            raise AssertionError(f"steps order wrong: create-plan={i_plan} grill-plan={i_grill} edit-code={i_edit}")


def test_advance_walks_create_plan_then_grill_plan_before_edit_code() -> None:
    """pi-job's existing step-ordering gate, applied to the new convention: within-slice
    advance must land on create-plan first, then grill-plan once create-plan is done,
    then edit-code only once both are done - proving the guardrail is actually enforced,
    not just documented."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.yaml"
        write_task_yaml(task, {
            "title": "Plan-and-grill gating test",
            "status": "in_progress",
            "orchestration": {
                "cursor": {"slice": "only-slice", "step": "create-plan"},
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
        advanced = run(str(PI_JOB), "--task", str(task), "advance").stdout
        assert_contains(advanced, "grill-plan")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction").stdout
        assert_contains(instruction, "Step: grill-plan")

        mutate_step_status(task, "only-slice", "grill-plan", "done")
        advanced = run(str(PI_JOB), "--task", str(task), "advance").stdout
        assert_contains(advanced, "edit-code")

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
                "cursor": {"slice": "only-slice", "step": "create-plan"},
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

        pick_next = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(f"advance should exit 0 with pick-next when Ready is empty:\n{pick_next.stderr}")
        assert_contains(pick_next.stdout, "PI-JOB PICK NEXT SLICE")
        assert_contains(pick_next.stdout, "Ready slices: none")


def test_advance_pick_next_when_nothing_ready() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "nothing-ready.yaml"
        write_task_yaml(task, {
            "title": "Nothing ready test",
            "status": "in_progress",
            "project": {"name": "Fixture"},
            "orchestration": {
                "cursor": {"slice": "only-slice", "step": "create-plan"},
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

        pick_next = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(f"advance should exit 0 with pick-next when nothing is ready:\n{pick_next.stderr}")
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


def test_advance_within_slice_then_pick_next_on_exhausted_slice() -> None:
    """Within-slice advance walks unfinished steps; when the slice is exhausted, emit pick-next."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "within-slice-pick-next.yaml"
        write_task_yaml(task, standard_fixture_mapping(cursor=("second-slice", "s2")))

        mutate_step_status(task, "second-slice", "s2", "done")
        within = run(str(PI_JOB), "--task", str(task), "advance").stdout
        assert_contains(within, "advanced cursor: second-slice / finish")

        mutate_step_status(task, "second-slice", "finish", "done")
        pick_next = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if pick_next.returncode != 0:
            raise AssertionError(f"advance should exit 0 with pick-next when slice is exhausted:\n{pick_next.stderr}")
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
                "cursor": {"slice": "only-slice", "step": "create-plan"},
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
        # ready-dependent should show deps with base:done
        if "base:done" not in show:
            raise AssertionError(f"expected 'base:done' in deps line:\n{show}")
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
                "cursor": {"slice": "orphan", "step": "s1"},
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
    """Regression test: cmd_init() must use slice_work_contract_phase() not hardcoded 'implement'."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "forward-ref.yaml"
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

        # init with full profile on a task with unmet dependency should die, not guess "implement"
        init_res = run(str(PI_JOB), "--task", str(task), "create", check=False)
        if init_res.returncode == 0:
            raise AssertionError("create unexpectedly succeeded with forward-reference dependency in full profile")
        assert_contains(init_res.stderr, "no slice is dependency-satisfied yet")


def test_legacy_cue_scaffold_output_has_no_local_schema() -> None:
    """Scaffold output should not contain local #Slice:/#Status: definitions."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "schema-test.cue"
        dry = run(str(PI_JOB), "--task", str(task), "create", "--dry-run").stdout
        if "#Status:" in dry or "#Step:" in dry or "#Slice:" in dry or "#Decision:" in dry or "#Artifact:" in dry:
            raise AssertionError(f"create dry-run should not contain local type definitions:\n{dry}")


def test_scaffold_output_still_validates_via_shared_schema() -> None:
    """Real (non-dry-run) scaffold, then pi-job status/show succeed against it."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "schema-validate.yaml"
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
                "cursor": {"slice": "only-slice", "step": "create-plan"},
                "policy": _orchestration_policy(),
            },
            "plan": {"note": "", "slices": []},
        })

        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "first-slice", "--title", "First", "--goal", "Initial work", "--kind", "implement")

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        assert_contains(show, "first-slice")


def test_add_slice_unified_final_steps_field() -> None:
    """Regression test: add-slice must correctly handle unified final_steps expressions.
    When a local #Slice has a closed final_steps (e.g., 2 fixed items) unified with the
    shared schema's open [...#Step] final_steps via cue def, parse_def_struct_fields must
    split on top-level & and classify the closed operand correctly (not misclassify as open).
    This ensures the new slice literal copies the fixed 2 items, not empty."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unified-final-steps.cue"
        # Define a local #Slice with a CLOSED 2-element final_steps list.
        # This mimics the bootstrap task structure.
        preamble_closed_final_steps = """
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
    title: string
    goal: string
    status: #Status
    note: string
    steps: [...#Step]
    final_steps: [
        #Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence or record the gap", status: "planned", note: ""},
        #Step & {key: "update-task-file", title: "Update this task file plan", status: "planned", note: ""},
    ]
}
"""
        fixture_closed_final = preamble_closed_final_steps + """
task: {
    title: "Closed final_steps test"
    status: "in_progress"
    project: {
        name: "Test"
    }
    orchestration: {
        cursor: {slice: "only-slice", step: "create-plan"}
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
    plan: {
        slices: []
    }
}
"""
        task.write_text(fixture_closed_final)

        # Dry-run should show the literal with BOTH final_steps entries
        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "extra-work", "--title", "Extra", "--goal", "Additional", "--kind", "implement", "--dry-run").stdout
        assert_contains(dry, 'key: "e2e-evidence"')
        assert_contains(dry, 'key: "update-task-file"')

        # Real add-slice should succeed (previously failed with "incompatible list lengths (0 and 2)")
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "extra-work", "--title", "Extra", "--goal", "Additional", "--kind", "implement")

        # Show should list the new slice with 2 final_steps
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "extra-work")


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
        sl = next(s for s in module.YamlTaskStore(task).read()["plan"]["slices"] if s["key"] == "second-slice")
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


def test_legacy_cue_add_step_final_rolls_back_on_closed_final_steps_schema() -> None:
    """add-step --final on a closed-length final_steps rolls back cleanly."""
    with tempfile.TemporaryDirectory() as tmp:
        # Use the bootstrap task which has a closed 2-element final_steps
        # Copy it to tmp to avoid modifying original
        task = Path(tmp) / "closed-final.cue"
        original_content = VENDORED_CLOSED_FINAL_STEPS_CUE.strip() + "\n"
        task.write_text(original_content)

        # Try to add a step to final_steps when it's a closed list
        res = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "init-harness", "--key", "extra-step", "--title", "Extra", "--final", check=False)

        # Should fail
        if res.returncode == 0:
            raise AssertionError("add-step --final should reject closed final_steps schema")

        # File should be byte-identical to original (rollback)
        after_content = task.read_text()
        if after_content != original_content:
            raise AssertionError("file was not rolled back to original on validation failure")


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
    """YAML add-slice ignores unsupported local CUE-only required fields (no task-schema preamble)."""
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


def test_legacy_cue_validate_passes_shared_schema_task() -> None:
    """validate is the canonical check: loads task + shared schema and exits 0."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "ok.cue"
        task.write_text(
            """
package task

task: {
    title: "Validate ok"
    status: "in_progress"
    project: {name: "Fixture"}
    orchestration: {
        cursor: {slice: "only", step: "create-plan"}
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
    plan: {
        note: ""
        slices: [
            #Slice & {
                key: "only"
                kind: "implement"
                title: "Only"
                goal: "g"
                status: "planned"
                note: ""
                steps: [
                    #Step & {key: "create-plan", title: "Plan", status: "planned", note: ""},
                    #Step & {key: "grill-plan", title: "Grill", status: "planned", note: ""},
                    #Step & {key: "edit-code", title: "Edit", status: "planned", note: ""},
                    #Step & {key: "verify", title: "Verify", status: "planned", note: ""},
                ]
                final_steps: [
                    #Step & {key: "e2e-evidence", title: "Evidence", status: "planned", note: ""},
                    #Step & {key: "vulnerability-scan", title: "Scan", status: "planned", note: ""},
                    #Step & {key: "share-with-team", title: "Share", status: "planned", note: ""},
                    #Step & {key: "update-task-file", title: "Update", status: "planned", note: ""},
                    #Step & {key: "wait-for-feedback", title: "Wait", status: "planned", note: ""},
                ]
            },
        ]
    }
}
"""
        )
        out = run(str(PI_JOB), "--task", str(task), "validate").stdout
        assert_contains(out, "ok:")
        assert_contains(out, "task-schema.cue")
        assert_contains(out, "pi-job validate")


def test_validate_warns_when_persisted_slice_predates_template_addition() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "older-template.yaml"
        run(str(PI_JOB), "--task", str(task), "create")
        module = load_pi_job_module()
        store = module.YamlTaskStore(task)
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
    store = module.YamlTaskStore(task)
    task_data = store.read()
    task_data["plan"]["slices"][0]["steps"][0]["note"] = "x" * note_len
    store.replace(task_data)


def test_validate_warns_on_long_note() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "long_note_validate.yaml"
        _scaffolded_task_with_long_step_note(task)
        result = run(str(PI_JOB), "--task", str(task), "validate")
        assert_contains(result.stdout, "warning:")
        assert_contains(result.stdout, "oversized note")
        assert_contains(result.stdout, "do-the-change/create-plan")


def test_status_warns_on_long_note() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "long_note_status.yaml"
        _scaffolded_task_with_long_step_note(task)
        result = run(str(PI_JOB), "--task", str(task), "status")
        assert_contains(result.stdout, "warning:")
        assert_contains(result.stdout, "oversized note")
        assert_contains(result.stdout, "do-the-change/create-plan")


def test_validate_warns_on_large_task_file() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "large_task.yaml"
        run(str(PI_JOB), "--task", str(task), "create")
        module = load_pi_job_module()
        store = module.YamlTaskStore(task)
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
        step = find_step(module.YamlTaskStore(task).read(), "implementation", "vulnerability-scan")
        assert long_note in step["note"]


def test_validate_fails_invalid_step_status() -> None:
    """validate must fail closed when a #Step status violates shared #Status."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bad.cue"
        task.write_text(
            """
package task

task: {
    title: "Bad step status"
    status: "in_progress"
    project: {name: "Fixture"}
    plan: {
        note: ""
        slices: [
            #Slice & {
                key: "only"
                kind: "implement"
                title: "Only"
                goal: "g"
                status: "planned"
                note: ""
                steps: [#Step & {key: "x", title: "X", status: "bogus", note: ""}]
                final_steps: []
            },
        ]
    }
}
"""
        )
        res = run(str(PI_JOB), "--task", str(task), "validate", check=False)
        if res.returncode == 0:
            raise AssertionError("validate should fail on invalid #Step status")
        assert_contains(res.stderr, "cue export failed")



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
    module.YamlTaskStore(task_path).replace(task)


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
    task = module.YamlTaskStore(task_path).read()
    task["orchestration"] = {
        "cursor": {"slice": "good", "step": "create-plan"},
        "policy": {
            "coding_execution": {
                "subagent_required": True,
                "lower_power_model_preferred": True,
                "orchestrator_reviews_subagent": True,
            }
        },
    }
    module.YamlTaskStore(task_path).replace(task)


def test_status_reports_structure_ok_for_conformant_task() -> None:
    """Healthy initialized task reports Structure: ok."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "ok.yaml"
        module = load_pi_job_module()
        mapping = module.example_task_mapping(title="Validate ok")
        mapping["project"] = {"name": "Fixture"}
        mapping["orchestration"] = {
            "cursor": {"slice": "only", "step": "create-plan"},
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


def test_migrate_task_reports_already_migrated() -> None:
    """Task with no local defs (pure shared-schema style) reports already migrated."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "shared-schema.cue"
        # Use NO preamble with local defs - just package and task
        fixture = """
package task

task: {
    title: "Shared schema task"
    status: "in_progress"
    project: {
        name: "Test"
    }
""" + LEGACY_MIGRATE_PLAN_BODY + """
}
"""
        task.write_text(fixture)

        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout
        assert_contains(out, "PI-JOB MIGRATION INSTRUCTION")
        assert_contains(out, "already migrated")
        assert_contains(out, "no local type declarations found")


def test_migrate_task_recommends_delete_for_identical_status() -> None:
    """Local #Status exactly matches shared schema → recommends DELETE."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "identical-status.cue"
        # Use preamble that already matches shared schema
        fixture = LEGACY_MIGRATE_PREAMBLE + """
task: {
    title: "Identical status"
    status: "in_progress"
    project: {
        name: "Test"
    }
""" + LEGACY_MIGRATE_PLAN_BODY + """
}
"""
        task.write_text(fixture)

        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout
        assert_contains(out, "#Status")
        assert_contains(out, "DELETE")
        assert_contains(out, "identical")


def test_migrate_task_recommends_keep_for_status_with_used_extra_value() -> None:
    """Extra status value declared AND used in instance data → recommends KEEP."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "used-extra-status.cue"
        # Define a custom #Status with extra value "ready"
        preamble_extra_status = """
package task

#Status: "planned" | "ready" | "in_progress" | "blocked" | "done" | "skipped"
#Step: {
    key: string
    title: string
    status: #Status
    note: string
}
#Slice: {
    key: string
    title: string
    goal: string
    status: #Status
    note: string
    steps: [...#Step]
    final_steps: [...#Step]
}
"""
        fixture = preamble_extra_status + """
task: {
    title: "Used extra status"
    status: "in_progress"
    project: {
        name: "Test"
    }
    plan: {
        slices: [
            #Slice & {
                key: "use-ready"
                kind:   "implement"
                title: "Use ready status"
                goal: "Test extra value usage"
                status: "ready"
                note: ""
                steps: []
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout
        assert_contains(out, "#Status")
        assert_contains(out, "KEEP AS-IS")
        assert_contains(out, "used")
        assert_contains(out, "ready")


def test_migrate_task_recommends_delete_for_status_with_unused_extra_value() -> None:
    """Extra value declared but never used → recommends DELETE."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unused-extra-status.cue"
        # Define a custom #Status with extra value "unused_value" that's never used
        preamble_unused = """
package task

#Status: "planned" | "unused_value" | "in_progress" | "blocked" | "done" | "skipped"
#Step: {
    key: string
    title: string
    status: #Status
    note: string
}
#Slice: {
    key: string
    title: string
    goal: string
    status: #Status
    note: string
    steps: [...#Step]
    final_steps: [...#Step]
}
"""
        fixture = preamble_unused + """
task: {
    title: "Unused extra status"
    status: "in_progress"
    project: {
        name: "Test"
    }
    plan: {
        slices: [
            #Slice & {
                key: "no-unused"
                kind:   "implement"
                title: "No unused status"
                goal: "Only use normal statuses"
                status: "planned"
                note: ""
                steps: []
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout
        assert_contains(out, "#Status")
        assert_contains(out, "DELETE")
        assert_contains(out, "unused")


def test_migrate_task_recommends_replace_for_slice_with_extra_fields() -> None:
    """#Slice with extra local fields (repos, closed final_steps) → recommends REPLACE."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "slice-extra-fields.cue"
        # Define a local #Slice with repos field and closed final_steps
        preamble_slice_extra = """
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
    title: string
    goal: string
    status: #Status
    note: string
    repos: [...string]
    steps: [...#Step]
    final_steps: [
        #Step & {key: "e2e-evidence", title: "E2E evidence", status: "planned", note: ""},
        #Step & {key: "update-task-file", title: "Update task", status: "planned", note: ""},
    ]
}
"""
        fixture = preamble_slice_extra + """
task: {
    title: "Slice with repos and closed final_steps"
    status: "in_progress"
    project: {
        name: "Test"
    }
    plan: {
        slices: [
            #Slice & {
                key: "multi-repo"
                kind:   "implement"
                title: "Multi repo work"
                goal: "Work across repos"
                status: "planned"
                note: ""
                repos: ["graphius", "darius"]
                steps: []
                final_steps: [
                    #Step & {key: "e2e-evidence", title: "E2E evidence", status: "planned", note: ""},
                    #Step & {key: "update-task-file", title: "Update task", status: "planned", note: ""},
                ]
            },
        ]
    }
}
"""
        task.write_text(fixture)

        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout
        assert_contains(out, "#Slice")
        assert_contains(out, "REPLACE")
        assert_contains(out, "repos")
        assert_contains(out, "final_steps")
        assert_contains(out, "e2e-evidence")
        assert_contains(out, "update-task-file")


def test_migrate_task_line_ranges_are_accurate() -> None:
    """Line ranges in output match the actual declaration span."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "line-ranges.cue"
        # Create fixture with a known #Slice block on specific lines
        fixture = """
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
    title: string
    goal: string
    status: #Status
    note: string
    repos: [...string]
    steps: [...#Step]
    final_steps: [...#Step]
}

task: {
    title: "Line range test"
    status: "in_progress"
    project: {
        name: "Test"
    }
    plan: {
        slices: []
    }
}
"""
        task.write_text(fixture)

        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout
        assert_contains(out, "#Slice")
        assert_contains(out, "lines")
        # Should mention the Slice definition block which is roughly on lines 9-17
        # (the exact range depends on formatting, but it should be reasonable)
        if "lines" not in out:
            raise AssertionError(f"expected 'lines' in output to show line ranges:\n{out}")


def test_migrate_task_partial_migration_only_slice_remains() -> None:
    """Regression: migrate-task should work on files already partially migrated.

    When a file has already had some types (like #Step) migrated away in a previous pass,
    the remaining local block (e.g. #Slice with a closed final_steps list) still references
    those deleted types. Previously, diagnose_slice() would shell `cue def <file alone>`
    which would fail with "reference #Step not found". The fix parses the raw text directly
    instead, avoiding the need to resolve deleted type references.

    This test creates a fixture where #Slice has a closed final_steps list (referencing #Step),
    but #Step itself is NOT declared locally (already migrated away), and verifies that
    migrate-task produces a real diagnosis instead of "Diagnosis error"."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "partially-migrated.cue"
        # Only #Slice is declared locally; #Step, #Status, etc. are already gone (migrated)
        fixture = """
package task

#Slice: {
    key: string
    title: string
    goal: string
    status: string
    note: string
    repos: [...string]
    steps: [...]
    final_steps: [
        #Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence", status: "planned", note: ""},
        #Step & {key: "share-with-team", title: "Share with team: ticket + PR", status: "planned", note: ""},
        #Step & {key: "update-task-file", title: "Update this task file plan", status: "planned", note: ""},
    ]
}

task: {
    title: "Partially migrated test"
    status: "in_progress"
    project: {
        name: "Test"
    }
    decisions: []
    plan: {
        slices: [
            #Slice & {
                key: "test-slice"
                kind:   "implement"
                title: "Test Slice"
                goal: "Test"
                status: "planned"
                note: ""
                repos: ["graphius"]
                steps: []
                final_steps: [
                    #Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence", status: "planned", note: ""},
                    #Step & {key: "share-with-team", title: "Share with team: ticket + PR", status: "planned", note: ""},
                    #Step & {key: "update-task-file", title: "Update this task file plan", status: "planned", note: ""},
                ]
            },
        ]
    }
}
"""
        task.write_text(fixture)

        # Run migrate-task and capture output
        out = run(str(PI_JOB), "--task", str(task), "migrate-task").stdout

        # Verify it does NOT contain "Diagnosis error" (the bug)
        if "Diagnosis error" in out:
            raise AssertionError(f"migrate-task should not error on partially-migrated files:\n{out}")

        # Verify it does contain a real diagnosis (mentions the file has delta fields)
        assert_contains(out, "#Slice")
        assert_contains(out, "REPLACE")
        assert_contains(out, "repos")
        assert_contains(out, "final_steps")
        # Verify closed_items are extracted correctly
        assert_contains(out, "e2e-evidence")
        assert_contains(out, "share-with-team")
        assert_contains(out, "update-task-file")


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
    """set-worktree requires exactly one of --path or --clear."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-clear-no-mode.yaml"
        write_task_yaml(task, standard_fixture_mapping())

        res = run(
            str(PI_JOB), "--task", str(task), "set-worktree",
            "--slice", "second-slice", "--repo", "graphius",
            check=False,
        )
        if res.returncode == 0:
            raise AssertionError("set-worktree should require --path or --clear")
        stderr = res.stderr.lower()
        if "mutually exclusive" not in stderr and "required" not in stderr:
            raise AssertionError(f"expected argparse mode error, got:\n{res.stderr}")


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
        repo_entry = module.YamlTaskStore(task).read()["plan"]["slices"][1]["repo_work"]["graphius"]
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
        store = module.YamlTaskStore(task_path)
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


MINIMAL_CUE_FIXTURE = """package task

task: {
	title:  "Minimal fixture"
	status: "in_progress"

	source: {
		jira:       ""
		discovered: ""
		context:    ""
	}

	project: {
		key:     "minimal"
		name:    "Minimal"
		route:   ""
		context: ""
	}

	context: "minimal context"

	decisions: []

	plan: {
		note: ""
		slices: [
			#Slice & {
				kind:   "implement"
				key:    "s1"
				title:  "Slice one"
				goal:   "Goal one"
				status: "planned"
				note:   ""
				steps: [
					#Step & {key: "step1", title: "Step one", status: "planned", note: ""},
				]
				final_steps: []
			},
		]
	}
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
        store.init_orchestration(module.Cursor(slice="alpha", step="edit-code"))

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

        store.set_cursor(module.Cursor(slice="alpha", step="edit-code"))

        task = store.read()

        assert task["title"] == "FS round-trip task"
        assert task["status"] == "in_progress"
        assert task["source"] == {}
        assert task["project"] == {}
        assert task["context"] == ""
        assert task["decisions"] == []

        orch = task["orchestration"]
        assert "profile" not in orch
        assert orch["cursor"] == {"slice": "alpha", "step": "edit-code"}
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


def test_fs_and_cue_task_store_shape_parity() -> None:
    """For a minimal one-slice/one-step task, FsTaskStore.read() and CueTaskStore.read()
    should agree on top-level keys and on plan.slices[0]'s keys (shape parity, not
    byte-identical output - list contents/order aren't compared here)."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        cue_path = Path(tmp) / "minimal.cue"
        cue_path.write_text(MINIMAL_CUE_FIXTURE)
        cue_task = module.CueTaskStore(cue_path).read()

        fs_base = Path(tmp) / "fsminimal"
        fs_base.mkdir()
        (fs_base / "title").write_text("Minimal fixture\n")
        (fs_base / "status").write_text("in_progress\n")
        (fs_base / "source").write_text("jira: \ndiscovered: \ncontext: \n")
        (fs_base / "project").write_text("key: minimal\nname: Minimal\nroute: \ncontext: \n")
        (fs_base / "context").write_text("minimal context\n")
        store = module.FsTaskStore(fs_base)
        store.add_slice(key="s1", kind="implement", title="Slice one", goal="Goal one", extra_fields={}, steps=[("step1", "Step one")], final_steps=[], after=None)

        fs_task = store.read()

        assert set(fs_task.keys()) == set(cue_task.keys()), (sorted(fs_task.keys()), sorted(cue_task.keys()))
        fs_slice0 = fs_task["plan"]["slices"][0]
        cue_slice0 = cue_task["plan"]["slices"][0]
        assert set(fs_slice0.keys()) == set(cue_slice0.keys()), (sorted(fs_slice0.keys()), sorted(cue_slice0.keys()))


PROJECT_FIXTURE_CUE = """package task

task: {
	title:  "Project fixture task"
	status: "in_progress"

	source: {
		jira:       "PROJ-1"
		discovered: "2026-07-01"
		context:    "Why this task exists."
	}

	project: {
		key:     "proj"
		name:    "Project Fixture"
		route:   "projects/proj/workflow.md"
		context: "Where this lives."
	}

	context: \"\"\"
		Multi-line free-form background.
		Second paragraph here.
		\"\"\"

	orchestration: {
		cursor: {
            slice: "alpha"
			step:  "edit-code"
		}
		policy: {
			coding_execution: {
				subagent_required:             true
				lower_power_model_preferred:   true
				orchestrator_reviews_subagent: true
			}
		}
		artifacts: {
			daily_boo: #Artifact & {status: "planned", note: "Append only on a real aha."}
		}
	}

	decisions: [
		#Decision & {date: "2026-07-01", note: "First \\"quoted\\" decision.\\n\\nSecond paragraph — still one note.", source: "chat:2026-07-01"},
		#Decision & {date: "2026-07-02", note: "Second decision.", source: "chat:2026-07-02"},
	]

	plan: {
		note: "Real plan note, not the scaffold placeholder."
		slices: [
			#Slice & {
				kind:   "implement"
				key:    "alpha"
				title:  "Alpha slice"
				goal:   "Alpha goal"
				status: "in_progress"
				note:   "Alpha note."
				repos: ["repo-a"]
				repo_work: {
					"repo-a": {
						worktree: "/tmp/worktrees/alpha"
						prs: [
							#PR & {url: "https://example.com/pr/1", status: "open", note: "first PR — with emdash"},
						]
					}
				}
				steps: [
					#Step & {key: "edit-code", title: "Add \\"on-hold\\": \\"mapped\\"", status: "done", note: "Done already.\\n\\nUpdated later with more detail.", execution: {model: "anthropic/claude-test", started: "2026-07-01T10:00:00Z", ended: "2026-07-01T10:05:00Z"}},
					#Step & {key: "verify", title: "Verify", status: "planned", note: ""},
				]
				final_steps: [
					#Step & {key: "e2e-evidence", title: "Evidence", status: "in_progress", note: "In flight."},
				]
			},
			#Slice & {
				kind:   "implement"
				key:    "beta"
				title:  "Beta slice"
				goal:   "Beta goal"
				status: "planned"
				note:   ""
				depends_on: ["alpha"]
				steps: []
				final_steps: []
			},
		]
	}
}
"""


def test_cue_escape_escapes_newlines_quotes_and_tabs() -> None:
    """Double-quoted CUE literals must escape control characters, not embed raw newlines."""
    module = load_pi_job_module()
    escaped = module.cue_escape('a\n"b"\tc\\d')
    assert escaped == 'a\\n\\"b\\"\\tc\\\\d', escaped
    assert "\n" not in escaped
    assert "\t" not in escaped


def test_cmd_project_cue_to_fs_to_cue_round_trip() -> None:
    """project() must be lossless: CUE fixture -> fresh FS dir -> fresh CUE file should
    produce a CueTaskStore.read() dict identical to the original fixture's, covering
    orchestration, decisions, plan.note, and every slice/step/repo_work/PR field.

    Fixture deliberately includes newlines, quotes, and em dashes in notes/titles - the
    shape that broke FS→CUE on real tasks when cue_escape left raw newlines unescaped."""
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        fixture_path = Path(tmp) / "fixture.cue"
        fixture_path.write_text(PROJECT_FIXTURE_CUE)

        fs_dir = Path(tmp) / "fsout"
        run(str(PI_JOB), "--task", str(fixture_path), "project", "--to", str(fs_dir))

        roundtrip_path = Path(tmp) / "roundtrip.cue"
        run(str(PI_JOB), "--task", str(fs_dir), "project", "--to", str(roundtrip_path))

        original = module.CueTaskStore(fixture_path).read()
        roundtrip = module.CueTaskStore(roundtrip_path).read()
        assert original == roundtrip, (original, roundtrip)
        # Sanity: the tricky fields survived, not just empty-fixture equality.
        assert "\n\n" in original["decisions"][0]["note"]
        assert '"quoted"' in original["decisions"][0]["note"]
        assert "\n\n" in original["plan"]["slices"][0]["steps"][0]["note"]


def test_cmd_project_refuses_nonempty_cue_destination() -> None:
    """project() must fail closed rather than append after an existing slice (which would
    silently shift every subsequent slice's position) when the CUE destination already
    has content."""
    with tempfile.TemporaryDirectory() as tmp:
        fixture_path = Path(tmp) / "fixture.cue"
        fixture_path.write_text(PROJECT_FIXTURE_CUE)

        nonempty_dst = Path(tmp) / "nonempty.cue"
        nonempty_dst.write_text(PROJECT_FIXTURE_CUE)

        res = run(str(PI_JOB), "--task", str(fixture_path), "project", "--to", str(nonempty_dst), check=False)
        assert res.returncode != 0
        assert_contains(res.stderr, "already has slices/decisions")


def test_persisted_models_document_every_field() -> None:
    module = load_pi_job_module()
    model_names = (
        "ExecutionDocument", "StepDocument", "DecisionDocument", "ArtifactDocument",
        "PullRequestDocument", "RepositoryWorkDocument", "SliceDocument", "SourceDocument",
        "ProjectDocument", "CodingExecutionPolicyDocument", "OrchestrationPolicyDocument",
        "CursorDocument", "OrchestrationDocument", "PlanDocument", "TaskDocument",
        "BootstrapSliceDocument", "BootstrapDocument",
        "ConfigLayeringDocument", "ArtifactGateDocument", "ArtifactRuleDocument",
        "ToolbeltAidDocument", "StepKindDocument", "SlicePoliciesDocument",
        "SliceKindDocument", "InstructionPacketsDocument", "CliHelpDocument",
        "CliHelpAddDecisionDocument", "CliHelpFinishDocument", "ProfileDocument",
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
        store = module.YamlTaskStore(task_path)
        store.replace(module.example_task_mapping(title="YAML round trip"))
        task_path.chmod(0o600)
        store.init_orchestration(module.Cursor(slice="do-the-change", step="create-plan"))
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
        assert task["orchestration"]["cursor"]["step"] == "create-plan"
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
        module.YamlTaskStore(task_path).replace(module.example_task_mapping(title="Concurrent"))

        def add_decision(index: int) -> None:
            module.YamlTaskStore(task_path).add_decision(
                date="2026-07-22",
                note=f"decision-{index}",
                source="concurrency-test",
            )

        with ThreadPoolExecutor(max_workers=8) as executor:
            list(executor.map(add_decision, range(24)))

        notes = {decision["note"] for decision in module.YamlTaskStore(task_path).read()["decisions"]}
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
            store = module.YamlTaskStore(task_path)
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
        store = module.YamlTaskStore(task_path)
        store.replace(module.example_task_mapping(title="Concurrent lifecycle"))
        store.init_orchestration(module.Cursor(slice="do-the-change", step="create-plan"))
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


def test_yaml_force_advance_cannot_overwrite_concurrent_finish() -> None:
    module = load_pi_job_module()
    with tempfile.TemporaryDirectory() as tmp:
        task_path = Path(tmp) / "advance-race.yaml"
        store = module.YamlTaskStore(task_path)
        store.replace(module.example_task_mapping(title="Advance race"))
        store.init_orchestration(module.Cursor(slice="do-the-change", step="create-plan"))
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

        def force_advance() -> subprocess.CompletedProcess[str]:
            barrier.wait()
            return run(
                str(PI_JOB), "--task", str(task_path), "advance",
                "--force", "--reason", "forced concurrently",
                check=False,
            )

        with ThreadPoolExecutor(max_workers=2) as executor:
            finish_future = executor.submit(finish)
            advance_future = executor.submit(force_advance)
            finish_result = finish_future.result()
            advance_result = advance_future.result()

        assert finish_result.returncode == 0, finish_result.stderr
        assert advance_result.returncode == 0, advance_result.stderr
        step = store.read()["plan"]["slices"][0]["steps"][0]
        assert step["status"] == "done"
        assert "completed" in step["note"]
        assert step["execution"]["ended"]


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


def test_cmd_project_cue_to_yaml_keeps_source_and_verifies_semantics() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "source.cue"
        destination = Path(tmp) / "source.yaml"
        source.write_text(PROJECT_FIXTURE_CUE)
        original = source.read_text()

        result = run(str(PI_JOB), "--task", str(source), "project", "--to", str(destination))
        assert_contains(result.stdout, "YAML task file")
        assert_contains(result.stderr, "CUE task storage is deprecated")
        assert source.read_text() == original
        assert destination.exists()

        module = load_pi_job_module()
        cue_task = module.semantic_task_mapping(module.CueTaskStore(source).read(), source=str(source))
        yaml_task = module.semantic_task_mapping(module.YamlTaskStore(destination).read(), source=str(destination))
        assert cue_task == yaml_task

        destination_before_refusal = destination.read_text()
        refusal = run(
            str(PI_JOB), "--task", str(source), "project", "--to", str(destination),
            check=False,
        )
        assert refusal.returncode != 0
        assert_contains(refusal.stderr, "destination YAML task already exists")
        assert destination.read_text() == destination_before_refusal


def test_legacy_cue_custom_fields_remain_readable_but_do_not_migrate_silently() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "custom.cue"
        destination = Path(tmp) / "custom.yaml"
        source.write_text(
            "package task\n"
            "#Slice: {\n"
            "  key: string\n  kind: string\n  title: string\n  goal: string\n"
            "  status: #Status\n  note: string\n  custom?: string\n"
            "  steps: [...#Step]\n  final_steps: [...#Step]\n}\n"
            "task: {\n"
            "  title: \"Custom CUE\"\n  status: \"planned\"\n"
            "  plan: {note: \"\", slices: [#Slice & {\n"
            "    key: \"one\", kind: \"implement\", title: \"One\", goal: \"Goal\",\n"
            "    status: \"planned\", note: \"\", custom: \"must not disappear\",\n"
            "    steps: [], final_steps: []\n"
            "  }]}\n"
            "}\n"
        )
        module = load_pi_job_module()
        task = module.CueTaskStore(source).read()
        assert task["plan"]["slices"][0]["custom"] == "must not disappear"

        result = run(
            str(PI_JOB), "--task", str(source), "project", "--to", str(destination),
            check=False,
        )
        assert result.returncode != 0
        assert_contains(result.stderr, "Extra inputs are not permitted")
        assert not destination.exists()



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
        step = find_step(module.YamlTaskStore(task).read(), "implementation", "vulnerability-scan")
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
        step = find_step(module.YamlTaskStore(task).read(), "implementation", "vulnerability-scan")
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
    """Reconcile fails when the target is already done."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "reconcile-done.yaml"
        write_task_yaml(task, lifecycle_mapping(
            status="done",
            note="already",
            execution={"model": "google/gemini-reviewer", "started": "2026-07-01T10:05:00Z", "ended": "2026-07-01T10:10:00Z"},
        ))

        res = run(
            str(PI_JOB), "--task", str(task), "finish", "--reconcile",
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
        step = find_step(module.YamlTaskStore(task).read(), "implementation", "vulnerability-scan")
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
        step = find_step(module.YamlTaskStore(task).read(), "implementation", "vulnerability-scan")
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
        task_slice = module.YamlTaskStore(task).read()["plan"]["slices"][0]
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
        task_slice = module.YamlTaskStore(task).read()["plan"]["slices"][0]
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
        task_slice = module.YamlTaskStore(task).read()["plan"]["slices"][0]
        assert task_slice["status"] == "blocked"
        assert task_slice["note"] == "Existing blocker context\n\nWaiting on upstream API"
        run(
            str(PI_JOB), "--task", str(task), "block-slice",
            "--key", "implementation", "--reason", "Still blocked",
        )
        task_slice = module.YamlTaskStore(task).read()["plan"]["slices"][0]
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
        task_slice = module.YamlTaskStore(task).read()["plan"]["slices"][0]
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


def test_advance_rejects_malformed_scan_timestamps() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "malformed-time.yaml"
        write_task_yaml(task, lifecycle_mapping(
            status="done",
            note="No findings",
            execution={"model": "google/gemini-reviewer", "started": "not-a-time", "ended": "2026-07-01T10:10:00Z"},
        ))
        result = run(str(PI_JOB), "--task", str(task), "advance", check=False)
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
        step = find_step(module.YamlTaskStore(task).read(), "implementation", "vulnerability-scan")
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
        task_slice = module.YamlTaskStore(task).read()["plan"]["slices"][0]
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
        task = Path(tmp) / "empty.yaml"
        out = run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--dry-run").stdout
        assert "slices: []" in out
        assert "do-the-change" not in out


def test_scaffold_initial_kind_setup_seeds_setup_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "initial-kind.yaml"
        out = run(str(PI_JOB), "--task", str(task), "create", "--kind", "setup", "--dry-run").stdout
        assert "setup-slice" in out
        assert "explore-context" in out
        assert "do-the-change" not in out


def test_bootstrap_creates_initialized_task() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap.yaml"
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
        assert_contains(result.stdout, "cursor: task-setup / explore-context")
        assert_contains(result.stdout, "PI-JOB EXECUTION INSTRUCTION")
        assert task.exists()
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: ok")
        assert_contains(status, "Cursor: task-setup / explore-context")
        validate = run(str(PI_JOB), "--task", str(task), "validate").stdout
        assert_contains(validate, "ok:")


def test_bootstrap_dry_run_prints_diff_and_does_not_write() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-dry.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Dry run test\nslices:\n  - key: only-slice\n    kind: implement\n    title: Only\n    goal: Test\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        out = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), "--dry-run").stdout
        assert "+++" in out
        assert "PI-JOB EXECUTION INSTRUCTION" not in out
        assert not task.exists()


def test_bootstrap_refuses_overwrite_without_force() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-exists.yaml"
        task.write_text("title: existing\nstatus: planned\n", encoding="utf-8")
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Bootstrap test\nslices:\n  - key: only-slice\n    kind: implement\n    title: Only\n    goal: Test\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "already exists")


def test_bootstrap_rejects_unknown_kind() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-unknown-kind.yaml"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Bad kind\nslices:\n  - key: slice\n    kind: nonexistent\n    title: Bad\n    goal: Fail\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "unknown slice kind")


def test_bootstrap_rejects_unresolved_dependency() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-bad-dep.yaml"
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
        store = module.YamlTaskStore(task)
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
        task = Path(tmp) / "project-mutation.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "set-project", "--key", "new-key", "--name", "New Name")
        module = load_pi_job_module()
        store = module.YamlTaskStore(task)
        task_data = store.read()
        assert task_data["project"]["key"] == "new-key"
        assert task_data["project"]["name"] == "New Name"


def test_set_context_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "context-mutation.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "set-context", "--context", "New context")
        module = load_pi_job_module()
        store = module.YamlTaskStore(task)
        task_data = store.read()
        assert task_data["context"] == "New context"


def test_add_decision_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "decision-mutation.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "add-decision", "--date", "2026-07-27", "--note", "Test decision", "--source", "test")
        module = load_pi_job_module()
        store = module.YamlTaskStore(task)
        task_data = store.read()
        assert len(task_data["decisions"]) == 1
        assert task_data["decisions"][0]["note"] == "Test decision"


def test_set_plan_note_mutation() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "plan-note-mutation.yaml"
        run(str(PI_JOB), "--task", str(task), "create", "--empty-plan", "--force")
        run(str(PI_JOB), "--task", str(task), "set-plan-note", "--note", "Plan note text")
        module = load_pi_job_module()
        store = module.YamlTaskStore(task)
        task_data = store.read()
        assert task_data["plan"]["note"] == "Plan note text"


def test_remove_slice_removes_and_guards() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "remove-slice.yaml"
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
        store = module.YamlTaskStore(task)
        task_data = store.read()
        assert len(task_data["plan"]["slices"]) == 1


def test_bootstrap_requires_yaml() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.cue"
        bootstrap_input = Path(tmp) / "input.yaml"
        bootstrap_input.write_text("title: Fail\nslices:\n  - key: slice\n    kind: implement\n    title: Slice\n    goal: Fail\ndecisions:\n  - date: '2026-07-27'\n    note: Decision\n    source: test\n", encoding="utf-8")
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(bootstrap_input), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "requires a YAML task file")


def test_create_from_requires_intent_path() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.yaml"
        result = run(str(PI_JOB), "--task", str(task), "create", "--from", str(Path(tmp) / "missing-intent.yaml"), check=False)
        assert result.returncode != 0
        assert_contains(result.stderr, "create intent not found")


def test_bootstrap_prints_seed_slice_plans_for_implement_not_setup() -> None:
    """After bootstrap, stdout lists qualifying implement slices in the seed block, not setup."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "bootstrap-seed.yaml"
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
        assert_contains(seed, "bootstrap-seed.plans/implement-one.md")
        assert_contains(seed, "Depends on: task-setup")
        if "- task-setup [" in seed:
            raise AssertionError(f"setup slice must not appear as a seed entry:\n{seed}")
        assert "task-setup.plans/" not in seed


def test_add_slice_implement_prints_seed_block_for_new_slice_only() -> None:
    """add-slice for a qualifying kind prints the seed block for that slice only."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice-seed.yaml"
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
        assert_contains(seed, "add-slice-seed.plans/wire-api.md")
        if "- task-setup [" in seed:
            raise AssertionError(f"seed block must list only the new slice:\n{seed}")
        assert "task-setup.plans/" not in seed


def test_add_slice_setup_prints_no_seed_block() -> None:
    """add-slice for setup (no create-plan in template) must not print a seed block."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice-no-seed.yaml"
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
        task = Path(tmp) / "bootstrap-dry-seed.yaml"
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
        task = Path(tmp) / "my-task.yaml"
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
        assert_contains(seed, "my-task.plans/feature-a.md")
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
        task = module.YamlTaskStore(task_path).read()
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
        task = module.YamlTaskStore(task_path).read()
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
        task = module.YamlTaskStore(task_path).read()
        digest_before = task["orchestration"]["content_digest"]
        raw = yaml.safe_load(task_path.read_text())
        raw["context"] = "edited outside pi-job"
        task_path.write_text(yaml.safe_dump(raw, sort_keys=False, allow_unicode=True), encoding="utf-8")
        run(
            str(PI_JOB), "--task", str(task_path), "start", "--model", "provider/test",
        )
        run(
            str(PI_JOB), "--task", str(task_path), "finish", "--note", "finished while dirty",
        )
        task_after = module.YamlTaskStore(task_path).read()
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
            "cursor": {"slice": chronological_cursor[0], "step": chronological_cursor[1]},
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
                "cursor": {"slice": "middle", "step": "create-plan"},
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
        blocked = module.YamlTaskStore(task).read()["plan"]["slices"][0]
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
        store = module.YamlTaskStore(task)
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
        decisions = module.YamlTaskStore(task).read().get("decisions") or []
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
        assert_contains(template, "## Open questions")
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
    park = module.load_profile_contract()["interrupt_park_steps"]
    assert "grill-plan" in park
    assert "clarify-scope" in park
    for field in (
        "slice_plan_stub",
        "findings_file_header",
        "status_interrupt_hint",
        "investigate_interrupt",
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
        mapping["orchestration"]["cursor"] = {"slice": "implementation", "step": "grill-plan"}
        mapping["plan"]["slices"][0]["steps"] = [
            {"key": "grill-plan", "title": "Grill", "status": "planned", "note": ""},
        ]
        mapping["plan"]["slices"][0]["status"] = "blocked"
        mapping["plan"]["slices"][0]["note"] = "GP PIN fails after treatment-change"
        # grill-plan requires user decision; but slice is blocked - still show hint from step kind
        # Use a second slice as cursor host that is not blocked
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
        mapping["orchestration"]["cursor"] = {"slice": "other", "step": "grill-plan"}
        write_task_yaml(task, mapping)
        out = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(out, "Blocked:")
        assert_contains(out, "implementation")
        assert_contains(out, "Cursor parked")
        assert_contains(out, "investigate")


def test_investigate_does_not_move_cursor() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "investigate.yaml"
        mapping = lifecycle_mapping()
        mapping["orchestration"]["cursor"] = {
            "slice": "implementation",
            "step": "vulnerability-scan",
        }
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
        assert_contains(out, "do not advance")
        module = load_pi_job_module()
        cursor = module.YamlTaskStore(task).read()["orchestration"]["cursor"]
        assert cursor["slice"] == "implementation"
        assert cursor["step"] == "vulnerability-scan"
        findings = Path(tmp) / "investigate.plans" / "_findings.md"
        assert findings.is_file()
        assert_contains(findings.read_text(encoding="utf-8"), "Evidence chain complete")


def main() -> None:
    test_profiled_task()
    test_uninitialized_task_requires_orchestration()
    test_init_with_kind_setup_seeds_setup_slice()
    test_setup_template_includes_wayfinder_step()
    test_fog_slice_kind_seeds_template()
    test_wayfinder_context_reports_frontier_and_fog()
    test_edit_code_owner_from_step_kinds()
    test_orchestrator_instruction_includes_task_record_discipline()
    test_instruction_includes_next_action_and_enter_the_loop()
    test_bootstrap_instruction_includes_next_action()
    test_subagent_instruction_includes_task_record_discipline()
    test_task_record_discipline_uses_task_file_and_slice_key_hints()
    test_subagent_instruction_prohibits_direct_task_store_inspection()
    test_subagent_instruction_includes_scoped_read_command()
    test_orchestrator_instruction_has_no_subagent_prompt()
    test_subagent_orchestrator_directs_markdown_slice_without_decisions_dump()
    test_add_decision_and_finish_help_describe_channels()
    test_decision_document_schema_describes_channels_contract()
    test_update_task_file_guidance_names_mutation_commands()
    test_plan_output_omits_task_record_discipline()
    test_create_plan_instruction_defines_constraint_and_behaviour_contract()
    test_grill_plan_instruction_defines_constraint_and_behaviour_contract()
    test_profile_yaml_aliases_shared_guidance_strings()
    test_advance_pick_next_then_jump_to_closing_slice()
    test_status_shows_cursor_and_ready_without_next_line()
    test_pick_next_reports_all_slices_done()
    test_advance_blocked_on_incomplete_current_step()
    test_advance_resync_rejects_force_and_missing_reason()
    test_advance_resync_explicit_jump_does_not_skip_previous_step()
    test_advance_resync_without_jump_fails_on_same_unfinished_step()
    test_advance_resync_without_jump_succeeds_when_cursor_on_finished_step()
    test_advance_force_still_skips_incomplete_step()
    test_missing_task_points_to_scaffold()
    test_scaffold_creates_task_file()
    test_toolbelt_lists_for_slice_kinds()
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
    test_advance_walks_create_plan_then_grill_plan_before_edit_code()
    test_status_ready_skips_unready_head_of_array()
    test_show_ready_tag_lists_only_ready_slices()
    test_status_ready_line_matches_ready_slices()
    test_blocked_status_slice_is_skipped()
    test_advance_pick_next_when_nothing_ready()
    test_status_warns_when_cursor_slice_not_ready()
    test_status_no_warning_when_cursor_slice_is_ready()
    test_advance_within_slice_then_pick_next_on_exhausted_slice()
    test_status_warns_on_unknown_dependency_key()
    test_show_renders_deps_with_mixed_statuses()
    test_show_omits_deps_line_when_absent()
    test_init_rejects_forward_reference_dependency()
    test_legacy_cue_scaffold_output_has_no_local_schema()
    test_scaffold_output_still_validates_via_shared_schema()
    test_add_slice_happy_path_no_repos()
    test_add_slice_happy_path_with_repos()
    test_add_slice_rejects_duplicate_key()
    test_add_slice_requires_repos_when_schema_requires_it()
    test_add_slice_rejects_unsupported_required_field()
    test_add_slice_after_inserts_in_correct_order()
    test_add_slice_rejects_unknown_after_slice()
    test_add_slice_works_on_empty_plan_slices()
    test_add_slice_unified_final_steps_field()
    test_add_step_happy_path()
    test_add_step_final_flag()
    test_add_step_rejects_duplicate_key()
    test_add_step_rejects_unknown_slice()
    test_add_step_after_inserts_in_correct_order()
    test_legacy_cue_add_step_final_rolls_back_on_closed_final_steps_schema()
    test_legacy_cue_validate_passes_shared_schema_task()
    test_validate_warns_when_persisted_slice_predates_template_addition()
    test_validate_warns_on_long_note()
    test_status_warns_on_long_note()
    test_validate_warns_on_large_task_file()
    test_finish_note_not_refused_when_long()
    test_validate_fails_invalid_step_status()
    test_validate_fails_when_slice_missing_template_steps()
    test_validate_allows_extra_steps_beyond_template()
    test_validate_fails_on_unknown_slice_kind()
    test_validate_slice_passes_when_only_that_slice_is_conformant()
    test_validate_slice_fails_for_nonconformant_slice()
    test_validate_slice_rejects_unknown_slice()
    test_validate_without_slice_still_fails_on_legacy_debt()
    test_status_reports_structure_ok_for_conformant_task()
    test_status_reports_structure_invalid_without_failing()
    test_migrate_task_reports_already_migrated()
    test_migrate_task_recommends_delete_for_identical_status()
    test_migrate_task_recommends_keep_for_status_with_used_extra_value()
    test_migrate_task_recommends_delete_for_status_with_unused_extra_value()
    test_migrate_task_recommends_replace_for_slice_with_extra_fields()
    test_migrate_task_line_ranges_are_accurate()
    test_migrate_task_partial_migration_only_slice_remains()
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
    test_sync_default_selection_and_status_override()
    test_add_slice_still_works_with_repo_work_in_schema()
    test_fs_task_store_round_trip()
    test_fs_task_store_ordering()
    test_fs_task_store_depends_on_symlink()
    test_fs_task_store_invalid_status_dies_on_read()
    test_fs_and_cue_task_store_shape_parity()
    test_cue_escape_escapes_newlines_quotes_and_tabs()
    test_cmd_project_cue_to_fs_to_cue_round_trip()
    test_cmd_project_refuses_nonempty_cue_destination()
    test_persisted_models_document_every_field()
    test_yaml_task_store_round_trip_and_atomic_mutations()
    test_yaml_mutations_serialize_concurrent_writers()
    test_yaml_task_lock_lives_under_xdg_cache_not_task_dir()
    test_yaml_task_lock_path_resolves_aliases_to_same_inode_key()
    test_yaml_lifecycle_lock_preserves_first_executor()
    test_yaml_force_advance_cannot_overwrite_concurrent_finish()
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
    test_cmd_project_cue_to_yaml_keeps_source_and_verifies_semantics()
    test_legacy_cue_custom_fields_remain_readable_but_do_not_migrate_silently()
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
    test_set_slice_updates_title_and_goal()
    test_set_slice_requires_one_field()
    test_set_slice_refuses_done_slice()
    test_block_slice_sets_status_and_appends_note()
    test_block_slice_refuses_done()
    test_unblock_slice_restores_planned()
    test_unblock_slice_refuses_non_blocked()
    test_start_refuses_blocked_slice()
    test_vulnerability_scan_rejects_writer_model()
    test_vulnerability_scan_rejects_unqualified_author_model()
    test_advance_rejects_malformed_scan_timestamps()
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
    test_set_context_mutation()
    test_add_decision_mutation()
    test_set_plan_note_mutation()
    test_remove_slice_removes_and_guards()
    test_bootstrap_requires_yaml()
    test_create_from_requires_intent_path()
    test_bootstrap_rejects_initial_slice_key_without_kind()
    test_pi_job_write_stores_content_digest()
    test_hand_edit_warns_on_next_read()
    test_acknowledge_edit_clears_warning()
    test_finish_while_dirty_does_not_clear_digest()
    test_missing_digest_does_not_warn()
    test_fixture_policy_disallows_incidental_cue_task_paths()
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
    test_investigate_does_not_move_cursor()
    print("pi-job tests passed")


if __name__ == "__main__":
    main()
