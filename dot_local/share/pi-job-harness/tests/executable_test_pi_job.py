#!/usr/bin/env python3
"""Regression tests for packages/pi-job-harness/bin/pi-job."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PI_JOB = Path(__file__).resolve().parents[1] / "bin" / "pi-job"


def load_pi_job_module():
    """Import pi-job (no .py suffix, chezmoi's executable_ naming) as a module so tests
    can exercise FsTaskStore/TaskLayout/CueTaskStore/Cursor directly instead of only via
    subprocess. Safe: `main()` only runs under `if __name__ == "__main__":`."""
    loader = importlib.machinery.SourceFileLoader("pi_job_under_test", str(PI_JOB))
    spec = importlib.util.spec_from_file_location("pi_job_under_test", PI_JOB, loader=loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module  # dataclasses needs the module registered before exec
    loader.exec_module(module)
    return module

TASK_PREAMBLE = """
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

PLAN_BODY = """
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

PLAN_BODY_WITH_DEPENDENCIES = """
    plan: {
        slices: [
            #Slice & {
                key: "base"
                kind:   "implement"
                title: "Base"
                goal: "Already done"
                status: "done"
                note: ""
                steps: []
                final_steps: []
            },
            #Slice & {
                key: "blocked-dependent"
                kind:   "implement"
                title: "Blocked Dependent"
                goal: "Depends on not-yet-done"
                status: "planned"
                note: ""
                depends_on: ["not-yet-done"]
                steps: []
                final_steps: []
            },
            #Slice & {
                key: "ready-dependent"
                kind:   "implement"
                title: "Ready Dependent"
                goal: "Depends on base (done)"
                status: "planned"
                note: ""
                depends_on: ["base"]
                steps: []
                final_steps: []
            },
            #Slice & {
                key: "blocked-status-slice"
                kind:   "implement"
                title: "Blocked Status"
                goal: "Has blocked status"
                status: "blocked"
                note: ""
                steps: []
                final_steps: []
            },
        ]
    }
"""

TASK_FIXTURE = TASK_PREAMBLE + """
task: {
    title: "Fixture task"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    orchestration: {
        cursor: {
            slice: "old-slice"
            step:  "old-step"
        }
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
""" + PLAN_BODY + """
}
"""

UNINITIALIZED_TASK_FIXTURE = TASK_PREAMBLE + """
task: {
    title: "Uninitialized fixture task"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
""" + PLAN_BODY + """
}
"""


def run(*args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    res = subprocess.run(args, cwd=ROOT, text=True, capture_output=True)
    if check and res.returncode != 0:
        raise AssertionError(f"command failed: {' '.join(args)}\nSTDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}")
    return res


def assert_contains(haystack: str, needle: str) -> None:
    if needle not in haystack:
        raise AssertionError(f"expected {needle!r} in:\n{haystack}")


def test_profiled_task() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "fixture.cue"
        task.write_text(TASK_FIXTURE)

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: ok")
        assert_contains(status, "Cursor: old-slice / old-step")
        assert_contains(status, "Next: second-slice / s2")

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "second-slice / s2", nxt

        dry = run(str(PI_JOB), "--task", str(task), "advance", "--dry-run").stdout
        assert_contains(dry, 'slice: "second-slice"')
        assert_contains(dry, 'step:  "s2"')

        run(str(PI_JOB), "--task", str(task), "advance")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: second-slice / s2")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "PI-JOB EXECUTION INSTRUCTION")
        assert_contains(instruction, "Owner: orchestrator")
        assert_contains(instruction, "Slice: second-slice [implement]")
        assert_contains(instruction, "Slice goal: Find next planned step")
        assert_contains(instruction, "Step: s2 — Next")
        assert_contains(instruction, "Execute this step in the main orchestration session.")

        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_contains(plan, "PI-JOB TASK PLAN")
        assert_contains(plan, "second-slice [kind:implement/in_progress]")
        assert_contains(plan, "Use the session todo capability to track this task plan.")

        assert_contains(instruction, "Todo tracking:")
        assert_contains(instruction, "Keep session todos aligned with `pi-job plan`")


def test_uninitialized_task_requires_orchestration() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "uninitialized.cue"
        task.write_text(UNINITIALIZED_TASK_FIXTURE)

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Initialization: required")
        assert_contains(status, "init [--kind setup|implement|...]")

        nxt = run(str(PI_JOB), "--task", str(task), "next", check=False)
        if nxt.returncode == 0:
            raise AssertionError("next unexpectedly succeeded for uninitialized task")
        assert_contains(nxt.stderr, "missing task.orchestration")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if instruction.returncode == 0:
            raise AssertionError("instruction unexpectedly succeeded for uninitialized task")
        assert_contains(instruction.stderr, "missing task.orchestration")

        init_dry = run(str(PI_JOB), "--task", str(task), "init", "--dry-run").stdout
        assert_contains(init_dry, 'slice: "second-slice"')
        assert_contains(init_dry, 'step:  "s2"')

        run(str(PI_JOB), "--task", str(task), "init")
        initialized = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(initialized, "Initialization: ok")
        assert_contains(initialized, "Cursor: second-slice / s2")


def test_init_with_kind_setup_seeds_setup_slice() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty-plan.cue"
        task.write_text(TASK_PREAMBLE + """
task: {
    title: "Empty plan"
    status: "in_progress"
    plan: {note: "", slices: []}
}
""")
        run(str(PI_JOB), "--task", str(task), "init", "--kind", "setup")
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Cursor: setup-slice / explore-context")
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "setup-slice")
        assert_contains(show, "explore-context")


def test_edit_code_owner_from_step_kinds() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "edit-code-owner.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Edit code owner"
    status: "in_progress"
    orchestration: {
        cursor: {slice: "only-slice", step: "edit-code"}
        policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}
    }
    plan: {
        slices: [
            #Slice & {
                key: "only-slice"
                kind: "implement"
                title: "Only"
                goal: "Check owner"
                status: "in_progress"
                note: ""
                steps: [
                    #Step & {key: "create-plan", title: "Plan", status: "done", note: ""},
                    #Step & {key: "grill-plan", title: "Grill", status: "done", note: ""},
                    #Step & {key: "edit-code", title: "Edit", status: "planned", note: ""},
                ]
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)
        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Owner: subagent")
        assert_contains(instruction, "Step kind: edit-code")
        assert_contains(instruction, "Run this step in a subagent.")


ALL_DONE_PLAN_BODY = """
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
                goal: "Also done"
                status: "done"
                note: ""
                steps: [
                    #Step & {key: "s1", title: "Done", status: "done", note: ""},
                    #Step & {key: "s2", title: "Also done", status: "done", note: ""},
                ]
                final_steps: [
                    #Step & {key: "finish", title: "Finish", status: "done", note: ""},
                ]
            },
        ]
    }
"""


CLOSING_PLAN_BODY = """
    plan: {
        slices: [
            #Slice & {
                key: "implement-done"
                kind: "implement"
                title: "Implement done"
                goal: "Already done"
                status: "done"
                note: ""
                steps: [
                    #Step & {key: "edit-code", title: "Edit", status: "done", note: ""},
                ]
                final_steps: []
            },
            #Slice & {
                key: "closing"
                kind: "closing"
                title: "Closing"
                goal: "Cross-slice bookkeeping"
                status: "planned"
                note: ""
                steps: [
                    #Step & {key: "update-test-plan", title: "Update test plan", status: "planned", note: ""},
                    #Step & {key: "update-docs", title: "Update docs", status: "planned", note: ""},
                    #Step & {key: "capture-metrics", title: "Capture metrics", status: "planned", note: ""},
                    #Step & {key: "update-task-file", title: "Update task file", status: "planned", note: ""},
                ]
                final_steps: []
            },
        ]
    }
"""


def test_next_walks_to_closing_slice_after_implement_done() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "all-implement-done.cue"
        fixture = (
            TASK_PREAMBLE
            + """
task: {
    title: "Implement done fixture"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    orchestration: {
        cursor: {
            slice: "implement-done"
            step:  "edit-code"
        }
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
"""
            + CLOSING_PLAN_BODY
            + "\n}\n"
        )
        task.write_text(fixture)

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "closing / update-test-plan", nxt

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Next: closing / update-test-plan")

        run(str(PI_JOB), "--task", str(task), "advance")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: closing / update-test-plan")


def test_next_done_when_all_slices_finished() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "all-slices-done.cue"
        fixture = (
            TASK_PREAMBLE
            + """
task: {
    title: "All slices done fixture"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    orchestration: {
        cursor: {
            slice: "second-slice"
            step:  "finish"
        }
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
"""
            + ALL_DONE_PLAN_BODY
            + "\n}\n"
        )
        task.write_text(fixture)

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "done", nxt

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Next: done")


def test_advance_blocked_on_incomplete_current_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "incomplete.cue"
        fixture = TASK_FIXTURE.replace(
            'slice: "old-slice"\n            step:  "old-step"',
            'slice: "second-slice"\n            step:  "s2"',
        )
        task.write_text(fixture)

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
        task2 = Path(tmp) / "incomplete-dry.cue"
        task2.write_text(fixture)
        dry = run(str(PI_JOB), "--task", str(task2), "advance", "--dry-run").stdout
        assert_contains(dry, "slice:")


def test_missing_task_points_to_scaffold() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        missing = Path(tmp) / "missing.cue"
        res = run(str(PI_JOB), "--task", str(missing), "status", check=False)
        if res.returncode == 0:
            raise AssertionError("status unexpectedly succeeded for missing task")
        assert_contains(res.stderr, "task file not found")
        assert_contains(res.stderr, "scaffold")
        assert_contains(res.stderr, "init [--kind")


def test_scaffold_creates_task_file() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "nested" / "new-task.cue"
        dry = run(str(PI_JOB), "--task", str(task), "scaffold", "--dry-run").stdout
        assert_contains(dry, "package task")
        assert_contains(dry, 'key:    "do-the-change"')
        if task.exists():
            raise AssertionError("dry-run wrote a task file")

        out = run(
            str(PI_JOB),
            "--task",
            str(task),
            "scaffold",
            "--title",
            "Scaffolded example",
        ).stdout
        assert_contains(out, f"scaffolded task file: {task}")
        assert task.exists()

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Task: Scaffolded example")
        assert_contains(status, "Initialization: required")

        again = run(str(PI_JOB), "--task", str(task), "scaffold", check=False)
        if again.returncode == 0:
            raise AssertionError("scaffold unexpectedly overwrote without --force")
        assert_contains(again.stderr, "already exists")


def test_toolbelt_lists_for_slice_kinds() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        implement_task = Path(tmp) / "implement.cue"
        implement_task.write_text(TASK_FIXTURE)
        out = run(str(PI_JOB), "--task", str(implement_task), "toolbelt").stdout
        assert_contains(out, "implement")
        for key in ("httpyac-api-spec", "test-case-table"):
            assert_contains(out, key)
        assert_contains(out, "[not registered]")

        out_setup = run(str(PI_JOB), "--task", str(implement_task), "toolbelt", "--kind", "setup").stdout
        assert_contains(out_setup, "setup")
        assert_contains(out_setup, "config-flag-matrix")

        research_task = Path(tmp) / "research.cue"
        research_fixture = TASK_PREAMBLE + """
task: {
    title: "Research"
    status: "in_progress"
    orchestration: {cursor: {slice: "r", step: "explore-context"}, policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}}
    plan: {slices: [#Slice & {key: "r", kind: "research", title: "R", goal: "g", status: "in_progress", note: "", steps: [#Step & {key: "explore-context", title: "Explore", status: "planned", note: ""}], final_steps: []}]}
}
"""
        research_task.write_text(research_fixture)
        out_research = run(str(PI_JOB), "--task", str(research_task), "toolbelt").stdout
        assert_contains(out_research, "sequence-diagram")
        assert_contains(out_research, "state-transition-table")


def test_toolbelt_add_records_artifact() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "full.cue"
        task.write_text(TASK_FIXTURE)

        out = run(
            str(PI_JOB), "--task", str(task), "toolbelt", "add", "httpyac-api-spec",
            "--path", "docs/api.http", "--status", "done", "--note", "Appendix B",
        ).stdout
        assert_contains(out, "registered toolbelt aid: httpyac-api-spec [done]")

        listed = run(str(PI_JOB), "--task", str(task), "toolbelt").stdout
        assert_contains(listed, "httpyac-api-spec [done]")

        # idempotent update in place (status changes, no duplicate key)
        run(str(PI_JOB), "--task", str(task), "toolbelt", "add", "httpyac-api-spec", "--status", "planned")
        text = task.read_text()
        if text.count('"httpyac-api-spec":') != 1:
            raise AssertionError(f"expected one httpyac-api-spec entry, got {text.count(chr(34)+'httpyac-api-spec'+chr(34)+':')}")
        assert_contains(text, 'status: "planned"')

        # unknown key fails closed
        bad = run(str(PI_JOB), "--task", str(task), "toolbelt", "add", "not-a-real-aid", check=False)
        if bad.returncode == 0:
            raise AssertionError("toolbelt add unexpectedly accepted an unknown key")
        assert_contains(bad.stderr, "unknown toolbelt aid")


def test_select_toolbelt_step_and_instruction() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "setup.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Setup toolbelt"
    status: "in_progress"
    orchestration: {
        cursor: {slice: "setup-slice", step: "select-toolbelt"}
        policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}
    }
    plan: {
        slices: [
            #Slice & {
                key: "setup-slice"
                kind: "setup"
                title: "Setup"
                goal: "Pick aids"
                status: "in_progress"
                note: ""
                steps: [
                    #Step & {key: "explore-context", title: "Explore", status: "done", note: ""},
                    #Step & {key: "select-toolbelt", title: "Select toolbelt", status: "planned", note: ""},
                ]
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_contains(plan, "select-toolbelt")

        instr = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instr, "Step kind: select-toolbelt")
        assert_contains(instr, "Toolbelt (planning aids)")
        assert_contains(instr, "config-flag-matrix")


def test_toolbelt_block_in_plan() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "setup-plan.cue"
        task.write_text(TASK_PREAMBLE + """
task: {
    title: "Setup plan"
    status: "in_progress"
    orchestration: {cursor: {slice: "setup-slice", step: "select-toolbelt"}, policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}}
    plan: {slices: [#Slice & {key: "setup-slice", kind: "setup", title: "Setup", goal: "g", status: "in_progress", note: "", steps: [#Step & {key: "select-toolbelt", title: "Select", status: "planned", note: ""}], final_steps: []}]}
}
""")
        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_contains(plan, "Toolbelt (planning aids)")
        assert_contains(plan, "config-flag-matrix")


def test_show_renders_tree_and_footer() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show.cue"
        fixture = TASK_FIXTURE.replace(
            'slice: "old-slice"\n            step:  "old-step"',
            'slice: "second-slice"\n            step:  "s2"',
        )
        task.write_text(fixture)

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


def test_show_aligns_kind_counts_after_longest_key() -> None:
    """Slice headers align [kind/N/M] just after the longest key (one space), not a fixed wide column."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "align.cue"
        task.write_text(TASK_FIXTURE)
        out = run(str(PI_JOB), "--task", str(task), "show", "--color", "never").stdout
        cols = []
        for line in out.splitlines():
            if "[" in line and "/" in line and line.lstrip()[:1] in "✓⊘▸✗○":
                cols.append(line.index("["))
        if len(cols) < 2:
            raise AssertionError(f"expected >=2 slice headers with [kind/N/M]:\n{out}")
        if len(set(cols)) != 1:
            raise AssertionError(f"[kind/N/M] columns should match, got {cols}:\n{out}")
        # TASK_FIXTURE keys: "first", "second-slice" → longest 12; "✓ " + pad + " ["
        expected = 2 + len("second-slice") + 1
        if cols[0] != expected:
            raise AssertionError(f"expected [ at column {expected} (tight to longest key), got {cols[0]}:\n{out}")


def test_show_started_flag_expands_non_planned_slices() -> None:
    """By default only the current cursor's slice expands. --started additionally
    expands in_progress/blocked slices; done/skipped and still-planned stay collapsed.
    --all expands everything."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "started.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Started slices test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
        slices: [
            #Slice & {
                key: "done-not-current"
                kind:   "implement"
                title: "Done"
                goal: "Finished"
                status: "done"
                note: ""
                repos: ["graphius"]
                depends_on: ["not-started"]
                steps: [
                    #Step & {key: "done-1", title: "Step one", status: "done", note: ""},
                ]
                final_steps: []
            },
            #Slice & {
                key: "in-progress-not-current"
                kind:   "implement"
                title: "In progress"
                goal: "Started work"
                status: "in_progress"
                note: ""
                steps: [
                    #Step & {key: "ip-1", title: "Step one", status: "planned", note: ""},
                ]
                final_steps: []
            },
            #Slice & {
                key: "blocked-not-current"
                kind:   "implement"
                title: "Blocked"
                goal: "Stuck on external thing"
                status: "blocked"
                note: ""
                steps: [
                    #Step & {key: "bl-1", title: "Step one", status: "planned", note: ""},
                ]
                final_steps: []
            },
            #Slice & {
                key: "not-started"
                kind:   "implement"
                title: "Not started"
                goal: "Still queued"
                status: "planned"
                note: ""
                steps: [
                    #Step & {key: "ns-1", title: "Step one", status: "planned", note: ""},
                ]
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

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
        task = Path(tmp) / "color.cue"
        fixture = TASK_FIXTURE.replace(
            'slice: "old-slice"\n            step:  "old-step"',
            'slice: "second-slice"\n            step:  "s2"',
        )
        task.write_text(fixture)

        plain = run(str(PI_JOB), "--task", str(task), "show", "--color", "never").stdout
        colored = run(str(PI_JOB), "--task", str(task), "show", "--color", "always").stdout
        if "\033[" in plain:
            raise AssertionError(f"--color never must not emit ANSI escapes:\n{plain!r}")
        assert_contains(colored, "\033[32m✓\033[0m")  # done green
        assert_contains(colored, "\033[36m▸\033[0m")  # current / in_progress cyan
        assert_contains(colored, "\033[1;36m← current\033[0m")  # cursor marker bold cyan
        assert_contains(plain, "← current")
        if "\033[1;36m" in plain:
            raise AssertionError("--color never must not tint ← current")


def test_scaffold_includes_reconcile_artifacts() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "new.cue"
        dry = run(str(PI_JOB), "--task", str(task), "scaffold", "--dry-run").stdout
        assert_contains(dry, 'key: "reconcile-artifacts"')
        assert_contains(dry, 'key: "share-with-team"')
        # order: reconcile-artifacts after e2e-evidence, before update-task-file
        i_e2e = dry.index("e2e-evidence")
        i_rec = dry.index("reconcile-artifacts")
        i_upd = dry.index("update-task-file")
        if not (i_e2e < i_rec < i_upd):
            raise AssertionError(f"final_steps order wrong: e2e={i_e2e} reconcile={i_rec} update={i_upd}")


def test_scaffold_includes_create_plan_and_grill_plan_before_edit_code() -> None:
    """The scaffold's steps must lead with create-plan then grill-plan, before edit-code -
    modeling the per-slice planning convention for anyone reading a fresh scaffold."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "new.cue"
        dry = run(str(PI_JOB), "--task", str(task), "scaffold", "--dry-run").stdout
        assert_contains(dry, 'key: "create-plan"')
        assert_contains(dry, 'key: "grill-plan"')
        i_plan = dry.index("create-plan")
        i_grill = dry.index("grill-plan")
        i_edit = dry.index("edit-code")
        if not (i_plan < i_grill < i_edit):
            raise AssertionError(f"steps order wrong: create-plan={i_plan} grill-plan={i_grill} edit-code={i_edit}")


def test_next_walks_create_plan_then_grill_plan_before_edit_code() -> None:
    """pi-job's existing step-ordering gate, applied to the new convention: next must
    report create-plan first, then grill-plan once create-plan is done, then edit-code
    only once both are done - proving the guardrail is actually enforced, not just
    documented."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "task.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Plan-and-grill gating test"
    status: "in_progress"
    orchestration: {
        cursor: {slice: "only-slice", step: "create-plan"}
        policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}
    }
    plan: {
        slices: [
            #Slice & {
                key: "only-slice"
                kind:   "implement"
                title: "Only slice"
                goal: "Exercise the gate"
                status: "in_progress"
                note: ""
                steps: [
                    #Step & {key: "create-plan", title: "Plan", status: "planned", note: ""},
                    #Step & {key: "grill-plan", title: "Grill", status: "planned", note: ""},
                    #Step & {key: "edit-code", title: "Edit", status: "planned", note: ""},
                ]
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert_contains(nxt, "create-plan")

        text = task.read_text().replace('key: "create-plan", title: "Plan", status: "planned"', 'key: "create-plan", title: "Plan", status: "done"')
        task.write_text(text)

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert_contains(nxt, "grill-plan")

        text = task.read_text().replace('key: "grill-plan", title: "Grill", status: "planned"', 'key: "grill-plan", title: "Grill", status: "done"')
        task.write_text(text)

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert_contains(nxt, "edit-code")


def test_next_skips_unready_head_of_array() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "deps.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Dependency test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY_WITH_DEPENDENCIES + "\n}\n"
        task.write_text(fixture)

        # blocked-dependent is first but has unmet dep; ready-dependent should be next
        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "ready-dependent", nxt


def test_next_all_lists_only_ready_slices() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "deps-all.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Dependency test all"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY_WITH_DEPENDENCIES + "\n}\n"
        task.write_text(fixture)

        # next --all should only include ready-dependent (deps satisfied), not blocked-dependent or blocked-status-slice
        result = run(str(PI_JOB), "--task", str(task), "next", "--all").stdout
        lines = result.strip().split("\n")
        # Should have exactly one ready slice
        if "ready-dependent" not in result:
            raise AssertionError(f"expected ready-dependent in output:\n{result}")
        if "blocked-dependent" in result:
            raise AssertionError(f"blocked-dependent should not appear in next --all:\n{result}")
        if "blocked-status-slice" in result:
            raise AssertionError(f"blocked-status-slice should not appear in next --all:\n{result}")


def test_status_ready_line_matches_next_all() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "status-ready.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Status ready line test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY_WITH_DEPENDENCIES + "\n}\n"
        task.write_text(fixture)

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        # Should have a Ready: line with ready-dependent
        if "Ready:" not in status:
            raise AssertionError(f"expected 'Ready:' line in status:\n{status}")
        if "ready-dependent" not in status:
            raise AssertionError(f"expected 'ready-dependent' in Ready: line:\n{status}")


def test_blocked_status_slice_is_skipped() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "blocked-status.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Blocked status test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
        slices: [
            #Slice & {
                key: "only-blocked"
                kind:   "implement"
                title: "Only Blocked"
                goal: "Just a blocked slice"
                status: "blocked"
                note: ""
                steps: []
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        # A blocked slice should not appear in next (no unfinished work to do)
        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        # Should print "done" or something indicating no ready slices
        if "only-blocked" in nxt:
            raise AssertionError(f"blocked slice should not be in next cursor:\n{nxt}")


def test_next_returns_blocked_when_nothing_ready() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "nothing-ready.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Nothing ready test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
        slices: [
            #Slice & {
                key: "unmet-dep"
                kind:   "implement"
                title: "Unmet Dependency"
                goal: "Depends on something"
                status: "planned"
                note: ""
                depends_on: ["nonexistent"]
                steps: []
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        # next should print "blocked: ..." (not "done")
        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        if "done" in nxt:
            raise AssertionError(f"should not say done when slices are blocked:\n{nxt}")

        # advance should die with clear message
        adv = run(str(PI_JOB), "--task", str(task), "advance", check=False)
        if adv.returncode == 0:
            raise AssertionError("advance should fail when nothing is ready")
        if "no dependency-satisfied slice is ready" not in adv.stderr:
            raise AssertionError(f"expected clear error message in:\n{adv.stderr}")


def test_status_warns_on_stale_cursor() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "stale.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Stale cursor test"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    orchestration: {
        cursor: {
            slice: "blocked-dependent"
        }
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
""" + PLAN_BODY_WITH_DEPENDENCIES + "\n}\n"
        task.write_text(fixture)

        # Cursor points to blocked-dependent (unmet deps), but ready-dependent is actually next
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        if "stale cursor" not in status.lower() and "⚠" not in status:
            raise AssertionError(f"expected stale cursor warning in:\n{status}")


def test_status_no_warning_when_consistent() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "consistent.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Consistent cursor test"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    orchestration: {
        cursor: {
            slice: "ready-dependent"
        }
        policy: {
            coding_execution: {
                subagent_required: true
                lower_power_model_preferred: true
                orchestrator_reviews_subagent: true
            }
        }
    }
""" + PLAN_BODY_WITH_DEPENDENCIES + "\n}\n"
        task.write_text(fixture)

        # Cursor points to ready-dependent, which is actually next (deps are met)
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        if "stale" in status.lower():
            raise AssertionError(f"should not warn about stale cursor when consistent:\n{status}")


def test_status_warns_on_unknown_dependency_key() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-dep.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Unknown dependency test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
        slices: [
            #Slice & {
                key: "bad-dep"
                kind:   "implement"
                title: "Bad Dependency"
                goal: "Has typo in dep"
                status: "planned"
                note: ""
                depends_on: ["nonexistent-slice"]
                steps: []
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        if "depends_on unknown slice key" not in status and "⚠" not in status:
            raise AssertionError(f"expected unknown dependency warning in:\n{status}")


def test_show_renders_deps_with_mixed_statuses() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-deps.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Show deps test"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY_WITH_DEPENDENCIES + "\n}\n"
        task.write_text(fixture)

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
        task = Path(tmp) / "show-no-deps.cue"
        task.write_text(TASK_FIXTURE)

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        # Existing fixture has no depends_on, should not show deps lines
        if "deps:" in show:
            raise AssertionError(f"should not show deps: line when no depends_on:\n{show}")


def test_init_rejects_forward_reference_dependency() -> None:
    """Regression test: cmd_init() must use slice_work_contract_phase() not hardcoded 'implement'."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "forward-ref.cue"
        fixture = TASK_PREAMBLE + """
task: {
    title: "Forward reference dependency test"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    plan: {
        slices: [
            #Slice & {
                key: "first-slice"
                kind:   "implement"
                title: "First"
                goal: "Depends on nonexistent slice"
                status: "planned"
                note: ""
                depends_on: ["nonexistent-slice"]
                steps: []
                final_steps: []
            },
        ]
    }
}
"""
        task.write_text(fixture)

        # init with full profile on a task with unmet dependency should die, not guess "implement"
        init_res = run(str(PI_JOB), "--task", str(task), "init", check=False)
        if init_res.returncode == 0:
            raise AssertionError("init unexpectedly succeeded with forward-reference dependency in full profile")
        assert_contains(init_res.stderr, "no slice is dependency-satisfied yet")


def test_scaffold_output_has_no_local_schema() -> None:
    """Scaffold output should not contain local #Slice:/#Status: definitions."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "schema-test.cue"
        dry = run(str(PI_JOB), "--task", str(task), "scaffold", "--dry-run").stdout
        if "#Status:" in dry or "#Step:" in dry or "#Slice:" in dry or "#Decision:" in dry or "#Artifact:" in dry:
            raise AssertionError(f"scaffold dry-run should not contain local type definitions:\n{dry}")


def test_scaffold_output_still_validates_via_shared_schema() -> None:
    """Real (non-dry-run) scaffold, then pi-job status/show succeed against it."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "schema-validate.cue"
        run(str(PI_JOB), "--task", str(task), "scaffold")

        # status and show should work without errors
        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Task:")

        # Initialize profile first
        run(str(PI_JOB), "--task", str(task), "init")

        show = run(str(PI_JOB), "--task", str(task), "show").stdout
        assert_contains(show, "do-the-change")


def test_add_slice_happy_path_no_repos() -> None:
    """Dry-run and real add-slice on a no-repos fixture; verify output and final state."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice.cue"
        task.write_text(TASK_FIXTURE)

        # dry-run should show the literal
        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New Slice", "--goal", "Do work", "--kind", "implement", "--dry-run").stdout
        assert_contains(dry, 'key: "new-slice"')
        assert_contains(dry, 'title: "New Slice"')
        assert_contains(dry, 'goal: "Do work"')

        # real write
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New Slice", "--goal", "Do work", "--kind", "implement")

        # show should list the new slice
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new-slice")


def test_add_slice_rejects_duplicate_key() -> None:
    """add-slice with duplicate key dies."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "dup-key.cue"
        task.write_text(TASK_FIXTURE)
        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "first", "--title", "Duplicate", "--goal", "Should fail", "--kind", "implement", check=False)
        if res.returncode == 0:
            raise AssertionError("add-slice should reject duplicate key")
        assert_contains(res.stderr, "already exists")


def test_add_slice_after_inserts_in_correct_order() -> None:
    """add-slice --after places slice after existing one."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "after.cue"
        task.write_text(TASK_FIXTURE)

        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "between", "--title", "Between", "--goal", "In middle", "--kind", "implement", "--after", "first")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        lines = show.split("\n")
        first_idx = next((i for i, l in enumerate(lines) if "first" in l), -1)
        between_idx = next((i for i, l in enumerate(lines) if "between" in l), -1)
        second_idx = next((i for i, l in enumerate(lines) if "second-slice" in l), -1)
        if not (0 <= first_idx < between_idx < second_idx):
            raise AssertionError(f"order wrong: first={first_idx}, between={between_idx}, second={second_idx}")


def test_add_slice_rejects_unknown_after_slice() -> None:
    """add-slice --after with unknown slice dies."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-after.cue"
        task.write_text(TASK_FIXTURE)
        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new", "--title", "New", "--goal", "Work", "--kind", "implement", "--after", "nonexistent", check=False)
        if res.returncode == 0:
            raise AssertionError("add-slice should reject unknown --after slice")
        assert_contains(res.stderr, "not found")


def test_add_slice_works_on_empty_plan_slices() -> None:
    """add-slice works even on plan.slices: [] fixture (shared schema)."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "empty-slices.cue"
        # Create a minimal fixture with empty slices
        fixture = TASK_PREAMBLE + """
task: {
    title: "Empty plan"
    status: "in_progress"
    project: {
        name: "Empty"
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
        task.write_text(fixture)

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
        task = Path(tmp) / "add-step.cue"
        task.write_text(TASK_FIXTURE)

        # dry-run
        dry = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "new-step", "--title", "New Step", "--dry-run").stdout
        assert_contains(dry, 'key: "new-step"')
        assert_contains(dry, 'title: "New Step"')

        # real write
        run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "new-step", "--title", "New Step")

        # show should list it
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new-step")


def test_add_step_final_flag() -> None:
    """add-step --final places step in final_steps."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "final-step.cue"
        task.write_text(TASK_FIXTURE)

        run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "final-new", "--title", "Final Step", "--final")

        text = task.read_text()
        if "final_steps" not in text or text.index('key: "final-new"') < text.rindex("final_steps"):
            # Key might appear after final_steps declaration
            pass
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "final-new")


def test_add_step_rejects_duplicate_key() -> None:
    """add-step rejects duplicate key in same slice."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "dup-step.cue"
        task.write_text(TASK_FIXTURE)
        res = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "s1", "--title", "Duplicate", check=False)
        if res.returncode == 0:
            raise AssertionError("add-step should reject duplicate key")
        assert_contains(res.stderr, "already exists")


def test_add_step_rejects_unknown_slice() -> None:
    """add-step with unknown slice dies."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-slice.cue"
        task.write_text(TASK_FIXTURE)
        res = run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "nonexistent", "--key", "step", "--title", "Step", check=False)
        if res.returncode == 0:
            raise AssertionError("add-step should reject unknown slice")
        assert_contains(res.stderr, "slice not found")


def test_add_step_after_inserts_in_correct_order() -> None:
    """add-step --after places step after existing one."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "step-after.cue"
        task.write_text(TASK_FIXTURE)

        run(str(PI_JOB), "--task", str(task), "add-step", "--slice", "second-slice", "--key", "s1b", "--title", "Between", "--after", "s1")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        text = show
        idx_s1 = text.index("s1")
        idx_s1b = text.index("s1b")
        idx_s2 = text.index("s2")
        if not (idx_s1 < idx_s1b < idx_s2):
            raise AssertionError(f"step order wrong: s1={idx_s1}, s1b={idx_s1b}, s2={idx_s2}")


def test_add_step_final_rolls_back_on_closed_final_steps_schema() -> None:
    """add-step --final on a closed-length final_steps rolls back cleanly."""
    with tempfile.TemporaryDirectory() as tmp:
        # Use the bootstrap task which has a closed 2-element final_steps
        # Copy it to tmp to avoid modifying original
        bootstrap_file = Path("/home/volodymyrvitvitskyi/proj/weight-loss/projects/pi-agent-job-harness/tasks/2026-07-09-bootstrap-pi-agent-job-harness.cue")
        if not bootstrap_file.exists():
            # Skip if the real file doesn't exist in this test environment
            return

        task = Path(tmp) / "closed-final.cue"
        original_content = bootstrap_file.read_text()
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
        task = Path(tmp) / "with-repos.cue"
        # Build a task with optional repos field
        preamble_with_repos = """
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
    depends_on?: [...string]
    repos?: [...string]
    steps: [...#Step]
    final_steps: [...#Step]
}
"""
        fixture_with_repos = preamble_with_repos + """
task: {
    title: "Fixture with repos"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY + """
}
"""
        task.write_text(fixture_with_repos)

        # dry-run with repos
        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "repo-slice", "--title", "Repo Slice", "--goal", "Work on repos", "--kind", "implement", "--repos", "graphius,darius", "--dry-run").stdout
        assert_contains(dry, 'key: "repo-slice"')
        assert_contains(dry, 'repos: ["graphius", "darius"]')

        # real write
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "repo-slice", "--title", "Repo Slice", "--goal", "Work on repos", "--kind", "implement", "--repos", "graphius,darius")

        # Verify show lists it
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo-slice")

        # Verify the file is still valid (can export)
        run(str(PI_JOB), "--task", str(task), "show")


def test_add_slice_requires_repos_when_schema_requires_it() -> None:
    """add-slice dies when repos is required by schema but not provided."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "repos-required.cue"
        # Build a task with required repos field (no ?)
        preamble_repos_required = """
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
"""
        fixture_repos_required = preamble_repos_required + """
task: {
    title: "Repos required"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY + """
}
"""
        task.write_text(fixture_repos_required)

        # Try to add without --repos; should fail
        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new", "--title", "New", "--goal", "Work", "--kind", "implement", check=False)
        if res.returncode == 0:
            raise AssertionError("add-slice should require --repos when schema requires it")
        assert_contains(res.stderr, "repos")
        assert_contains(res.stderr, "requires")


def test_add_slice_rejects_unsupported_required_field() -> None:
    """add-slice dies when schema has a required field the CLI doesn't support."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unsupported-field.cue"
        # Build a task with an unsupported required field (owner)
        preamble_with_owner = """
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
    owner: string
    steps: [...#Step]
    final_steps: [...#Step]
}
"""
        fixture_with_owner = preamble_with_owner + """
task: {
    title: "With owner field"
    status: "in_progress"
    project: {
        name: "Fixture"
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
""" + PLAN_BODY + """
}
"""
        task.write_text(fixture_with_owner)

        # Try to add; should fail because owner is required but unsupported
        res = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new", "--title", "New", "--goal", "Work", "--kind", "implement", check=False)
        if res.returncode == 0:
            raise AssertionError("add-slice should reject unsupported required fields")
        assert_contains(res.stderr, "owner")


def test_validate_passes_shared_schema_task() -> None:
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
                steps: [#Step & {key: "create-plan", title: "Plan", status: "planned", note: ""}]
                final_steps: []
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
""" + PLAN_BODY + """
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
        fixture = TASK_PREAMBLE + """
task: {
    title: "Identical status"
    status: "in_progress"
    project: {
        name: "Test"
    }
""" + PLAN_BODY + """
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
        task = Path(tmp) / "worktree.cue"
        task.write_text(TASK_FIXTURE)

        # dry-run shows the literal
        dry = run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1", "--dry-run").stdout
        assert_contains(dry, 'worktree: "/tmp/wt1"')

        # real write
        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")

        # show should render it
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo_work[graphius]")
        assert_contains(show, "worktree=/tmp/wt1")


def test_set_worktree_upserts_existing_path() -> None:
    """set-worktree twice with different paths; show contains only the latest."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "worktree-upsert.cue"
        task.write_text(TASK_FIXTURE)

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
        task = Path(tmp) / "worktree-bad-slice.cue"
        task.write_text(TASK_FIXTURE)

        res = run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "nonexistent", "--repo", "graphius", "--path", "/tmp/wt", check=False)
        if res.returncode == 0:
            raise AssertionError("set-worktree should reject unknown slice")
        assert_contains(res.stderr, "slice not found")


def test_add_pr_happy_path_creates_repo_work() -> None:
    """add-pr with no prior set-worktree auto-creates repo entry with worktree absent."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pr-happy.cue"
        task.write_text(TASK_FIXTURE)

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
        task = Path(tmp) / "pr-upsert.cue"
        task.write_text(TASK_FIXTURE)

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
        task = Path(tmp) / "pr-bad-slice.cue"
        task.write_text(TASK_FIXTURE)

        res = run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "nonexistent", "--repo", "graphius", "--url", "https://github.com/example/pr/1", "--status", "open", check=False)
        if res.returncode == 0:
            raise AssertionError("add-pr should reject unknown slice")
        assert_contains(res.stderr, "slice not found")


def test_add_pr_after_set_worktree_preserves_worktree() -> None:
    """set-worktree then add-pr on same slice/repo; both survive in show."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "pr-with-worktree.cue"
        task.write_text(TASK_FIXTURE)

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/tmp/wt1")
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", "https://github.com/example/pr/1", "--status", "open")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "repo_work[graphius]")
        assert_contains(show, "worktree=/tmp/wt1")
        assert_contains(show, "pr open https://github.com/example/pr/1")


def test_show_renders_repo_work_worktree_and_prs() -> None:
    """show renders both worktree path and PR status/url/note substrings."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "show-repo-work.cue"
        task.write_text(TASK_FIXTURE)

        run(str(PI_JOB), "--task", str(task), "set-worktree", "--slice", "second-slice", "--repo", "graphius", "--path", "/home/user/worktrees/graphius")
        run(str(PI_JOB), "--task", str(task), "add-pr", "--slice", "second-slice", "--repo", "graphius", "--url", "https://github.com/emed/graphius/pull/123", "--status", "open", "--note", "WIP schema changes")

        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        # Verify all the rendering parts are present
        assert_contains(show, "repo_work[graphius]: worktree=/home/user/worktrees/graphius")
        assert_contains(show, "pr open https://github.com/emed/graphius/pull/123 — WIP schema changes")


def test_add_slice_still_works_with_repo_work_in_schema() -> None:
    """Regression: add-slice --dry-run doesn't mention repo_work, and real add-slice succeeds."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "add-slice-regression.cue"
        task.write_text(TASK_FIXTURE)

        # dry-run should not include repo_work
        dry = run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New", "--goal", "Work", "--kind", "implement", "--dry-run").stdout
        if "repo_work" in dry:
            raise AssertionError(f"add-slice dry-run should not mention repo_work:\n{dry}")

        # real add-slice should still succeed
        run(str(PI_JOB), "--task", str(task), "add-slice", "--key", "new-slice", "--title", "New", "--goal", "Work", "--kind", "implement")

        # show should include the new slice
        show = run(str(PI_JOB), "--task", str(task), "show", "--all").stdout
        assert_contains(show, "new-slice")


SYNC_FIXTURE_CUE = """package task

task: {
	title:  "Sync fixture"
	status: "in_progress"
	source: {jira: "", discovered: "", context: ""}
	project: {key: "sync", name: "Sync", route: "", context: ""}
	context: ""
	orchestration: {
		cursor: {slice: "only-slice", step: "create-plan"}
		policy: {coding_execution: {subagent_required: true, lower_power_model_preferred: true, orchestrator_reviews_subagent: true}}
	}
	decisions: []
	plan: {
		note: ""
		slices: [
			#Slice & {
				key: "active-slice"
                kind:   "implement", title: "Active", goal: "g", status: "in_progress", note: ""
				steps: [#Step & {key: "s1", title: "s1", status: "in_progress", note: ""}]
				final_steps: []
			},
			#Slice & {
				key: "blocked-slice"
                kind:   "implement", title: "Blocked", goal: "g", status: "blocked", note: ""
				steps: [#Step & {key: "s1", title: "s1", status: "planned", note: ""}]
				final_steps: []
			},
			#Slice & {
				key: "planned-slice"
                kind:   "implement", title: "Planned", goal: "g", status: "planned", note: ""
				steps: [#Step & {key: "s1", title: "s1", status: "planned", note: ""}]
				final_steps: []
			},
			#Slice & {
				key: "done-with-open-pr"
                kind:   "implement", title: "Done but PR open", goal: "g", status: "done", note: ""
				repo_work: {"some-repo": {prs: [#PR & {url: "https://example.com/pr/9", status: "open", note: ""}]}}
				steps: []
				final_steps: []
			},
		]
	}
}
"""


def test_sync_default_selection_and_status_override() -> None:
    """Default sync selection: in_progress/blocked slices, plus any slice carrying an
    open PR even if its own status is done. --status overrides to an exact status set."""
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "sync.cue"
        task.write_text(SYNC_FIXTURE_CUE)

        default_out = run(str(PI_JOB), "--task", str(task), "sync").stdout
        assert_contains(default_out, "active-slice")
        assert_contains(default_out, "blocked-slice")
        assert_contains(default_out, "done-with-open-pr")
        if "planned-slice" in default_out:
            raise AssertionError(f"planned-slice (no PR, not in_progress/blocked) should be excluded by default:\n{default_out}")

        status_out = run(str(PI_JOB), "--task", str(task), "sync", "--status", "planned").stdout
        assert_contains(status_out, "planned-slice")
        for excluded in ("active-slice", "blocked-slice", "done-with-open-pr"):
            if excluded in status_out:
                raise AssertionError(f"{excluded} should be excluded by --status planned override:\n{status_out}")


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
					#Step & {key: "edit-code", title: "Add \\"on-hold\\": \\"mapped\\"", status: "done", note: "Done already.\\n\\nUpdated later with more detail."},
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


def main() -> None:
    test_profiled_task()
    test_uninitialized_task_requires_orchestration()
    test_init_with_kind_setup_seeds_setup_slice()
    test_edit_code_owner_from_step_kinds()
    test_next_walks_to_closing_slice_after_implement_done()
    test_next_done_when_all_slices_finished()
    test_advance_blocked_on_incomplete_current_step()
    test_missing_task_points_to_scaffold()
    test_scaffold_creates_task_file()
    test_toolbelt_lists_for_slice_kinds()
    test_toolbelt_add_records_artifact()
    test_select_toolbelt_step_and_instruction()
    test_toolbelt_block_in_plan()
    test_show_renders_tree_and_footer()
    test_show_aligns_kind_counts_after_longest_key()
    test_show_started_flag_expands_non_planned_slices()
    test_show_color_always_tints_glyphs_never_stays_plain()
    test_scaffold_includes_reconcile_artifacts()
    test_scaffold_includes_create_plan_and_grill_plan_before_edit_code()
    test_next_walks_create_plan_then_grill_plan_before_edit_code()
    test_next_skips_unready_head_of_array()
    test_next_all_lists_only_ready_slices()
    test_status_ready_line_matches_next_all()
    test_blocked_status_slice_is_skipped()
    test_next_returns_blocked_when_nothing_ready()
    test_status_warns_on_stale_cursor()
    test_status_no_warning_when_consistent()
    test_status_warns_on_unknown_dependency_key()
    test_show_renders_deps_with_mixed_statuses()
    test_show_omits_deps_line_when_absent()
    test_init_rejects_forward_reference_dependency()
    test_scaffold_output_has_no_local_schema()
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
    test_add_step_final_rolls_back_on_closed_final_steps_schema()
    test_validate_passes_shared_schema_task()
    test_validate_fails_invalid_step_status()
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
    print("pi-job tests passed")


if __name__ == "__main__":
    main()
