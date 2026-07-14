#!/usr/bin/env python3
"""Regression tests for packages/pi-job-harness/bin/pi-job."""

from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PI_JOB = Path(__file__).resolve().parents[1] / "bin" / "pi-job"

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
    title: string
    goal: string
    status: #Status
    note: string
    steps: [...#Step]
    final_steps: [...#Step]
}
"""

PLAN_BODY = """
    plan: {
        slices: [
            #Slice & {
                key: "first"
                title: "First"
                goal: "Already done"
                status: "done"
                note: ""
                steps: []
                final_steps: []
            },
            #Slice & {
                key: "second-slice"
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

TASK_FIXTURE = TASK_PREAMBLE + """
task: {
    title: "Fixture task"
    status: "in_progress"
    project: {
        name: "Fixture"
    }
    orchestration: {
        profile: "small"
        cursor: {
            phase: "old_phase"
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
        assert_contains(status, "Cursor: old_phase / old-slice / old-step")
        assert_contains(status, "Next: implement / second-slice / s2")

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "implement / second-slice / s2", nxt

        dry = run(str(PI_JOB), "--task", str(task), "advance", "--dry-run").stdout
        assert_contains(dry, 'phase: "implement"')
        assert_contains(dry, 'step:  "s2"')

        run(str(PI_JOB), "--task", str(task), "advance")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: implement / second-slice / s2")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "PI-JOB EXECUTION INSTRUCTION")
        assert_contains(instruction, "Owner: subagent")
        assert_contains(instruction, "Contract phase: implement")
        assert_contains(instruction, "Slice: second-slice — Second")
        assert_contains(instruction, "Slice goal: Find next planned step")
        assert_contains(instruction, "Step: s2 — Next")
        assert_contains(instruction, "Run this step in a subagent.")

        plan = run(str(PI_JOB), "--task", str(task), "plan").stdout
        assert_contains(plan, "PI-JOB PROFILE PLAN")
        assert_contains(plan, "Profile: small")
        assert_contains(plan, "implement [subagent/required]")
        assert_contains(plan, "explore_context [orchestrator/required]")
        assert_contains(plan, "Use the session todo capability to track this profile plan.")

        assert_contains(instruction, "Todo tracking:")
        assert_contains(instruction, "Keep session todos aligned with `pi-job plan`")


def test_uninitialized_task_requires_profile() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "uninitialized.cue"
        task.write_text(UNINITIALIZED_TASK_FIXTURE)

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Profile: <unset>")
        assert_contains(status, "Initialization: required")
        assert_contains(status, "init --profile <profile>")

        nxt = run(str(PI_JOB), "--task", str(task), "next", check=False)
        if nxt.returncode == 0:
            raise AssertionError("next unexpectedly succeeded for uninitialized task")
        assert_contains(nxt.stderr, "missing task.orchestration.profile")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", check=False)
        if instruction.returncode == 0:
            raise AssertionError("instruction unexpectedly succeeded for uninitialized task")
        assert_contains(instruction.stderr, "missing task.orchestration.profile")

        init_dry = run(str(PI_JOB), "--task", str(task), "init", "--profile", "small", "--dry-run").stdout
        assert_contains(init_dry, 'profile: "small"')
        assert_contains(init_dry, 'phase: "implement"')

        run(str(PI_JOB), "--task", str(task), "init", "--profile", "small")
        initialized = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(initialized, "Profile: small")
        assert_contains(initialized, "Initialization: ok")
        assert_contains(initialized, "Cursor: implement / second-slice / s2")


def test_full_profile_slice_work_uses_implement_slices() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "full.cue"
        fixture = TASK_FIXTURE.replace('profile: "small"', 'profile: "full"').replace(
            'phase: "old_phase"\n            slice: "old-slice"\n            step:  "old-step"',
            'phase: "implement_slices"\n            slice: "second-slice"\n            step:  "s2"',
        )
        task.write_text(fixture)

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "implement_slices / second-slice / s2", nxt

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Owner: subagent")
        assert_contains(instruction, "Contract phase: implement_slices")
        assert_contains(instruction, "Validators: each-slice-loop-complete")
        assert_contains(instruction, "Inputs: task.plan.slices")
        assert_contains(instruction, "Outputs: slice_evidence")
        assert_contains(instruction, "Run this step in a subagent.")


def test_contract_owner_for_implement_phase() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "implement.cue"
        fixture = TASK_FIXTURE.replace(
            'phase: "old_phase"\n            slice: "old-slice"\n            step:  "old-step"',
            'phase: "implement"\n            slice: "second-slice"\n            step:  "s2"',
        )
        task.write_text(fixture)

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "Owner: subagent")
        assert_contains(instruction, "Contract phase: implement")
        assert_contains(instruction, "Run this step in a subagent.")


ALL_DONE_PLAN_BODY = """
    plan: {
        slices: [
            #Slice & {
                key: "first"
                title: "First"
                goal: "Already done"
                status: "done"
                note: ""
                steps: []
                final_steps: []
            },
            #Slice & {
                key: "second-slice"
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


def test_next_walks_profile_phases_after_slices_done() -> None:
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
        profile: "small"
        cursor: {
            phase: "implement"
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

        # small profile phase order: explore_context, clarify_if_needed, implement, verify, update_task_record.
        # Saved cursor.phase is "implement" (slice work done) -> next should be "verify".
        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "verify", nxt

        status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(status, "Next: verify")

        # Advancing should move the saved cursor to "verify" (step marked done -> not blocked).
        run(str(PI_JOB), "--task", str(task), "advance")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: verify")
        assert_contains(advanced, "Next: update_task_record")

        # One more advance walks to the final phase.
        run(str(PI_JOB), "--task", str(task), "advance")
        final_status = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(final_status, "Cursor: update_task_record")
        assert_contains(final_status, "Next: done")


def test_advance_blocked_on_incomplete_current_step() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "incomplete.cue"
        fixture = TASK_FIXTURE.replace(
            'phase: "old_phase"\n            slice: "old-slice"\n            step:  "old-step"',
            'phase: "implement"\n            slice: "second-slice"\n            step:  "s2"',
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
        assert_contains(dry, "phase:")


def test_advance_rejects_unknown_phase() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "unknown-phase.cue"
        task.write_text(TASK_FIXTURE)

        res = run(str(PI_JOB), "--task", str(task), "advance", "--phase", "not_a_real_phase", check=False)
        if res.returncode == 0:
            raise AssertionError("advance unexpectedly succeeded with an unknown --phase")
        assert_contains(res.stderr, "unknown contract phase")


def test_missing_task_points_to_scaffold() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        missing = Path(tmp) / "missing.cue"
        res = run(str(PI_JOB), "--task", str(missing), "status", check=False)
        if res.returncode == 0:
            raise AssertionError("status unexpectedly succeeded for missing task")
        assert_contains(res.stderr, "task file not found")
        assert_contains(res.stderr, "scaffold")
        assert_contains(res.stderr, "init --profile")


def test_scaffold_creates_task_file() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        task = Path(tmp) / "nested" / "new-task.cue"
        dry = run(str(PI_JOB), "--task", str(task), "scaffold", "--dry-run").stdout
        assert_contains(dry, "package task")
        assert_contains(dry, "#Status:")
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
        assert_contains(status, "Profile: <unset>")
        assert_contains(status, "Initialization: required")

        again = run(str(PI_JOB), "--task", str(task), "scaffold", check=False)
        if again.returncode == 0:
            raise AssertionError("scaffold unexpectedly overwrote without --force")
        assert_contains(again.stderr, "already exists")


def main() -> None:
    test_profiled_task()
    test_uninitialized_task_requires_profile()
    test_full_profile_slice_work_uses_implement_slices()
    test_contract_owner_for_implement_phase()
    test_next_walks_profile_phases_after_slices_done()
    test_advance_blocked_on_incomplete_current_step()
    test_advance_rejects_unknown_phase()
    test_missing_task_points_to_scaffold()
    test_scaffold_creates_task_file()
    print("pi-job tests passed")


if __name__ == "__main__":
    main()
