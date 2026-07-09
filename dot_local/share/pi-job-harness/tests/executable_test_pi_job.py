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
        assert_contains(status, "Next: second_slice / second-slice / s2")

        nxt = run(str(PI_JOB), "--task", str(task), "next").stdout.strip()
        assert nxt == "second_slice / second-slice / s2", nxt

        dry = run(str(PI_JOB), "--task", str(task), "advance", "--dry-run").stdout
        assert_contains(dry, 'phase: "second_slice"')
        assert_contains(dry, 'step:  "s2"')

        run(str(PI_JOB), "--task", str(task), "advance")
        advanced = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(advanced, "Cursor: second_slice / second-slice / s2")

        instruction = run(str(PI_JOB), "--task", str(task), "instruction", "--current").stdout
        assert_contains(instruction, "PI-JOB EXECUTION INSTRUCTION")
        assert_contains(instruction, "Owner: orchestrator")
        assert_contains(instruction, "Contract:")
        assert_contains(instruction, "Execute this step in the main orchestration session.")

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
        assert_contains(init_dry, 'phase: "second_slice"')

        run(str(PI_JOB), "--task", str(task), "init", "--profile", "small")
        initialized = run(str(PI_JOB), "--task", str(task), "status").stdout
        assert_contains(initialized, "Profile: small")
        assert_contains(initialized, "Initialization: ok")
        assert_contains(initialized, "Cursor: second_slice / second-slice / s2")


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


def main() -> None:
    test_profiled_task()
    test_uninitialized_task_requires_profile()
    test_contract_owner_for_implement_phase()
    print("pi-job tests passed")


if __name__ == "__main__":
    main()
