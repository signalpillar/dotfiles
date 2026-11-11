---
name: implement-slice
description: >-
  Run one implement slice as a todo list: map, plan, grill, edit, verify, review,
  share as one ticket and one PR, then follow it to release.
  Use this skill when a prompt attaches it to one slice of work.
metadata:
  profile_slice_kind: implement
  omitted_steps:
    - update-task-file
    - pi-job-feedback
---

# Implement one slice

This session owns one slice.
The prompt states the goal.
Do not start a second slice.

## Todos

When the session has no todos, create the list below with the todo tool.
Use the step key as the todo id.
Use the step title as the todo content.
Set the first todo to `in_progress` and the others to `pending`.
When a todo with a step key already exists, keep it.

Work in step order.
Keep one todo `in_progress` at a time.
When you complete a todo, add one line of evidence to its content.
Cancel a todo only for the skip reason in its step.
Write that reason into the todo content.

When a step returns you to an earlier step, set that earlier todo to `in_progress` again.

## Plan file

The plan file is the contract for the current increment.
When the prompt names a plan file, use it.
Otherwise write `.plans/<slice>.md` at the root of the repo you edit.
Add `.plans/` to `.git/info/exclude`.
Do not commit the plan file.
Keep one plan file for the slice.
Revise it in place for each increment.

## Links

Each skill link is relative to this file.
When you read this file from a URL, resolve each link against that URL.
When a step names a skill, open that skill and follow it in full.
When this file and that skill disagree, the skill wins.
Do not write a ticket, a PR body, or a deck from this file.

## Subagents

You choose whether a step runs in a subagent.

## Steps

1. `map-current-state` - Map how it works now

   Read the code, manifests, APIs, and docs that the slice touches.
   Write **How it works now** in the plan file with four facts:
   current behaviour, who depends on it, repos searched, and repos not searched, with a date.
   Each arrow in a flow is a real call or trigger, not a time order.
   Record an evidence gap instead of a guess.
   When e2e cases exist, or a thin case is cheap, run them against current behaviour.
   Record pass or fail.
   Give the user a short reading order before you plan.

2. `create-plan` - Write the plan file

   Write these sections:
   - **How it works now** from step 1.
   - **How it should work**: one indented call stack for this increment, for example `handler -> service -> store`.
   - **Assertion**: one failing test, or one behaviour a person can check in seconds.
   - **Must-not**: the boundaries this increment does not cross.
   - **Verification**: the exact command that proves this increment.
   - **Acceptance**: step 4 fills it.

   Do not restate the slice goal.
   Do not add sections for brief, intent, touch surface, or open questions.
   Plan only the current increment, not the full slice.

3. `grill-plan` - Grill the plan file

   Follow [grill-me](../grill-me/SKILL.md).
   Challenge each section of the plan file.
   This step stays open while any of the four facts in **How it works now** is a placeholder.
   Revise the plan file and grill again until it survives.
   Skip only when the user declines the grill.

4. `plan-acceptance` - Plan the acceptance checks

   Ask the user for the target env: dev or staging.
   Wait for the answer.
   List acceptance cases in product language under **Acceptance**.
   For each case, name the affected users and the error symptom.
   These cases run on a deployed env after merge.
   The repo test suite is pre-merge verification, not acceptance.
   Skip only for a trivial single-file edit.

5. `edit-code` - Edit the code

   Before the first edit, follow the section "Share decisions early" in [share-with-team](../share-with-team/SKILL.md).
   Ask, and wait for the answer.
   When the user says yes, follow [decision-review-deck](../decision-review-deck/SKILL.md).
   Before the first edit, read [volod-style](../volod-style/SKILL.md) and write to that bar.

   When **How it works now** lacks one of its four facts, fill it and go back to `grill-plan`.
   When the assertion does not exist yet, write it first.
   Change only what the current increment needs.
   Stop when the **Verification** command passes.
   When the slice goal has more work, revise the plan file for the next increment.
   Then go back to `grill-plan`.
   Use a separate worktree only when you edit a repo other than the current one.

6. `verify` - Verify

   Run the **Verification** command.
   Keep the exact compiler, type-check, or test output.
   Do not keep a summary instead.
   When no command can prove the increment, record that gap.

7. `code-review` - Review the finished diff

   Review the diff after `verify`, as a separate pass.
   Following volod-style while you code does not satisfy this step.
   Follow the review section of [volod-style](../volod-style/SKILL.md) on `git diff origin/main...HEAD`.
   When the diff adds or changes tests, follow the authoring gate in [test-audit](../test-audit/SKILL.md).
   When you delete or merge a test, name the test that remains.
   Prove that the remaining test fails for the same regression:
   snapshot the file, mutate the production line, run the test, then restore from the snapshot.
   Confirm that `git status --short` shows only the intended changes.
   Apply every recommendation.
   A severity label does not permit a skip.
   When a recommendation conflicts with the plan file, stop and ask the user.
   Run `verify` again after the edits.
   Skip only for a trivial single-file edit.

8. `vulnerability-scan` - Scan for vulnerabilities

   Ask the user whether this slice needs a vulnerability scan.
   Wait for the answer.
   When the user declines, cancel this todo with that reason.
   When the user accepts, review the changed code for vulnerabilities, insecure defaults, authorization gaps, injection, secrets, and unsafe trust boundaries.
   Resolve each finding, or record the risk the user accepts, before `share-with-team`.

9. `share-with-team` - Open the ticket and the PR

   Follow [share-with-team](../share-with-team/SKILL.md) in full.
   It owns the branch name, the commit message, the ticket, and the PR body.
   Write one ticket and one PR for each repo you changed.
   When the user gives a ticket key, use that ticket.
   Do not create GitHub Issues.
   After the first push of a new branch, run `gh pr create --head <branch>`.
   Do not open a PR before this step.

10. `wait-for-feedback` - Follow the PR until it merges

    This todo stays `in_progress` until the PR merges or the user abandons it.
    For each check, run `git fetch` and `gh pr view`.
    Classify the state in one line: nit, contract, extract, merged, or abandoned.
    - **nit**: apply the change, verify, push, and wait again.
      Do not grill a nit.
    - **contract** or **scope**: grill the change with the user first.
      Then apply it, verify, push, and wait again.
    - **extract**: a reviewer asks to split the PR.
      Grill the split with the user, and capture the new ticket title and parent epic.
      A split is a new slice.
      Stop and ask the user how to run it.
    - **merged**: write the merge commit into the todo content.
    - **abandoned**: cancel the remaining todos with that reason.

    Before each push, run `git fetch`.
    Never force push.
    When the remote branch moved, run `git pull --no-rebase`, then verify again.
    When the PR is approved, do not push cleanup commits to it.
    Put them in a follow-up PR, unless the user decides otherwise.
    When this session cannot wait for the review, stop.
    Tell the user the PR URL and the current class.

11. `e2e-evidence-publish` - Run acceptance after merge

    Ask the user which env runs the merged code before rollout: dev, local, or CI.
    Wait for the answer.
    Run each **Acceptance** case on that env.
    Record pass or fail.
    Capture a repeatable artifact for each case, or record the gap.
    Repo unit or integration suites do not count as e2e evidence.
    Write the report in the format in [qa-report](references/qa-report.md).
    Ask the user before you post the report as a ticket comment.
    When a case is red, do not move the ticket to Ready for Release, unless the gap names the blocker.
    Skip only when this slice needs no e2e evidence.
    Write why.

12. `ready-for-release` - Move the ticket to Ready for Release

    Move the ticket to Ready for Release.
    Add a short testing note to the ticket when it helps.
    Skip when the slice has no ticket, or the ticket is already Ready for Release or Done.

13. `wait-for-deploy` - Wait for the deploy

    Ask the user which env is the real target, unless **Acceptance** names it.
    Watch until the merged change reaches that env.
    Do not comment, review, or change code in this step.
    When the user abandons the deploy, or it has no date, cancel this todo with that reason.
    Write the same reason on the ticket.
    Skip when the slice has no deploy step separate from the merge.

14. `e2e-evidence-deploy` - Run acceptance after deploy

    Run the same cases as `e2e-evidence-publish`, or more, on the target env.
    Write a new run in the format in [qa-report](references/qa-report.md).
    A red case here is a live incident, because the release shipped.
    Record the blocker in the report.
    Tell the team, as [share-with-team](../share-with-team/SKILL.md) describes.
    Do not complete this todo until the blocker is fixed, or the user accepts it as a tracked follow-up.
    Skip only when this slice needs no e2e evidence.
    Write why.

## Maintenance

The step keys follow `slice_kinds.implement.step_template` in `dot_local/share/pi-job-harness/profile.yaml`.
`metadata.omitted_steps` lists the profile steps that this skill leaves out.
A harness test fails when the step keys and the profile do not match.
When you change an implement step in the profile, update this file in the same change.
