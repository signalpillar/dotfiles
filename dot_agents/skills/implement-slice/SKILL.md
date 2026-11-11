---
name: implement-slice
description: >-
  Run one implement slice as a TODO list.
  Use this skill only when the prompt attaches it for one slice.
  The session plans, edits, reviews, and opens one ticket and one PR.
  Create one todo per step in this file.
  Open the named step file and the named template files.
---

# Implement one slice

This session owns one slice.
The prompt states the goal.
Do not add another slice.
Do not run setup work.
Do not run closing work.

The files under `references/` are compiled from the shipped profile.
Follow those files.
Do not rewrite them during the slice.

## First action

When the session has no todos, create the list below with the todo tool.
When a todo id already matches a step key, keep that todo.
Set `map-current-state` to `in_progress` when you create the list.
Set every other new todo to `pending`.
Use the step key as the todo id.
Use the sentence block as the todo content.

Work in step-number order.
When a todo is in progress, read its step file and follow that file.
Mark the todo `completed` only after that work is done.
Cancel a todo only when the step file states a skip rule.
Write the skip reason into the todo content.
Do not skip a todo in silence.
Do not start the next todo while the current todo is `in_progress`.

1. `map-current-state`

   Map how it works now.
   Record current behaviour, dependents, repos searched, and repos not searched.
   Read `references/steps/01-map-current-state.md`.

2. `create-plan`

   Write one plan file for this increment.
   The step file lists the required sections.
   The step file links [grill-me](references/skills/grill-me/SKILL.md), [share-with-team](references/skills/share-with-team/SKILL.md), and [decision-review-deck](references/skills/decision-review-deck/SKILL.md).
   Read `references/steps/02-create-plan.md`.

3. `grill-plan`

   Grill the plan file with [grill-me](references/skills/grill-me/SKILL.md).
   Ask one question at a time.
   The step file also links [share-with-team](references/skills/share-with-team/SKILL.md) and [decision-review-deck](references/skills/decision-review-deck/SKILL.md).
   Read `references/steps/03-grill-plan.md`.

4. `plan-acceptance`

   Ask the user for the target env.
   Write the acceptance cases in the plan file.
   Read `references/steps/04-plan-acceptance.md`.

5. `edit-code`

   Before the first edit, follow [share-with-team](references/skills/share-with-team/SKILL.md).
   When the user accepts, follow [decision-review-deck](references/skills/decision-review-deck/SKILL.md).
   Follow [volod-style](references/skills/volod-style/SKILL.md) before the first edit.
   Edit only the current micro-contract in a worktree.
   Read `references/steps/05-edit-code.md`.

6. `verify`

   Run the Verification command from the plan file.
   Keep the raw tool output.
   Read `references/steps/06-verify.md`.

7. `code-review`

   Review the diff in a separate pass.
   Follow [volod-style](references/skills/volod-style/SKILL.md) on that diff.
   When tests changed, follow [test-audit](references/skills/test-audit/SKILL.md).
   Read `references/steps/07-code-review.md`.

8. `vulnerability-scan`

   Ask whether this slice needs a vulnerability scan.
   On a decline, cancel this todo and record the reason.
   Read `references/steps/08-vulnerability-scan.md`.

9. `share-with-team`

   Open [share-with-team](references/skills/share-with-team/SKILL.md) and follow it.
   Write the ticket from its ticket template.
   Write the PR from its hybrid template.
   Read `references/steps/09-share-with-team.md`.

10. `update-task-file`

    Update the task record with what changed.
    Read `references/steps/10-update-task-file.md`.

11. `wait-for-feedback`

    Stay on this todo until the PR merges or you abandon it.
    Classify each review before you edit.
    Read `references/steps/11-wait-for-feedback.md`.

12. `e2e-evidence-publish`

    After the PR merges, run the acceptance cases.
    Write the QA report from the step file.
    Read `references/steps/12-e2e-evidence-publish.md`.

13. `ready-for-release`

    Move the Jira ticket to Ready for Release.
    Cancel this todo when the slice has no ticket.
    Read `references/steps/13-ready-for-release.md`.

14. `wait-for-deploy`

    Wait until the change reaches the target env.
    Do not edit code on this step.
    Read `references/steps/14-wait-for-deploy.md`.

15. `e2e-evidence-deploy`

    Run the same acceptance cases on the target env.
    A red result here is a live incident.
    Notify the team with [share-with-team](references/skills/share-with-team/SKILL.md).
    Read `references/steps/15-e2e-evidence-deploy.md`.

## Templates

A step file links each skill it names.
Open that link and follow the file.
The ticket template and the hybrid PR template live in [share-with-team](references/skills/share-with-team/SKILL.md).
Do not write a ticket or a PR body from memory.
Do not shorten those templates.
The decision deck lives in [decision-review-deck](references/skills/decision-review-deck/SKILL.md).
Do not write a deck from the share-with-team summary.

## One plan file

When the prompt gives a plan path, use that file.
When the prompt gives no plan path, write `plans/<slice>.md` in the repo you edit.
Revise that same file for every later increment.
Do not create a second plan file.
Write each recorded fact in that plan file.

## Edit execution

Run the edit-code work in a subagent.
Prefer a lower-power model for that subagent.
Review the result in this session.
Keep a trivial single-file edit in this session.
When the user asks this session to edit, edit here.

## Source

This copy was compiled by hand on 2026-11-11.
The source is `dot_local/share/pi-job-harness/profile.yaml`.
The slice kind is `implement`.
The step order follows `slice_kinds.implement.step_template`.
The feedback step is omitted.
Each step file copies that step's guidance.
Each named skill is copied under `references/skills/`.
Each skill mention in a step is a markdown link to that file.
A home path `~/.agents/skills/<name>/SKILL.md` is rewritten into that link.
Regenerate `references/` when `profile.yaml` changes.
Do not hand-edit `references/` after the compiler exists.
