---
key: create-plan
order: 2
title: "Create plan"
owner: "subagent"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "plan_file"
validators:
  - "plan-recorded"
  - "create-plan-and-grill-plan-done-or-skip-reason-before-other-steps"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Create plan

Read this file when the `create-plan` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Guidance

Before other implement/spike steps, lead with create-plan then grill-plan:
(1) create-plan - sibling Markdown plan file as the current micro-contract
    (not a how-to essay, delivery journal, or full-slice design).
    One file per slice. Never create a second plan file for the same slice.
    Revise the same path in place for the next increment.
    Required sections: how it works now, how it should work, assertion, must-not, verification, acceptance.
    How it works now: current behaviour, who depends on this bit, repos searched, repos not searched.
    When the plan already states those four facts, keep that text.
    When any fact is missing, write it under How it works now before edit-code.
    How it should work: one indented stack for this increment (e.g. handler -> service -> store).
    Prefer a shallow stack over a placeholder.
    Assertion: one failing test or one behaviour bullet a human can check in seconds.
    Must-not: the boundaries this increment must not cross.
    Verification: the exact command that proves this increment.
    Do not add Brief, Intent, System behaviour, Touch surface, or Open questions.
    Do not restate the slice goal; do not inline the plan body in the task file.
    Do not move delivery status, cursor, or session journals into plan files
    (pi-job store owns those).
    DX and agent experience share the same constructs: clear names and short contracts.
    Persist a task-goal agreement with `pi-job add-decision` only when a worker
    on a different slice needs the fact without this slice plan or code.
    Revise the same slice plan for slice-local choices (must-not / verification).
    Write wiki only for a shared operator runbook.
    Do not leave a load-bearing task-goal choice only in chat.
    When the decision body spills, pass `--slug kebab-topic` so the file is
    `_decision-<date>-<slug>.md`, not a UTC stamp.
    Step evidence belongs in `finish --note`, not `add-decision`.
    Token smell: shrink the slice or contract; do not dump more of the task store.
    Naming: `<task-stem>.plans/<slice-key>.md`. The plan path follows the layout
    convention, so create-plan writes no step note.
(2) grill-plan - grill-me skill [grill-me](../skills/grill-me/SKILL.md).
    Challenge How it works now, How it should work, the assertion, must-not, the verification command, and the Acceptance section.
    How it works now names current behaviour and each dependent.
    It names repos searched and repos not searched, with a date.
    Grill-plan stays open while any of those four facts is still a placeholder.
    Prose volume is not an acceptance criterion.
    Note what changed; revise the same plan FILE and re-grill until it survives.
    After grill survives, apply the membership test.
    On a clear pass, record one add-decision for the surviving task-goal lock
    (date/note/source/`--slug` kebab-topic when the body spills), not only in chat.
    Do not record each grill turn, "grill survived", function names, file paths,
    error strings, lib pins, or e2e harness shape.
    Fleet: on doubt, ask the manager; add-decision only after the manager or user names it.
    Solo: on doubt, ask the user.
    Do not use add-decision for PR, deploy, e2e, or progress chatter.
    When a decision supersedes an earlier one, append a new row whose note begins with
    `SUPERSEDES: YYYY-MM-DD (source) - …` pointing at the superseded entry; never delete
    or edit prior decision rows. When the supersede changes slice title or goal, run in the
    same turn: `set-slice --slice SLICE_KEY --title …` and/or
    `--goal …` (refuses done/skipped slices; spawn a follow-up slice instead).
Advance blocks past incomplete create-plan/grill-plan. Distinct from setup `grill`.
Exception: trivial single-file edit may skip both (status: skipped, one-line reason) -
same class as coding_execution.exceptions; only way to omit How it works now, How it should work, the assertion, or Acceptance.
Soft enforcement: guidance + phrase locks + seeded stub; no hard plan-file heading linter.
The worker stops before edit-code when How it works now lacks any of the four facts.
If a seed stub already exists, revise it rather than starting blank.
(3) After grill-plan survives and before edit-code, read
    [share-with-team](../skills/share-with-team/SKILL.md), section "Share decisions early".
    Follow that section.
    When it names decision-review-deck, read
    [decision-review-deck](../skills/decision-review-deck/SKILL.md) and follow that file.
    Do not write a deck from this packet.
    Do not write a deck from the share-with-team summary.
    Skip the deck only when the user declines that section's ask.
