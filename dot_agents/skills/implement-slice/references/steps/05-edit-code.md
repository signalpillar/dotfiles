---
key: edit-code
order: 5
title: "Edit code"
owner: "subagent"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "step_note"
  - "repo_work"
validators:
  - "coding-policy-recorded"
  - "worktree-used-when-repo-editing"
  - "volod-style-loaded-before-first-edit"
  - "decision-review-deck-read-or-user-declined-before-first-edit"
  - "current-behaviour-and-dependents-in-plan"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Edit code

Read this file when the `edit-code` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Skills

- Open [share-with-team](../skills/share-with-team/SKILL.md) and follow it.
  It owns the early decision-share ask.
  Read it before any edit.
- Open [decision-review-deck](../skills/decision-review-deck/SKILL.md) and follow it.
  It owns the decision deck.
  Read it before the first edit.
- Open [volod-style](../skills/volod-style/SKILL.md) and follow it.
  It owns the coding bar.
  Read it before the first edit.

## Guidance

Before any edit, read [share-with-team](../skills/share-with-team/SKILL.md) section "Share decisions early" and follow it.
When it names decision-review-deck, read [decision-review-deck](../skills/decision-review-deck/SKILL.md) and follow that file.
Do not write a deck from this packet.
Skip the deck only when the user declines that ask.
Read How it works now before the first edit.
When that section names current behaviour, each dependent, repos searched, and repos not searched, start the edit.
When any of those four facts is missing, fill the same plan file and return to grill-plan.
Implement only the current micro-contract in this slice's plan file.
Do not implement the rest of the slice goal in this step.
If the assertion is missing, write that artifact first and stop.
Use a worktree when editing a repo other than the current one.
Before the first edit, read [volod-style](../skills/volod-style/SKILL.md) and follow it.
Write to that bar. Following it while coding does not replace the later
distinct code-review pass.
Stop when the plan Verification command is green.
When the slice goal still has work, replace How it should work in the same plan file.
Revise assertion, must-not, and verification in that same file.
Keep How it works now current when the increment changes that picture.
Do not create a second plan file.
