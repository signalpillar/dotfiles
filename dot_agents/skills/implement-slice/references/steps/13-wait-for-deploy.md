---
key: wait-for-deploy
order: 13
title: "Wait for deploy to the target environment"
owner: "orchestrator"
requires_user_decision: true
skip_rule: "skip when this slice has no separate deploy step from publish (record why)"
record_channels:
  - "step_note"
validators:
  - "landed-or-abandoned-with-reason"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Wait for deploy to the target environment

Read this file when the `wait-for-deploy` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

This step needs a user decision.
Stop and ask.
Do not decide it alone.

## Skip

skip when this slice has no separate deploy step from publish (record why)

Cancel the todo only for this reason.
Write the reason into the todo content.

## Guidance

Checkpoint 3 of capture-by-test starts here: observe until the merged
change actually rolls out to the target environment named in plan file
## Acceptance, or ask the user which env is the real target for this
slice.
No engagement beyond observing: no comments, reviews, or code changes.
On abandon or indefinite delay, skip with reason; note it on the ticket.
