---
key: plan-acceptance
order: 4
title: "Plan acceptance and post-merge checks"
owner: "orchestrator"
requires_user_decision: true
skip_rule: "skip only for a trivial single-file edit (record a one-line reason)"
record_channels:
  - "step_note"
  - "plan_file"
validators:
  - "acceptance-recorded-or-skip-reason"
  - "user-decision-recorded"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Plan acceptance and post-merge checks

Read this file when the `plan-acceptance` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

This step needs a user decision.
Stop and ask.
Do not decide it alone.

## Skip

skip only for a trivial single-file edit (record a one-line reason)

Cancel the todo only for this reason.
Write the reason into the todo content.

## Guidance

Define post-merge checks before edit-code.
Read the slice plan and the test-case-table aid.
Ask the user for the target env: dev or staging.
List acceptance use cases in product language.
Run each case against a deployed env, never the repo unit suite.
The repo suite owns pre-merge verification, not acceptance.
Name affected users and error symptoms per case.
Write results under plan file ## Acceptance.
Keep the step note short: env plus plan file pointer.
Revise the same plan file when scope changes.
