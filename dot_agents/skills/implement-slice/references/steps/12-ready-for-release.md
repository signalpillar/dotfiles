---
key: ready-for-release
order: 12
title: "Mark the ticket Ready for Release"
owner: "orchestrator"
requires_user_decision: false
skip_rule: "skip when this slice has no Jira ticket, or the ticket is already Ready for Release / Done"
record_channels:
  - "step_note"
validators:
  - "ticket-ready-for-release-or-not-applicable"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Mark the ticket Ready for Release

Read this file when the `ready-for-release` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Skip

skip when this slice has no Jira ticket, or the ticket is already Ready for Release / Done

Cancel the todo only for this reason.
Write the reason into the todo content.

## Guidance

After wait-for-feedback (merge) and e2e-evidence-publish (or an explicit e2e skip), transition
the slice's Jira ticket to Ready for Release.
Leave a short testing/landing note on the ticket when useful.
Record the transition (or not-applicable/skip reason) in finish --note.
Do not mark the slice done until this gate is finished or skipped with reason.
