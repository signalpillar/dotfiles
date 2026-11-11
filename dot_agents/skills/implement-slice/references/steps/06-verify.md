---
key: verify
order: 6
title: "Verify"
owner: "orchestrator"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "step_note"
validators:
  - "relevant-tests-or-gap-recorded"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Verify

Read this file when the `verify` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Guidance

Run the Verification command from this slice's plan file, or record the gap. Feed the agent the exact compiler, type-check, or test output, not a summary.
