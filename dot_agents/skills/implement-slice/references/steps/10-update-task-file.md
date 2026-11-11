---
key: update-task-file
order: 10
title: "Update this task file"
owner: "orchestrator"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "step_note"
validators:
  - "task-file-current"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Update this task file

Read this file when the `update-task-file` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Guidance

Keep the task record current via pi-job mutation commands only (add-slice, add-step, add-decision, set-plan-note, finish; see pi-job --help). Do not edit the task store directly. Write the smallest note that states what changed, or finish with no note when the mutations already speak. Create a new slice only for a product, test, or shipped-doc change. A decision is not a slice. A cross-slice fact uses add-decision with its reader slice named. Slice-local facts stay in the slice plan. Give that change a bounded goal, kind, and dependencies. Charter or workaround follow-ups (temporary coupon id, inline-not-ref, restructure hold) spawn a bounded implement or fog slice via add-slice with explicit depends_on - not an extra go-to-prod or closing checklist step. go-to-prod and closing slices stay bookkeeping. When a superseding decision changes slice title or goal, run in the same turn: `set-slice --slice SLICE_KEY --title …` and/or `--goal …`. When the target slice is done or skipped, spawn a follow-up slice instead of set-slice.
