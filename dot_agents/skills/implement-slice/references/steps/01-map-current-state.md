---
key: map-current-state
order: 1
title: "Map how it works now"
owner: "orchestrator"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "step_note"
  - "slice_note"
validators:
  - "current-state-aids-produced-or-gap-recorded"
  - "cross-layer-spine-completed-or-not-applicable"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Map how it works now

Read this file when the `map-current-state` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Guidance

Build the selected understanding aids before asking the user implementation or scope questions. Investigate facts in code, manifests, APIs, and trusted documents; use parallel subagents when useful. When task.layers is non-empty, always replace the bigpicture stub with an AS-IS causal spine. Cover each primary journey across every confirmed layer; mark an idle layer explicitly when it has no hop. Each arrow must mean a real call or trigger, not only chronology. Keep proposed TO-BE behaviour out, except for short and clearly labelled gaps. Build other selected understanding aids now, such as vocabulary, lifecycles, data shapes, and endpoint maps. Follow each catalog aid's example when present. Mark each completed aid done with an AS-IS note; leave planning-only aids planned. Record evidence gaps instead of inventing behaviour. Before finishing, give the user a short reading order and the facts that will ground clarify-scope and grill. In an implement slice, when e2e test cases already exist, or writing a thin one is cheap, run them against current behaviour now and record pass/fail here - a live run beats a narrative guess. When toolbelt test-user-registry is registered, reuse its personas instead of inventing accounts; update reused_by and last_verified for every user id exercised. When no cases exist yet and writing them before code is not worth it, record that explicit gap in the note instead; this choice does not change whether the rest of this step can be skipped.
