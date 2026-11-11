---
key: e2e-evidence-deploy
order: 15
title: "Provide e2e/acceptance evidence after deploy, or record the gap"
owner: "orchestrator"
requires_user_decision: true
skip_rule: "skip when e2e/acceptance evidence is not needed for this slice (record why)"
record_channels:
  - "step_note"
  - "plan_file"
validators:
  - "evidence-or-gap-recorded"
  - "user-decision-recorded"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Provide e2e/acceptance evidence after deploy, or record the gap

Read this file when the `e2e-evidence-deploy` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

This step needs a user decision.
Stop and ask.
Do not decide it alone.

## Skip

skip when e2e/acceptance evidence is not needed for this slice (record why)

Cancel the todo only for this reason.
Write the reason into the todo content.

## Skills

- Open [share-with-team](../skills/share-with-team/SKILL.md) and follow it.
  It owns the team notice when this live check is red.

## Guidance

After wait-for-deploy confirms the rollout - the final live-health
check now that the release has actually shipped.
Read plan file ## Acceptance when plan-acceptance wrote it; run the
same cases e2e-evidence-publish ran (same or more), against the actual
target env this time (dev or staging - whichever wait-for-deploy
confirmed).
Record pass or fail; capture the repeatable artifact or record an
explicit gap.
Use the same report format, path convention, and Jira-comment
discipline as e2e-evidence-publish - do not duplicate the template here.
When toolbelt test-user-registry is registered, reuse its personas
instead of inventing accounts per case; update reused_by and
last_verified for every user id exercised.
On red here, the release has already shipped: this is a live incident,
not a release gate. Record the blocker in the report, notify the team
per [share-with-team](../skills/share-with-team/SKILL.md), and do not finish this step done until the
blocker is resolved or explicitly accepted as a tracked follow-up.
