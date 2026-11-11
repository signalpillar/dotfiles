---
key: share-with-team
order: 9
title: "Share with team: ticket + PR for this slice's repo"
owner: "orchestrator"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "step_note"
  - "pull_request"
validators:
  - "share-with-team-skill-read-and-followed"
  - "ticket-and-pr-body-taken-from-that-skill"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Share with team: ticket + PR for this slice's repo

Read this file when the `share-with-team` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Skills

- Open [share-with-team](../skills/share-with-team/SKILL.md) and follow it.
  It owns the PR body, the branch name, and the ticket.
  Read it before writing the ticket or the PR body.
  Do not open a PR before this step.

## Artifacts

- `ticket`: one ticket from the share-with-team skill
- `pull_request`: one PR, body from the share-with-team skill

## Guidance

The share-with-team skill owns the ticket and the PR body
[share-with-team](../skills/share-with-team/SKILL.md).
Read that file and follow it.
Do not restate, shorten, or override it here.
Do not write the ticket or the PR body from this packet.
When that file names another skill, open that skill's link and follow it.
After opening the PR, record it:
  add-pr --slice SLICE_KEY --repo REPO --url URL --status open
Satisfy the ticket gate by recording an existing ticket key in `finish --note`.
There is no `add-ticket` command. Do not create GitHub Issues.
Prefer `gh pr create --head <branch>` right after the first push of a new branch
(avoids the "must first push" abort).
