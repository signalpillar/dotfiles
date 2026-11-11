---
key: wait-for-feedback
order: 10
title: "Wait for team feedback: loop on PR review until merged"
owner: "orchestrator"
requires_user_decision: false
skip_rule: ""
record_channels:
  - "step_note"
  - "pull_request"
validators:
  - "feedback-window-recorded"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Wait for team feedback: loop on PR review until merged

Read this file when the `wait-for-feedback` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Next action

Do not finish this step unless the class is merged or abandoned.
1. `start [--slice-only if the slice has no execution metadata] --owner {owner} --model cursor/grok-4.6`
   The model is an attribution hint only. It does not route reviews or debugging.
2. Live-check `sync`, then `gh pr view`.
3. Classify. State the class in one line. Follow Guidance for that class.
4. After nit, contract, or extract: `set-step-note` and stay on this step. Do not finish.
5. After merged or abandoned: `finish --owner {owner} --slice SLICE_KEY --step STEP_KEY --note '<evidence>'`, then `instruction --owner {owner}`.
Pause for grill on contract, scope, or extract.

## Guidance

This step stays in progress until the PR merges or you abandon it.
Re-check via `sync` (store-side) then live `gh pr view`.
State the class in one line: nit, contract, extract, merged, or abandoned.
Record class, SHA, and PR URL with `set-step-note --slice SLICE_KEY --step wait-for-feedback --note '...'`.
Put long prose in the slice plan file.

Before each push to the PR branch, run `git fetch`.
Expect the remote to have moved, because others update the branch.
Never force push.
When the remote moved, merge the remote tip with `git pull --no-rebase`.
Then re-verify before you push.
When the PR is already approved, do not push audit or cleanup commits into it.
A push can dismiss the approval.
Land those commits in a follow-up PR, unless the user decides otherwise.

When the class is nit, apply in the parent worktree.
Verify, push, then wait again.
Do not grill nits.

When the class is contract or scope, grill first.
Then apply, verify, push, and wait again.

When the class is extract, grill first.
Capture the new ticket title and parent epic in that grill.
Apply the split on this step:
Create a worktree from the default branch.
Replay the extracted hunks onto that branch.
Drop those hunks from the parent PR.
Verify both branches.
Run `add-slice --kind implement` for the extract.
Run `set-worktree` on the new slice.
`finish --skip` each step before share-with-team.
Name the parent slice and this grill in each skip reason.
Always open a new ticket on the extract slice.
When the parent cannot stay green without the extract, set parent `depends_on` extract.
The new slice starts at share-with-team, then its own wait-for-feedback.
This parent step stays in progress.

When the class is merged, record it before finish:
  add-pr --slice SLICE_KEY --repo REPO --url URL --status merged
Then finish --note with the merge evidence.

When the class is abandoned:
  finish --skip --reason with the abandon reason.
