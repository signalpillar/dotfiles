---
key: code-review
order: 7
title: "Run a distinct volod-style review"
owner: "orchestrator"
requires_user_decision: false
skip_rule: "skip only for a trivial single-file edit (record a one-line reason)"
record_channels:
  - "step_note"
validators:
  - "review-is-a-separate-pass-not-style-applied-while-coding"
  - "reviewer-model-differs-from-code-author-model"
  - "every-recommendation-addressed-in-the-worktree"
  - "test-audit-gate-passed-when-tests-changed"
different_model_from_step: "edit-code"
compiled_from: dot_local/share/pi-job-harness/profile.yaml
slice_kind: implement
compiled_on: 2026-11-11
---

# Run a distinct volod-style review

Read this file when the `code-review` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

## Skip

skip only for a trivial single-file edit (record a one-line reason)

Cancel the todo only for this reason.
Write the reason into the todo content.

## Model

Use a different model from the `edit-code` step.

## Skills

- Open [volod-style](../skills/volod-style/SKILL.md) and follow it.
  It owns the distinct review pass.
  Read it this pass; loading it during edit-code does not satisfy this step.
- Open [test-audit](../skills/test-audit/SKILL.md) and follow it.
  It owns the test authoring gate.
  Read it when the diff adds or changes tests.

## Guidance

Run one distinct final review of the actual diff with the volod-style skill
[volod-style](../skills/volod-style/SKILL.md). Applying volod-style while planning or coding
does NOT satisfy this step; this is a separate pass on the finished change.
When the diff adds or changes tests, also load and follow the test-audit skill
[test-audit](../skills/test-audit/SKILL.md) on the same diff. It is borrowed from
https://github.com/openclaw/openclaw/blob/main/.agents/skills/test-audit/SKILL.md.
- Read the model recorded on edit-code.
- When a higher-reasoning model exists than that author model, dispatch
  the review to a subagent on it.
- When the author model is already top-tier, pick a different capable reviewer.
- Do not review in the same session or with the fast coding model.
- Ignore lower_power_model_preferred for this review pass.
- Record the actual reviewer model on start/finish.
- Do not record the orchestrator default when a higher-model subagent ran
  the review.
- Check volod-style concerns: service boundaries, explicit contracts and type
  strictness, fail-closed safety, decision/edge-case comments, kebab-case
  logging, and test quality (test.each, fixture reuse).
- When tests changed, run the test-audit authoring gate on each new or changed test.
  Check each one against the junk patterns.
  Remove every test that fails the gate, or name the contract that keeps it.
- When the audit deletes or merges a test, scope it with `git diff origin/main...HEAD` (three dots).
  Count only the lines this change introduced.
- For each deleted or merged test, name the surviving test.
  Prove the survivor fails for the same regression.
  Mutate the production line, run the survivor, and restore the line.
  Snapshot the file first and restore from the snapshot.
  Then confirm `git status --short` shows only the intended changes.
- Read the twin test or table row before you call a test a duplicate.
- When a test still passes without the production line it claims to guard, it guards nothing.
  Delete the test with the line, or keep it knowingly and record why.
- After the audit edits, run one distinct review pass on the diff.
- Apply every review recommendation and every simplification in the worktree before finish.
- A severity label does not permit a skip.
- Record each item and its worktree change in finish --note.
- When no simplification applies, record that fact.
- Re-verify after those edits.
- When a recommendation conflicts with a locked plan decision, stop and ask.
