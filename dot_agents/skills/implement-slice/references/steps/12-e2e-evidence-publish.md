---
key: e2e-evidence-publish
order: 12
title: "Provide e2e/acceptance evidence after publish, or record the gap"
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

# Provide e2e/acceptance evidence after publish, or record the gap

Read this file when the `e2e-evidence-publish` todo is in progress.
Follow this file.
Then mark that todo completed, or cancel it with the skip reason.

This step needs a user decision.
Stop and ask.
Do not decide it alone.

## Skip

skip when e2e/acceptance evidence is not needed for this slice (record why)

Cancel the todo only for this reason.
Write the reason into the todo content.

## Guidance

Right after the PR merges (wait-for-feedback), before this slice moves
to Ready for Release - checkpoint 2 of capture-by-test. Answers "does
the PR work", not "does the release work"; e2e-evidence-deploy (after
wait-for-deploy) answers that one.
Run the ## Acceptance use cases. Read plan file ## Acceptance when
plan-acceptance wrote it.
Ask the user for the env to run against: dev, local, or CI - whatever
reflects the merged code before rollout to the real target env.
Run each case on that env and record pass or fail.
Capture the repeatable artifact or record an explicit gap.
When toolbelt test-user-registry is registered, reuse its personas
instead of inventing accounts per case; update reused_by and
last_verified for every user id exercised.
Repo unit or integration suites do not count as e2e evidence.
Name them as pre-merge verification, not e2e.
Write the report to references/working/<slice-key>-e2e-run<N>.md.
Keep fixture builders and runner scripts next to the report.
Use the report format below; it follows the FCD-189 QA evidence comments.
Number each run. For a re-run, write a new report; do not edit an earlier run.
A re-run report states what changed, re-verifies the affected cases, and names the cases it does not affect.
Ask the user before you post the report as a Jira ticket comment.
Mirror ticket key, env, verdict, and comment URL in finish --note.
Update plan file ## Acceptance with results or gap.
Prefer ticket comments and finish --note over add-decision.
On red E2E, do not move the ticket to Ready for Release unless the gap note names the blocker.

Report format - every prose section is short bullets, one fact per
bullet, no restating the table, no hedging, no throat-clearing:

## QA evidence for <repo>#<PR>, run <N> (<date>)

**Verdict: <ready for release | NOT ready for release> <conditions>.** One sentence why.
Full evidence, fixture builder and runners: `<path>`.

**Setup.**
- Merge commit or deployed build `<sha>`; how it runs; the env it talks to (real reads/writes).
- What is stubbed, what is real.
- Config values and IDs the cases depend on.
- Fixture records, named so a reader can find them in the env.
- Any real (organic) record the fixtures copy or replay.

### Blocker - <one-line title>      (one section per blocker or finding; omit when none)
- Cause, with links.
- Repro: case ID, request, exact status and error.
- Impact on real users.
- Options (a)/(b)/(c) and who decides.

### <Surface> - `<METHOD> <path>` (<where it ran>)
| # | Scenario | Result | Pass |
| --- | --- | --- | --- |
| <P1> | <inputs that matter> | <HTTP status; key values in bold; side effects, e.g. resource version unchanged> | ✅ / ✅ as designed / ✅ guard / ✅ fail-safe / ❌ **Blocker** |
Use one table per surface and a stable ID prefix per surface.
Cover the happy paths, each boundary value, each retry, each error guard, and each fallback.
When a case needs extra columns (for example the same case on two config versions), add them.
After the table, one line on the persisted shape and any mixed-version or rollout compatibility check.

### Unit tests and merge
One line: suites, counts passed/failed/pending, why each pending test exists, merges from main since the last run.

### Dependencies / notes
- Linked PRs and their state.
- Stale ticket text, if any.
- QA housekeeping: env side effects, fixture cleanup.

### Notes for release verification
- Checks to run after release (log lines, records to inspect).
- Env-specific risks.
- What this run could not simulate.
