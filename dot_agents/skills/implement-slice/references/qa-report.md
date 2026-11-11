# QA report format

Use this format for `e2e-evidence-publish` and `e2e-evidence-deploy`.
Write every prose section as short bullets, one fact per bullet.
Do not restate the table.
Do not hedge.

Number each run.
For a re-run, write a new report.
Do not edit an earlier run.
A re-run report states what changed, re-verifies the affected cases, and names the cases it does not affect.

````
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
````

Use one table per surface and a stable ID prefix per surface.
Cover the happy paths, each boundary value, each retry, each error guard, and each fallback.
When a case needs extra columns, add them.
An example is the same case on two config versions.
After the table, write one line on the persisted shape and any mixed-version or rollout check.
