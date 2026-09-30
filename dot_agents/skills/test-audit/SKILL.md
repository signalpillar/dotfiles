---
name: test-audit
description: "Invoke whenever writing, changing, reviewing, or sweeping tests. Authoring gate for new tests plus audit workflow for low-value, implementation-coupled, or duplicative tests and the test-only production seams they demand."
license: MIT (upstream)
metadata:
  source: https://github.com/openclaw/openclaw/blob/main/.agents/skills/test-audit/SKILL.md
  source-commit: 80930af448ebabc84174146b56bc106d37fab3b4
  upstream-copyright: Copyright (c) 2026 OpenClaw Foundation
---

# Test Audit

Borrowed from OpenClaw.
Original: <https://github.com/openclaw/openclaw/blob/main/.agents/skills/test-audit/SKILL.md>
Upstream license: MIT, Copyright (c) 2026 OpenClaw Foundation.
Local changes: remove OpenClaw-only tooling (`$openclaw-testing`, `$crabbox`, `$autoreview`, `scripts/*`, `extensions/`).
Local changes: reword to ASD-STE100 style, one sentence per line.
The upstream rules and their order stay the same.
Compare with upstream before you trust a rule that looks odd.

Three modes, one value bar.

- Authoring mode gates every new or changed test at write time.
- Audit mode runs focused sweeps of tests that re-assert source, duplicate stronger proof, couple behavior to implementation, or keep test-only production seams alive.
- Campaign mode prunes the whole test surface of one subsystem.

Continue broad audits as separate coherent follow-up PRs.
Optimize for confidence, not deletion count.
Before you start campaign mode, read [CAMPAIGN.md](CAMPAIGN.md).

## Authoring gate

Before you add any test, answer four questions.
If an answer is missing, do not add the test yet.

1. What observable behavior, invariant, or independent contract does it protect?
2. What credible regression makes it fail?
3. Why does existing coverage not already catch that failure?
   - Each contract has one primary test owner at the strongest boundary.
   - Another layer needs its own distinct risk, such as a transport or lifecycle failure the owner cannot reach.
   - Prefer a table-driven case or a shared fixture over a near-duplicate test.
   - Consolidate duplicated setup in the same change.
4. Does it need a production seam (export, flag, wrapper, injection hook) that no production caller needs?
   If yes, move the test to the real boundary instead.

Then check the test against every [junk pattern](#junk-patterns).
A match fails the gate unless the [retention bar](#retention-bar) names the contract it independently guards.
A test that breaks under a behavior-preserving refactor asserts implementation, not behavior.
Rewrite it at the owning boundary before it lands.

A bug regression test must fail on the pre-fix code for the intended reason.
It must pass after the owner-boundary repair.
A regression test that never demonstrably failed proves the mock, not the fix.
One regression at the owner boundary covers the bug.
Do not replay the same scenario at every layer it crosses.

## Junk patterns

Both modes use this checklist.
The authoring gate rejects a new test that matches one.
Audits hunt for existing tests that match one.

- Assertion-free coverage probes.
- Self-comparisons and identity copiers.
- Copied fixtures, inventories, manifests, or export lists.
- Exact source, import, or string greps.
- Private predicate or call-shape tests duplicated at real boundaries.
- Duplicate invocations of the same contract.
- Provider-local replays of shared helpers.
- Tests whose only purpose is to preserve test-only exports, globals, or wrappers.
- Dead production code whose only callers are tests.
- Expected values produced by the helper or renderer under test.
- Mocks that implement the asserted behavior, or one identical mock that stands in for different APIs.
- Fixtures that supply the receipt, admission, or callback ordering the owner must produce.
- Persistence asserted against a store the path never writes.
- Capability tests that restate declared flags instead of exercising the delivery or acknowledgement the flag promises.
- Negative controls that pass for an unrelated reason, such as a denial from a different guard or a rejection the production path never reaches.
- Names or fixtures that promise more than the input exercises, such as a "retires the window" test that asserts the window was not cleared.

## Value bar

A test justifies its maintenance cost when it protects behavior, a credible regression, or an independently meaningful contract.

In an audit, an existing test that must change for a behavior-preserving source reorganization is suspect.
It is not automatically deletable.
The authoring gate still rejects new ones.

Before you judge a candidate, read all of these:

- the complete test;
- the production owner, its entry point, callers, and callees;
- sibling implementations and overlapping tests;
- CI routing and relevant history.

Read the root and scoped `AGENTS.md` files first.
When the test claims dependency-backed behavior, inspect the dependency source or types directly.

## Discovery

Keep discovery read-only.
Report evidence before you edit.
For a broad scope, run parallel discovery lanes when available.
Split the lanes by area of the repository, plus one cross-cutting pattern sweep.

Outside campaign mode, prefer a few high-confidence candidates over a large speculative inventory.
Hunt for the [junk patterns](#junk-patterns).

## Retention bar

Keep a test when it independently enforces one of these contracts:

- public API, SDK, or protocol;
- config, migration, or storage;
- security or platform;
- defaults or prompt bytes;
- generated cross-language output;
- package, release, or architecture rules.

Also keep:

- call ordering when order is observable behavior;
- regressions with a credible failure mode;
- source inspection when it is the cheapest independent guard.
  It must fail when the contract changes (the user-facing key, byte, or path).
  It must survive an identifier-only refactor.
- a retained test that fails on the baseline.
  Treat it as a possible product bug.
  Reproduce it, then repair the owner.
  Do not delete it.

Static or slow is not a deletion reason.
A test that resembles implementation may still be the independent contract.
Prove otherwise before you remove it.

## Candidate evidence

Record every field before you edit.
If a field is missing, the candidate is not ready for deletion.

- Exact test name and location.
- What failure it can actually detect.
- Non-test callers of the covered production or support seam.
- Stronger remaining owner-boundary proof, or why no proof is needed.
- Relevant history and the reason the test or seam exists.
- Production or test-support deletion it unlocks.
- Risk and the focused validation command.

## Edit shape

Choose one coherent owner-boundary batch.
Delete obsolete test-only exports, globals, wrappers, and dead production paths.
Do not keep aliases.
Move retained regressions to their canonical owners.
Consolidate repeated package or dependency assertions into one generic contract.

Prefer net-negative production LOC.
Do not add replacement tests that restate the same implementation.
Do not turn an uncertain candidate into cleanup to raise the deletion count.

## Validation

Never edit source or tests while the test runner runs in the checkout.

1. Run the smallest owner and sibling tests with the repository test command.
2. For a removed source grep or plan assertion, run the executable script or dry-run that owns the real contract.
3. Run the targeted formatter, then `git diff --check`.
4. Run the changed-files gate that the repository policy requires.
5. Inspect `git diff --numstat`.
   Report production and tooling separately from tests and test support.
6. After the final audit edits, run one distinct review pass on the diff.

## Landing and continuation

Commit, push, open a PR, or land only when the user authorizes it.
Follow the repository PR flow.
Land one coherent PR at a time.
After landing, refresh from current `main`.
Rerun read-only discovery for the next high-confidence batch.

## Handoff

Report:

- root cause and removed low-value categories;
- production owner simplifications;
- retained false positives and why they remain valuable;
- focused and full proof actually run;
- production versus test LOC;
- PR and merge state;
- named follow-ups.
