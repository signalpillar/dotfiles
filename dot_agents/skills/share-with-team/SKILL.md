---
name: share-with-team
description: Share work with the team — early (the decisions/approach once the plan is known, before coding) and after (commit, ticket, PR). Use when a plan or approach is decided and should be reviewed before implementing, or when work is done and needs a commit / ticket / PR.
license: personal
compatibility: all
metadata:
  audience: developer
---

## Share decisions early — after the plan, before the code

- The moment the plan/approach is decided and **before implementation starts**, pause and ask:
  "Want me to share the decisions with the team first?"
- If yes, use the `decision-review-deck` skill (`../decision-review-deck/SKILL.md`) to produce a
  light, ~2-minute ASCII deck of the high-level decisions and trade-offs, so colleagues can react
  **before** code is written — a decision is cheaper to change than a diff.
- This is separate from the post-work sharing below (commit / ticket / PR), which happens once the
  work is done.

## What I do

- **First, ask**: "Do you want me to run a `volod-style` review of the staged changes first?" Point at `../volod-style/SKILL.md` and wait for an answer before doing anything else. If yes, run that review pass and surface findings before continuing; let the user decide whether to address them now or proceed as-is.
- Check which branch you are on.
- If you are on `main`/`master` (or any non-feature branch), create a feature branch BEFORE committing.
  - Use `feat/` for feature work and `bug/` for fixes.
  - Include the ticket key in the branch name.
  - Example: `bug/TICKET-10010-fix-injection-message`
- Review the diff and keep the scope tight (avoid committing local-only files like `.envrc` and unrelated edits).
- Assume the user stages the intended changes; if anything is unclear about what should be included, ask.
- Ensure changes are tested/linted.
- Ask for an existing ticket or whether to create a new ticket.
- Ask for the tracker/project id if a new ticket needs to be created.

## Writing principle: low noise, high signal

Commit messages, tickets, and PR descriptions are read by people who will also read the code/diff. Write them to carry information the reader cannot infer from the diff — skip redundant retelling of what changed.
Prefer bullet lists and short fragments over long sentences, especially in tickets and PR descriptions. If a point takes more than one short sentence, break it into bullets.

**Include (high signal):**
- **Observation / problem.** What data, trace, bug report, or spec finding motivates this change.
- **Decisions.** What alternatives were considered and why this approach was chosen — especially trade-offs not visible in code.
- **Constraints preserved.** Invariants deliberately kept (e.g. "active check still applied to every price") when not obvious.
- **Testing (brief).** Say *what kind* — unit, manual, staging — and call out any regression test added for a specific contract.
- **Acceptance criteria for QA.** Concrete scenarios the reviewer/QA should exercise, including edge cases.

**Skip (noise):**
- File-by-file or function-by-function change descriptions — the diff shows this.
- Test counts ("23 tests pass"), typecheck status ("tsc --noEmit clean"), lint status — these are baseline expectations.
- Individual test names from the added/modified suite.
- Restating type signatures or API shapes — let code comments / diff carry that.
- "I updated X to do Y" when Y is literally what Y's function name says.

## Ticket

Create a ticket (or update the existing one) with:

```
# Problem
- <What is wrong or missing>
- <Concrete evidence: trace, failing scenario, spec gap, or constraint change>

# Expected
- <What done looks like externally>
- <Constraints that must stay true>

# QA Acceptance (optional, include if the change has non-obvious verification)
- [ ] Scenario 1 to verify
- [ ] Edge case to verify
```

## Commit message

Before committing, draft a message consistent with recent history. If the repo uses single-line commit messages matched to the PR title:

```
[<TICKET>] <verb phrase describing what the commit does> (<scope qualifier if helpful>)
```

Examples:
- `[TICKET-118] Enforce cumulative defer cap on create (slice 0)`
- `[TICKET-137] Deep-link guard: reject request at cap`
- `[TICKET-121] Fix update failing when field is null on ongoing record`

Rules:
- Start with `[TICKET]` — always include the sub-task ticket, not just the parent.
- Verb first, present tense: `Enforce`, `Fix`, `Add`, `Reject`, `Guard`.
- Add a scope qualifier `(slice N)` when the work is one part of a multi-PR epic.
- No body needed — the PR description carries the narrative.
- Match the repo's existing history for format and length.

## PR description

First check whether the repository has a PR template file:
- `.github/pull_request_template.md`
- `.github/PULL_REQUEST_TEMPLATE.md`
- any template under `.github/PULL_REQUEST_TEMPLATE/`

If a repo template exists, follow that template exactly and fill all relevant sections.

**Link the decisions.** When the PR description references a decision constant by short code (e.g. `<DECISION-SLUG>`), include a link to where the constant is defined. The link saves the reviewer a grep; it does *not* substitute for naming the trade-off in prose. Example:

```
- **<DECISION-SLUG>** ([decision](https://github.com/<owner>/<repo>/blob/<branch>/src/decisions.ts#L736)) — hard-reject overlapping starts; the alternative (silent accept) corrupts derivation.
```

**Use absolute URLs, not relative paths.** GitHub does *not* auto-resolve relative paths like `src/foo.ts#L42` in PR descriptions — they resolve against the PR page URL and break (you'll see `compare/src/foo.ts?expand=1`). Use the full `https://github.com/<owner>/<repo>/blob/<branch>/<path>#L<line>` form. The branch name keeps the link tracking the PR head as it gets pushed; a commit SHA gives a stable permalink. Find the line number with `grep -n "<DECISION_CONST>" path/to/decisions.ts`.

If a decision is mentioned only in passing (e.g. "still honours <DECISION-SLUG>"), the link is optional — link the ones the reviewer is most likely to want to read.

If no repo template exists, use this fallback PR template:

```
## <TICKET> · <parent-ticket if sub-task> (<scope label e.g. slice 0>) — <one-line summary>

Ticket: <tracker-url>/browse/<TICKET>

### Vocabulary
<!-- Define domain terms upfront so reviewers don't have to guess.
     One line per term. Include the key nouns/concepts introduced or relied on by this PR. -->

- **<term>** — <definition>

### How <feature> works
<!-- Narrative of the end-to-end user/system flow, written in the present tense after this PR lands.
     Use a numbered list for flows with a clear sequence.
     This is the most valuable section — write it so a reviewer unfamiliar with the domain
     can follow the full path. Name the system/actor at each step. -->

1. ...

### What this PR adds
<!-- One bolded sentence per decision, followed by one line of rationale.
     Reference decision slugs where they exist.
     End with any known limitation or deferred work. -->

**<Decision summary (DECISION-SLUG)>:** <why this approach, not alternatives>

### <System/API quirk> (if applicable)
<!-- Document any surprising behaviour discovered — contract gaps, silent failures,
     normalisation hacks. Explain why it exists and how this PR handles it. -->

### Errors (HTTP 200, errors in body)

| slug | code | when |
|---|---|---|
| `<slug>` | `BAD_USER_INPUT` | <condition> |

### Known limitation (if applicable)
<!-- Be explicit about what is deliberately NOT done in this PR and why.
     Name the follow-up slice or open question. -->

### Tests
<!-- Two to three sentences. Name the categories of coverage (unit, integration, e2e).
     Call out any specific regression guard added for a discovered edge case. -->
```

**What makes a good PR description:**
- **Vocabulary first** — reviewers can't follow the decisions section without shared terminology.
- **Write the flow as it works now** — not "I changed X to do Y" but "the user does A, the client calls B, the service checks C." Present tense, full path.
- **Name decisions with slugs** — a slug lets reviewers trace to the decision source without searching.
- **Surface quirks explicitly** — if you discovered a system bug or gap and worked around it, say so. Hiding it makes the workaround look like a design choice.
- **Errors table** — reviewers and QA can copy-paste slugs to write test cases or check monitoring.
- **Known limitations are not failures** — be explicit about what is deferred and why; it signals intentional scoping.

## When to use me

Use this when some work was done in the project but not shared with the team yet.
Ask clarifying questions if the problem is not clear.

## Notes

- Never push directly to `main`/`master`; push a feature branch and open a PR.
