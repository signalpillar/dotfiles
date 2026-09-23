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
- <One line, product language: who is affected and what breaks for them.
  No function names, file paths, or type names in this line>
- <Technical detail, tech language: root cause, the exact trace/error string,
  the function(s)/file(s) involved, and why>
- <Concrete evidence: trace, failing scenario, spec gap, or constraint change>

# Expected
- <What done looks like externally>
- <Constraints that must stay true>

# QA Acceptance (optional, include if the change has non-obvious verification)
- [ ] Scenario 1 to verify
- [ ] Edge case to verify
```

The Problem section's first bullet matches the first §1 bullet.
State the user-visible symptom before any code name.
The second bullet carries the technical detail.
Name the throwing function, the exact error string, and each file at fault.
An engineer can jump to the code from that bullet.

**Bad first bullet:** `resolveFlowConfigFromProgramme throws when versaFlows is undefined.`
**Good first bullet:** `A PMOS patient who taps blood collection gets stuck: Versa never starts the flow, for every PMOS purchase.`

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

Always write the hybrid template.
Do not ask whether to use repository, full, or hybrid.
Do not look for a repo PR template.
Do not read `.github/pull_request_template.md`, `.github/PULL_REQUEST_TEMPLATE.md`, or files under `.github/PULL_REQUEST_TEMPLATE/`.
Do not append repo-template sections to the PR body.
The PR body is hybrid only.

**Link the decisions.** When the PR description references a decision constant by short code (e.g. `<DECISION-SLUG>`), include a link to where the constant is defined. The link saves the reviewer a grep; it does not substitute for naming the trade-off in prose. Example:

```
- **<DECISION-SLUG>** ([decision](https://github.com/<owner>/<repo>/blob/<branch>/src/decisions.ts#L736)) - hard-reject overlapping starts; the alternative (silent accept) corrupts derivation.
```

**Use absolute URLs, not relative paths.** GitHub does not auto-resolve relative paths like `src/foo.ts#L42` in PR descriptions - they resolve against the PR page URL and break (you'll see `compare/src/foo.ts?expand=1`). Use the full `https://github.com/<owner>/<repo>/blob/<branch>/<path>#L<line>` form. The branch name keeps the link tracking the PR head as it gets pushed; a commit SHA gives a stable permalink. Find the line number with `grep -n "<DECISION_CONST>" path/to/decisions.ts`.

If a decision is mentioned only in passing (e.g. "still honours <DECISION-SLUG>"), the link is optional - link the ones the reviewer is most likely to want to read.

### Template: hybrid

Pyramid lead, then the call-stack.
Omit §4 when this PR does not change the error or API surface.

§1 states three facts, in this order.
Use simple technical language.
A reviewer who saw only the previous merged PR can read it.

1. Who still consumes what, or what is missing.
2. What this PR changes, as a product or contract fact.
3. What this PR does not do.

Write each fact as its own bullet.
Keep each bullet to one clause and 20 words.
Use **bold** and *italic* to mark the words a reader must see.
Do not join the three facts into one paragraph.
Do not chain causes with "but" or a long "and" list.
Put the cause, the rename mechanics, and the test detail in §2, §3, or §5.
Do not open a bullet on a function name, type name, or in-group label.
Put those names in §3 or the files list.

**Bad:** [graphius#1778](https://github.com/emed-labs/graphius/pull/1778) wrote 89 words in one paragraph.
It opens on `resolveHasHormoneContext` and chains the cause, the rename, and the non-change.

**Good:** the same PR, three bullets.
- Pre-prescribing hormone questions still use the same-order **PMOS Discovery** gate from #1705.
- This PR corrects the **PROG-4** text, renames the flag to **hasHormoneContext**, and adds the GraphQL test.
- It does *not* change that gate's runtime behaviour.

````
## <TICKET> · <parent-ticket if sub-task> — <one-line change>

Ticket: <tracker-url>/browse/<TICKET>

### 1. Lead
- <who still consumes what, or what is missing>
- <this PR's product or contract change>
- <what this PR does not do>

### 2. Decision
**<slug>:** <chosen approach>. Not <rejected approach>, because <reason>.

### 3. Where this sits

```
A
|
v
B  <--- this PR
|
v
C  (unchanged / next slice)
```

This PR owns **B**. It does not own **C**.

### 4. Contract (omit if the error/API surface did not move)
| slug | when |
|---|---|
| `...` | ... |

### 5. Tests / limit
- Tests: <kind + contract>
- Not in this PR: <follow-up>
````

Name A/B/C as real systems or services, not placeholders. Mark the changed node with `<--- this PR`. Name the follow-up on C when work is split across slices.

**What makes a good PR description:**
- **Always hybrid** - do not ask repository, full, or hybrid.
- **Hybrid only** - do not read or append a repo PR template.
- **Hybrid: lead then map.** §1 is three bullets (who, this change, not in this PR).
  Keep each bullet to one clause and 20 words.
  Use bold and italic so the facts scan.
  Do not open a bullet on a function name.
  §3 shows where the PR sits in the call stack.
- **Write the flow as it works now** - not "I changed X to do Y" but "the user does A, the client calls B, the service checks C." Present tense, full path.
- **Name decisions with slugs** - a slug lets reviewers trace to the decision source without searching.
- **Surface quirks explicitly** - if you discovered a system bug or gap and worked around it, say so. Hiding it makes the workaround look like a design choice.
- **Errors table** - reviewers and QA can copy-paste slugs to write test cases or check monitoring. Omit it on hybrid when the surface did not move.
- **Known limitations are not failures** - be explicit about what is deferred and why; it signals intentional scoping.

## When to use me

Use this when some work was done in the project but not shared with the team yet.
Ask clarifying questions if the problem is not clear.

## Notes

- Never push directly to `main`/`master`; push a feature branch and open a PR.
- Before push on an existing PR branch, run `git pull --no-rebase` (or equivalent) when the remote branch advanced.
- When `repo_work` already has an open PR URL for this repo, update that PR. Do not open a second PR.
