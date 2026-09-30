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

**Bad first bullet:** `loadRouteConfig throws when the route table is undefined.`
**Good first bullet:** `A customer who taps the next step gets stuck: the flow never starts, for every purchase.`

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
- **<DECISION-SLUG>** ([decision](https://github.com/<owner>/<repo>/blob/<branch>/src/decisions.ts#L736)) - reject the duplicate write; silent accept stores two records.
```

**Use absolute URLs, not relative paths.** GitHub does not auto-resolve relative paths like `src/foo.ts#L42` in PR descriptions - they resolve against the PR page URL and break (you'll see `compare/src/foo.ts?expand=1`). Use the full `https://github.com/<owner>/<repo>/blob/<branch>/<path>#L<line>` form. The branch name keeps the link tracking the PR head as it gets pushed; a commit SHA gives a stable permalink. Find the line number with `grep -n "<DECISION_CONST>" path/to/decisions.ts`.

If a decision is mentioned only in passing (e.g. "still honours <DECISION-SLUG>"), the link is optional - link the ones the reviewer is most likely to want to read.

### Template: hybrid

Pyramid lead, then the call-stack.
Keep a markdown table in §4 when the PR maps states, fields, or error slugs.
Do not drop that table because the GraphQL schema did not change.
Omit §4 only when there is no such map.

§1 is a short lead under **In one line**.
A reviewer reads it in one pass.
Use simple technical language.
A reviewer who saw only the previous merged PR can read it.

The lead has these parts, in this order.
Use one sentence per part.
Add a sentence when the PR has another product result.
A new mechanism and a new write path are both product results.
Keep both in the lead.

1. What the system did before this PR, or what behaviour stays.
   For a linter or a refactor, name the unenforced convention.
   Do not invent a member when the change is a tool or a refactor.
2. The change in the author's words.
   When the work adds a linter rule, the sentence says **linter rule**.
   Do not translate that into a user journey.
3. The boundary that remains.
   When runtime results stay the same, write *no* behavior change, a **pure refactor**.
   Put excluded tickets and sibling pull requests in §5, not in the lead.

Put each sentence on its own line.
One sentence states one fact a reviewer must remember.
Use **bold** and *italic* to mark the words a reader must see.
Do not collapse the lead into one paragraph.
Do not cut a product result to keep the lead to three sentences.
Do not replace the lead with one-clause bullets that drop a product fact.
Do not open the first sentence on a function name, type name, or in-group label.
Put file names, the cause, rename mechanics, and test detail in §2, §3, or §5.

**Bad:** one paragraph that chains the old behaviour, every result, the cause, and the tests.
It opens on a function name.

**Also bad:** a lead so short that it names one flag and drops the new mechanism.

**Good:** a small product change, one sentence per fact.
Checkout still uses the existing **payment** gate from the last release.
This PR corrects the **price label** and renames the flag to **priceConfirmed**.
It does *not* change that gate's runtime behaviour.

**Good:** a larger change, one sentence per product result.
A report included only a **fixed set** of columns.
This PR adds a **registry** so any report can register **columns**.
It also fills the **address** columns from forms other than the profile page.
Columns that no report owns still stay **blank**.

**Good:** a linter or refactor, one sentence per fact.
Helpers outside the **handler** can still open the **database**.
This PR adds a **linter rule** that rejects that use.
*No* behavior change, a **pure refactor**.

````
## <TICKET> · <parent-ticket if sub-task> — <one-line change>

Ticket: <tracker-url>/browse/<TICKET>

### 1. In one line
<what the system did before, or what behaviour stays>.
<each product result, one sentence each, in the author's words>.
<the boundary that remains, or *no* behavior change>.

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

### 4. Contract (omit only when there is no state, field, or error map)
| CarePlan state | Programme status |
|---|---|
| `active` | `ActiveProgrammeStatus` |

| slug | when |
|---|---|
| `...` | ... |

Use one table, or both, when each one states a different map.
A field list under the table is fine when the cells need a short note.

### 5. Tests / limit
- Tests: <kind + contract>
- Not in this PR: <follow-up>
````

Name A/B/C as real systems or services, not placeholders. Mark the changed node with `<--- this PR`. Name the follow-up on C when work is split across slices.

**What makes a good PR description:**
- **Always hybrid** - do not ask repository, full, or hybrid.
- **Hybrid only** - do not read or append a repo PR template.
- **Hybrid: lead then map.** §1 is a short lead under **In one line**: prior behaviour, each product result, then the boundary or *no* behavior change.
  One sentence per fact, each on its own line.
  A second product result gets its own sentence. Do not drop it to stay short.
  Use bold and italic so the facts scan.
  Do not open the first sentence on a function name.
  Put file names, causes, and tests in §2, §3, or §5.
  §3 shows where the PR sits in the call stack.
- **Write the flow as it works now** - not "I changed X to do Y" but "the user does A, the client calls B, the service checks C." Present tense, full path.
- **Name decisions with slugs** - a slug lets reviewers trace to the decision source without searching.
- **Surface quirks explicitly** - if you discovered a system bug or gap and worked around it, say so. Hiding it makes the workaround look like a design choice.
- **Tables stay in the hybrid body.** A state map or an error-slug map is a markdown table in §4. Do not drop it to keep the lead short. A schema that did not change can still need the table. Omit §4 only when there is no map.
- **Known limitations are not failures** - be explicit about what is deferred and why; it signals intentional scoping.

## When to use me

Use this when some work was done in the project but not shared with the team yet.
Ask clarifying questions if the problem is not clear.

## Notes

- Never push directly to `main`/`master`; push a feature branch and open a PR.
- Before push on an existing PR branch, run `git pull --no-rebase` (or equivalent) when the remote branch advanced.
- When `repo_work` already has an open PR URL for this repo, update that PR. Do not open a second PR.
