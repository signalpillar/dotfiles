---
name: lets-do-the-job
description: Clarify scope, execute work in isolated worktrees via a subagent, review, and share with team through tickets and PRs
license: personal
compatibility: all
metadata:
  audience: developer
---

## What I do

- Build one canonical TODO list up front so no step is silently skipped.
- Clarify scope first so implementation choices map to the real problem.
- Grill the plan before coding to resolve every branch of the decision tree.
- Run implementation in isolated worktrees for each involved repository, one slice at a time, in a per-slice loop.
- Keep the durable task record (the source of truth) and any project tracker current so work can be resumed if interrupted.
- Execute the requested changes, verify them, run review guidance, and share results with the team.
- Finish by invoking `share-with-team` to create/update tickets and open PRs.

## Trigger phrases

Use this skill for requests like:

- "Let's do the job"
- "Take this through implementation to PR"
- "Handle this end to end across repos"

## Phase 0 — Build the run checklist (do this first, every time)

**Before anything else, create a TODO list with the todo tool.** Seed it with the *same* items
every run, regardless of the task. Include the conditional ones too — mark them, do not omit them —
so nothing gets skipped by accident. This list is the spine of the whole run; work it top to bottom
and never drop an item without saying why.

Seed the list with exactly these items:

1. Explore context (repos, task record, adjacent code, project tracker).
2. Clarify scope — ask questions one by one (Phase 2).
3. Grill the plan with `grill-me` (skip only if the user declines).
4. Locate or create the durable task record (the plan; it usually already exists).
5. Implement each slice in a loop (Phase 5) — each slice gets its own nested sub-list.
6. Review pass (`volod-style`).
7. Share with team (`share-with-team`) — one ticket + one PR per repo.
8. Keep the task record up to date (spans the whole run).
9. Keep the project tracker up to date, if the project has one (spans the whole run).

Items 8 and 9 are standing obligations, not one-shot steps — revisit them after every slice.

> For all coding tasks, use your judgement to pick an appropriate **lower-power model** and run it
> in a **subagent**; the main session directs and reviews. Keep this in mind from Phase 5 onward.

## Workflow

### Phase 1 — Explore context

Before asking anything, gather what you can yourself so questions are sharp and few:

1. Identify the repositories/services in play and their absolute filesystem paths.
2. Look for an existing task record (a plan file, issue, or ticket) for this work.
3. Read the adjacent code you will touch and its nearby siblings (read before you write).
4. Check whether the project has a task tracker or project DB and read its current state.

Mark item 1 done only when you have concrete paths, not guesses.

### Phase 2 — Clarify scope (ask one question at a time)

Ask clarifications **one by one using the question capability** — one distinct decision per
question, not a single free-text prompt bundling everything. Only ask what Phase 1 did not already
answer. Typical questions:

1. Repository absolute path(s) — **required; do not infer or assume** if Phase 1 left any unknown.
2. The task-record location (the canonical task record — see Phase 4). If it came from an existing
   plan it usually already exists; confirm the path rather than assuming.
3. **"Link the tickets to an epic/parent? If so, which key?"** — carry the answer to Phase 7.
4. Any constraint (security, compatibility, rollout, deadlines) or definition-of-done ambiguity that
   changes implementation direction.

Then echo the agreed scope (user outcome, repos + paths, worktree-per-repo, constraints, DoD, epic)
before execution starts.

### Phase 3 — Grill the plan

Load and run the `grill-me` skill to stress-test the plan before writing code: resolve each branch
of the decision tree, surface conflicts rather than averaging them, and push back where a simpler
approach exists. Fold any decisions that come out of grilling into the task record's decisions.
Skip this phase only if the user explicitly declines.

### Phase 4 — Locate or create the durable task record (source of truth)

The durable task record is a **checked-in plan** that is the source of truth for the task. Use
whatever format the project standardizes on — a Markdown plan file, a tracked issue, a ticket, or a
structured task file. It is the working document; do **not** create a second parallel plan alongside
it.

1. **Locate it first.** Use the path/reference confirmed in Phase 2. Check the project's usual
   location for an existing record matching this task before creating anything. If one exists, USE it.
2. **If none exists, create it** following the project's convention. At minimum it should carry:
   a status (`planned|ready|in_progress|blocked|done`), the source/context, a list of decisions
   (`{date, note, source}`), and a plan of slices — each slice `{key, title, goal, status, note,
   steps, final_steps}` where `final_steps` is always `[e2e-evidence, update-task-record]`.
3. **Plan the slices.** One slice per coherent, independently-landable increment; use **multiple
   slices when PRs must land separately** (multi-repo, feature-flag gate, staged rollout). This
   slice list drives the Phase 5 loop.

Rules:
- Populate project/source and the first-slice `goal` from Phase 2 before the subagent starts.
- Keep notes short and factual; the record is a resume anchor, not a design doc.
- Validate any structured edits with the project's schema/format checker.

### Phase 5 — Implement each slice (per-slice loop)

Loop over the plan's slices. **For each slice, add a nested sub-list to the TODO** with these items —
always the same, so none is skipped:

1. **Negative control tests** — write/identify a test that *fails* for the right reason first, so a
   later pass proves the change did something (tests check behavior).
2. **Implement** the slice inside the worktree.
3. **Verify** — run tests/build/lint from the worktree path; confirm the negative control now passes.
4. **E2E test evidence** — run e2e/acceptance coverage when possible (see below) and capture evidence.
5. **Update the task record** — set the slice's step statuses, decisions, session links (PR URLs,
   ticket keys), and both `final_steps` notes; flip the slice `status` to `done`.
6. **Update the project tracker** if the project has one (see Standing obligations).

Do not start the next slice until the current slice's sub-list is fully checked off.

**Worktree + subagent setup for the loop:**

1. Launch a subagent for implementation-heavy work, running a **lower-power model of your choosing** —
   the main session directs and reviews.
2. **For each target repository, create a git worktree before touching any files:**
   - Check the repo's current branch: `git -C <repo> branch --show-current`.
   - If on `main`/`master` or any branch that is not the intended feature branch:
     `git -C <repo> worktree add <worktree-path> -b <feature-branch>`
   - If already on the correct feature branch, still create a worktree from it:
     `git -C <repo> worktree add <worktree-path> <feature-branch>`
   - **All editing must happen inside the worktree path — never in the original repo directory.**
3. Keep worktree paths and branch names deterministic, e.g. worktree at `<repo>/../<repo>-<ticket>`,
   branch `<type>/<ticket>-<short-topic>`.
4. Require the subagent to reference the task record and return a completion summary with changed
   files, commands run, and unresolved risks.

**Test scenario mapping (do this as part of step 1/4 of each slice):**

- Use the project's flow/scenario map as the canonical scenario source when user-facing flows are involved.
- Map changed backend behavior to impacted user-flow nodes (screen and transition); include variant
  and feature-flag combinations that can alter behavior.
- Write the resulting test list into the slice's `note`/`steps`; mark executed vs deferred explicitly.

**E2E / acceptance discovery and run (step 4):**

1. **Discover** what exists (stop at first match):
   - a dedicated e2e test suite (example: Jest e2e — `src/tests/e2e/**/*.e2e.ts`, run with
     `npx jest --testRegex="<feature>\.e2e\.ts$"`)
   - `*.http` / `*.httpyac` files — run with `httpyac run <file>`
   - any `e2e`, `smoke`, or `acceptance` script in `package.json` (or the project's task runner)
2. **Run** the tests covering the changed feature. If the server must start locally: check for a busy
   port (`lsof -i :<port>`, pick a free one); read the port var from the project's env file; override
   when starting; point the test env at the chosen port (revert after); stop the temporary server
   afterward.
3. **Capture evidence:** test names, pass/fail, duration, representative response payloads.
4. **Include evidence** in the PR description and as a ticket comment (same format as `share-with-team`).
5. If no e2e/acceptance coverage exists for the changed feature, note it explicitly — it is a gap,
   not a pass.

### Phase 6 — Review pass

1. Load `volod-style` for architecture, observability, and test-quality checks; self-review for
   correctness, error modeling, and edge cases.
2. Fix findings in the same worktree; re-run relevant verification.
3. Mark review done — `share-with-team` will be told to skip its own `volod-style` prompt.

### Phase 7 — Share with team

1. Load and invoke `share-with-team` once per repository changed.
   - Tell it: **"volod-style review already done in Phase 6 — skip that prompt."**
   - Tell it: **"create one ticket for this repo"**, supplying the epic/parent key from Phase 2 (if any).
   - Do not create tickets or write PR descriptions independently — `share-with-team` owns that.
   - **Use its templates, do not improvise.** Actually open `../share-with-team/SKILL.md` and follow
     its **ticket template** and **PR description template** verbatim: for the PR, check for a repo
     template first (`.github/pull_request_template.md` etc.); if none exists, use its fallback PR
     template (Vocabulary → How it works → What this PR adds → quirks → Errors table → Known
     limitation → Tests). Skipping these templates is the most common failure of this phase — treat
     "invoke `share-with-team`" as "produce the ticket and PR body from its templates," not as a
     loose handoff.
2. Use **tables and ASCII diagrams** in ticket descriptions wherever they add clarity:
   - Tables for field mappings, config changes, state transitions, side-by-side comparisons.
   - ASCII before/after diagrams for flow, data-model, or topology changes:
     ```
     Before:            After:
     A → B → C          A → B → D
                              ↘ C
     ```
3. Return a final digest: ticket key(s), PR URL(s), what changed and why, verification performed, and
   e2e/acceptance evidence (or explicit note if none exists).

## Standing obligations (revisit after every slice)

- **Keep the task record up to date.** Top-level `status`, each slice/step `status`
  (`planned` → `in_progress` → `done`), decisions, session links (PR URLs, ticket keys, handoff
  docs), and both `final_steps` notes. It replaces any ad-hoc plan and is the resume anchor.
- **Keep the project tracker up to date** when the project has one. Reflect: new ticket created,
  status changes (Backlog → In Progress → Done), question resolved (e.g. by grilling), new open
  question surfaced, new decision codified.

## Default decisions

- Ask scope questions **one at a time** via the question capability; prefer one focused question over
  an assumption when ambiguity changes implementation direction.
- **Always use a worktree per repo — no exceptions.** Editing the original working directory while it
  may have unrelated in-progress changes is not safe.
- **Run coding work in a subagent on a lower-power model of your choosing**; the main session directs
  and reviews.
- The task record is the source of truth; keep it short and operational, not a design doc. Do not
  substitute a second parallel plan for it.
- Prefer proper type/error modeling over ad hoc filters.

## Completion checklist

- Phase 0 TODO list built with all canonical items (including conditional ones).
- Context explored; repos + paths known; existing task record / project tracker checked.
- Scope clarified one question at a time; epic key captured (or explicitly skipped).
- Plan grilled with `grill-me` (or user declined); decisions folded into the task record.
- Task record located or created with project/source and first-slice `goal` populated; slices planned.
- Each slice run through its loop: negative control test → implement → verify → e2e evidence →
  task-record update → project-tracker update; slice `status` set to `done`.
- Worktree(s) created and isolated; coding done in a subagent on a lower-power model.
- E2E/acceptance evidence included in PR description and ticket comment (or gap noted).
- Review pass done (`volod-style`).
- Task record and project tracker current.
- `share-with-team` invoked once per repo — one ticket (with tables/ASCII diagrams where useful)
  and one PR each, both written from `share-with-team`'s ticket and PR templates (repo PR template
  if present, else its fallback template) — not improvised.
