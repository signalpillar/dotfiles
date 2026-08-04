# pi-job-harness

Portable deterministic job harness for machine-owned YAML task files.

`pi-job` keeps durable task state in exactly one `TaskStore` backend, with YAML as the default.
It validates task state and the package-local `profile.yaml` through documented Pydantic models, computes the next unfinished slice or step, updates the orchestration cursor, and emits deterministic instruction packets.
Legacy CUE files remain readable and writable during migration, but every CUE invocation prints the exact `project --to <task>.yaml` migration command.

## Contents

```text
bin/pi-job              CLI entrypoint
profile.yaml            validated step kinds, slice kinds, toolbelt, and artifact rules
pyproject.toml          ruff config (includes extensionless chezmoi scripts)
task-schema.cue         compatibility schema used only by legacy CUE tasks
tests/executable_test_pi_job.py   regression tests (may install as tests/test_pi_job.py)
```

## Dependencies

- Python 3.12+
- `uv`, which resolves the script's PEP 723 dependencies
- `cue` CLI on `PATH` only while reading, writing, or migrating legacy CUE tasks

The executable declares compatible Pydantic and PyYAML versions in its inline PEP 723 metadata.
No separate virtual environment or package installation command is required when invoking it through `uv run`.

Recommended: use [`uv`](https://docs.astral.sh/uv/) to install and pin the Python version.
`uv` also keeps a working Python available across machines without fighting system Python.

```bash
# install uv: https://docs.astral.sh/uv/getting-started/installation/
uv python install 3.12
uv python pin 3.12   # optional, in a project directory
```

## What pi-job does

`pi-job` is a small CLI that answers orchestration questions from durable state.
It does **not** run the agent session and does **not** spawn subagents.

Given a YAML task file and package-local `profile.yaml`, it can:

- `bootstrap` - create and initialize a task file from a bootstrap intent document in one atomic transaction (the fastest path from nothing to a working task)
- `scaffold` - create a missing task file from the generic example shape; `--empty-plan` and `--initial-kind K` for targeted seeding
- `init` - initialize `task.orchestration` and set the cursor; optionally seed the first slice from a slice kind template
- `add-slice` / `remove-slice` - add or remove ordered slices with steps from the profile template
- `add-step` - append a step to a slice
- `set-project` / `set-context` / `set-plan-note` / `add-decision` / `set-slice` / `block-slice` / `unblock-slice` / `acknowledge-edit` - write task metadata and product/scope decisions without hand-editing the store
- `status` / `next` / `plan` - report where the work is and what slices/steps remain
  (`status` also reports `Structure: ok` or a non-fatal `Structure: invalid` line from slice template lint; warns on oversized notes / large files)
- `show` / `show --slice KEY` / `show --full` / `show --short` - tree view (compact by default), optional models, collapsed consecutive done names, or a slice-local detail view (goal, notes, steps, repo_work)
- `markdown` / `markdown --chronological` / `markdown --summary` / `markdown --slice KEY` - read-only Markdown preview on stdout (works without orchestration init; never mutates the store)
- `instruction` - emit a deterministic packet for the current or next cursor (owner, validators, gates, todo reminders, task-record discipline)
- `start` / `finish` - record the executing model and UTC timestamps while transitioning slice/step status (`finish --note` appends by default; `--replace` overwrites)
- `advance` - write the next cursor back into the task file after evidence lands; fails closed if the current step is not `done`/`skipped` unless `--force --reason '<why>'` is given
- `profile` / `schema` / `kinds` - inspect the active execution profile, task document schema, and slice kinds
- `toolbelt` - list or register planning aids
- `sync` - print a checklist of slices worth re-verifying
- `set-worktree` / `add-pr` - manage worktree paths and pull request records
- YAML writes store a semantic `orchestration.content_digest`; hand-edits produce a loud warning until `acknowledge-edit --reason`

Same task state should yield the same next action and instruction, independent of which model is orchestrating.

## Orchestrator loop

Assumption: a smart orchestrator model keeps calling `pi-job` instead of freelancing from chat memory.

1. `pi-job --task <file> status` (and usually `plan`)
2. `pi-job --task <file> instruction` (current or next)
3. `pi-job --task <file> start --model <provider/model>`
4. Do that step in the orchestrator session, or launch a subagent when the packet says so
5. Record evidence / decisions / blockers, then run `finish [--note ...]`
6. `pi-job --task <file> advance`
7. Repeat until `next` is `done`

During every step, capture discovered future work, revisitable issues, technical debt, and unresolved doubts as explicit bounded slices with the appropriate kind and dependencies.
Do not leave actionable follow-up work only in notes.

`pi-job` only answers "what next?" and "how should this step run?" when asked.
The orchestrator owns model choice, tool use, and whether to keep consulting the harness.

## Picture

```text
  human / capture
        |
        v
  YAML task file <----------------------------- advance (atomic rewrite)
  (concrete work: slices, steps, status, evidence, cursor {slice, step})
        |
        | strict Pydantic validation
        v
     pi-job  ---- loads ----> profile.yaml
        |                     (step_kinds, slice_kinds, toolbelt, artifact_rules)
        |
        +-- status / plan / next
        |
        +-- instruction ----> orchestrator agent
                                   |
                                   +-- session todos (track slice/step plan)
                                   |
                                   +-- maybe subagent (lower-cost model)
                                   |
                                   +-- evidence back into task file
```

## Concepts: task file vs contract catalogs

The task file stores **concrete work**: slice keys, titles, goals, per-step status and notes, decisions, artifacts, repo work, and the saved cursor.
The contract file stores **reusable live meaning**: step owners, guidance, validators, artifact gates, slice-kind policies, and default step templates for new slices.
Old tasks pick up contract updates without rewriting step bodies because metadata is looked up by step key at runtime.

```text
TASK FILE (durable state)                    CONTRACT (profile.yaml)
═════════════════════════                    ═════════════════════════════════

task.plan.slices[]                             slice_kinds: setup | implement | closing
  key, title, goal, status, note, execution      | research | spike
  kind  ───────────────────────────────►         policies, step_template (for add-slice / init)
  depends_on[]  ◄── guard: skip until deps done
  steps[] + final_steps[]                      step_kinds: keyed catalog
    key, title, status, note, execution           owner, guidance, validators,
    key ───────────────────────────────►         artifact_gates, skip_rule
  repo_work, decisions, artifacts

task.orchestration.cursor                      toolbelt: aids keyed by suits: [slice kinds]
  slice, step  (no phase, no profile)

WALK ORDER
══════════
next / advance walk task.plan.slices in array order.
Within each slice: steps[], then final_steps[].
Skip slices that are done/skipped, blocked, or have unsatisfied depends_on.
First unfinished step wins.
Closing work is a closing slice at the end of the plan, not a post-slice phase tail.
When every slice is done/skipped, next reports done.
```

Typical slice layout for an end-to-end implementation task:

```text
1. task-setup          [kind: setup]     explore → clarify → grill → wayfinder → select-toolbelt → plan-slices
2. wire-api            [kind: implement] create-plan → grill-plan → edit-code → verify → vulnerability-scan → … → wait-for-feedback
3. fix-follow-up       [kind: implement] …
4. task-closing        [kind: closing]   update-test-plan → update-docs → capture-metrics → update-task-file
```

Cursor example: `{slice: "wire-api", step: "grill-plan"}`.

### Custom step keys

Steps may use keys not listed in `step_kinds`.
They remain valid in the task file and in cursor walks.
`instruction` defaults owner to `orchestrator` and omits step-kind guidance, validators, and artifact gates.
Add a contract entry when a step key should carry shared enforcement text.

### Guards vs validators

**Guards** are code paths that can refuse a command:

1. `blocking_incomplete_step()` - `advance` cannot move past an incomplete current step unless `--force --reason`
2. `dependency_satisfied()` / `is_actionable()` - `next` skips slices whose `depends_on` are not done/skipped
3. `enforce_owner_policy()` - dies when owner is subagent but slice-kind coding policy forbids it without a recorded exception
4. `blocking_execution_policy()` - enforces user-decision and distinct-model policies declared by the current step kind

**Validators** are descriptive strings on a `#StepKind`.
They appear in `instruction` and `plan` for orchestrator self-check.
`pi-job` does not parse or verify validator strings except where step-order blocking already applies.

## Principles

- Task state lives in exactly one `TaskStore` backend (YAML by default) - no parallel cursor and no agent memory as state.
- Slice kinds and step kinds are configuration in `profile.yaml`, not hardcoded Python.
- YAML task files are machine-owned documents.
  Prefer `pi-job` mutation commands over manual edits.
  pi-job stores `orchestration.content_digest` on writes and warns on read when semantic content no longer matches; run `acknowledge-edit --reason` after a legitimate hand-edit.
  `validate` does not fail on a stale digest.
- The harness is deterministic and fail-closed: missing task → `scaffold`; missing orchestration → `init`; then `plan` / `instruction` / `advance`.
- `pi-job` emits instruction packets.
  It does not spawn agents.
  The orchestrator chooses models and launches subagents.
- Session todos should track the slice/step plan from `plan`, not a separate profile phase list.
- Prefer small context: `status` / `show --slice` / `markdown --slice` / `instruction` over loading the whole task file.
  Token smell: if a step needs a huge dump, shrink the contract or the slice.
- Sibling slice plans are succinct constraint-and-behaviour contracts (intent, behaviour, constraints, verification).
  Persist product/scope/architecture/policy agreements with `add-decision` (and/or the grilled plan), not only in chat.
  Step evidence belongs in `finish --note`, not `add-decision`.
- Developer experience and agent experience share the same constructs: clear names, modular boundaries, and machine-readable contracts help both.

## Channels

Authoritative channel rules live in `profile.yaml`
(`instruction_packets.task_record_discipline`).
CLI help for `add-decision` / `finish --note` lives in `profile.yaml` `cli_help`
(loaded into `--help`; do not hardcode those strings in Python).

Short examples:

**Good `add-decision`**

- UK ConfirmMedication assets resolve via ProgrammeDefinition.defaultPartnerId; no static Graphius maps.
- Ship with temporary US CDN assets; UK-native assets blocked on uk-treatment-assets-commission before prod.

**Bad `add-decision` (use `finish --note` / `add-pr` instead)**

- PR #3420 MERGED + deployed to dev-uk.
- e2e passed for assetUrl on ConfirmMedication.
- RESOLVED: folded mapping into SHEMED-2329.

**Good `finish --note`**

- Ran graphius e2e on dev-uk; ConfirmMedication assetUrl returns CDN path; PI hold path verified.
- `gh pr view` shows #3420 merged; advanced past wait-for-feedback.

## How an agent should know about it

1. Global hint: chezmoi [`AGENTS.md`](../../../AGENTS.md) points agents at `pi-job` for durable task orchestration.
2. On PATH after chezmoi apply: `pi-job` -> `~/.local/bin/pi-job`.
3. When a user names a task file, run `pi-job --task <file> status` first.
4. If the file is missing, the preferred path is a single `bootstrap` transaction.
   Alternatively, use `scaffold --empty-plan` (or `scaffold --initial-kind K` when appropriate), then `init [--kind setup]` to seed the first slice.
   In either case, use task mutation commands (`set-project`, `set-context`, `add-decision`, `set-plan-note`, `remove-slice`) to shape the task.
5. Use `plan` to create session todos, `instruction` before acting, and `advance` only after evidence or a recorded blocker.
6. Prefer this package README over inventing a parallel workflow.

## Agent self-install (no full repo clone)

If `pi-job` is missing, an agent can pull only the harness files from the public raw GitHub tree and install them under `~/.local` - no need to clone `signalpillar/dotfiles`.

```bash
# Prefer uv so Python 3.11+ is managed for you:
#   curl -LsSf https://astral.sh/uv/install.sh | sh
#   uv python install 3.12

BASE=https://raw.githubusercontent.com/signalpillar/dotfiles/master/dot_local/share/pi-job-harness
mkdir -p ~/.local/share/pi-job-harness/bin ~/.local/bin
curl -fsSL "$BASE/bin/executable_pi-job" -o ~/.local/share/pi-job-harness/bin/pi-job
curl -fsSL "$BASE/profile.yaml" -o ~/.local/share/pi-job-harness/profile.yaml
curl -fsSL "$BASE/task-schema.cue" -o ~/.local/share/pi-job-harness/task-schema.cue
curl -fsSL "$BASE/README.md" -o ~/.local/share/pi-job-harness/README.md
chmod +x ~/.local/share/pi-job-harness/bin/pi-job
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'exec uv run --script "$HOME/.local/share/pi-job-harness/bin/pi-job" "$@"' \
  > ~/.local/bin/pi-job
chmod +x ~/.local/bin/pi-job
# requires: uv with Python 3.12; cue is needed only for legacy CUE tasks
```

Direct system-Python execution requires compatible Pydantic and PyYAML packages to be installed manually.
The `uv run` wrapper is preferred because it honors the executable's declared dependency versions.

## Install (chezmoi / local copy)

Copy this directory anywhere, for example into chezmoi:

```bash
~/.local/share/chezmoi/dot_local/share/pi-job-harness/
```

Then either add `bin/` to `PATH` or wrap the executable:

```bash
# after chezmoi apply, files land at:
~/.local/share/pi-job-harness/bin/pi-job
~/.local/bin/pi-job   # thin wrapper
```

When kept inside a product repo, a thin wrapper such as `scripts/pi-job` can point at the global install.

## Usage

Run from the repository that owns the task file.
The current working directory is reported as the repository root in instruction packets.

```bash
# The fastest path: bootstrap from an intent document (one transaction)
pi-job --task projects/example/tasks/task.yaml bootstrap --from task.bootstrap.yaml

# If the task file does not exist yet (scaffold then init):
pi-job --task projects/example/tasks/task.yaml scaffold --empty-plan
pi-job --task projects/example/tasks/task.yaml init --kind setup

# Alternative: scaffold with the initial slice already seeded
pi-job --task projects/example/tasks/task.yaml scaffold --initial-kind setup
pi-job --task projects/example/tasks/task.yaml init

pi-job --task projects/example/tasks/task.yaml status
pi-job --task projects/example/tasks/task.yaml validate
pi-job --task projects/example/tasks/task.yaml plan
pi-job --task projects/example/tasks/task.yaml next
pi-job --task projects/example/tasks/task.yaml instruction --current
pi-job --task projects/example/tasks/task.yaml start --model openai/gpt-5.6-sol
pi-job --task projects/example/tasks/task.yaml finish --note "Verification evidence recorded."
pi-job --task projects/example/tasks/task.yaml advance
```

If `--task` points at a missing file, commands fail closed and tell the agent how to create one.
A task without `task.orchestration` is not initialized; run `init` (or `bootstrap` which includes it) before `plan`, `next`, `advance`, or `instruction`.

### validate

- `pi-job --task <t> validate` is the canonical way to check a task file.
- `pi-job --task <t> validate --slice KEY` checks only that slice against the active profile (kind plus required/template steps).
  On success, if other slices still have structure issues, pi-job prints a non-fatal note (`full-task: N legacy structure issue(s); use validate without --slice`) and still exits 0.
  Unknown slice keys fail closed with the known slice keys listed.
- YAML syntax is loaded with duplicate-key detection.
- Task and profile fields are checked through strict Pydantic models that reject unknown fields.
- Slice structure and live profile-template requirements are checked after document validation.
- Legacy CUE validation still exports through `task-schema.cue` before applying the same Pydantic task model.
- `validate` and `status` warn (non-fatal) when slice or step notes exceed ~2000 characters or the task file exceeds ~100KB; keep long prose in slice plan files.

### scaffold

- `scaffold` creates a YAML task file from the generic example shape (one implement slice `do-the-change`).
- `--empty-plan` scaffolds with `plan.slices: []` so the plan starts clean; pair with `init --kind setup`.
- `--initial-kind K` scaffolds with a single slice of kind K (e.g. `setup-slice` with key `{kind}-slice`), immediately valid for `init` without `--kind`.
- `--dry-run` prints the scaffold content without writing.
- `--force` overwrites an existing file.

### bootstrap

`bootstrap` creates and initializes a task file from a bootstrap intent document in one atomic transaction.
It is the fastest path from nothing to a working task: validates the intent, expands steps from the active profile, seeds the initial slice, initializes orchestration, writes atomically, reads back, and emits the deterministic instruction packet.

```bash
pi-job --task projects/example/tasks/task.yaml bootstrap --from task.bootstrap.yaml
pi-job --task projects/example/tasks/task.yaml bootstrap --from task.bootstrap.yaml --dry-run  # unified diff, no write
pi-job --task projects/example/tasks/task.yaml bootstrap --from task.bootstrap.yaml --force    # replace existing
```

The bootstrap intent document describes the task shape holistically:

```yaml
title: Add GCSE roadmap support resources
initial_slice_kind: setup
source:
  discovered: "2026-07-27"
  context: Why this task exists.
project:
  key: gcse-science-f1-roadmap
  name: GCSE Science F1 Roadmap
  route: prototypes/gcse-science-f1-roadmap.html
context: Background required before acting.
decisions:
  - date: "2026-07-27"
    source: chat:2026-07-27
    note: Keep the existing published HTML URL.
slices:
  - key: support-foundation
    kind: implement
    title: Build the support foundation
    goal: Add the resource schema, loader, UI and validation.
    depends_on:
      - task-setup
```

Safety guarantees:

- Refuses accidental overwrites (pass `--force` to replace).
- Validates the prospective task before writing, including the Pydantic task contract.
- Expands steps only from the active profile's step templates.
- Verifies slice keys through `TaskDocument` and checks that all dependency references resolve.
- `--dry-run` prints a unified diff without writing.
- Writes through a temporary file and atomic rename.
- Reads back and revalidates the written result; aborts on semantic mismatch.
- Returns the initialized cursor and the full instruction packet for the orchestrator to act on.
- Reports the active profile and schema version used.
- When the bootstrap plan introduces implement or spike slices (kinds whose template includes `create-plan`), prints a trailing `SEED SLICE PLAN FILES NOW` reminder listing relative plan paths so agents can write plan files immediately.

### init and add-slice

- `init [--kind K]` creates `task.orchestration` with cursor at the first actionable slice/step.
  When `plan.slices` is empty and `--kind` is supplied, seeds one slice from `slice_kinds[K].step_template`.
- `add-slice --kind K --key … --title … --goal …` is required for every new slice; steps are filled from the template.
- After a successful non-dry-run `add-slice` for a qualifying kind, prints the same seed reminder for that slice only.

### Task mutation commands

These commands write task metadata and durable state without editing the YAML by hand:

- `pi-job --task <t> set-project --key K --name N --route R --context C` - merge into `task.project` (at least one flag required).
- `pi-job --task <t> set-context --context TEXT` or `--file PATH` - replace `task.context`.
- `pi-job --task <t> add-decision --date YYYY-MM-DD --note RATIONALE --source ORIGIN` - append a product/scope decision (not step evidence; use `finish --note`; date defaults to today UTC; source defaults to `pi-job add-decision`).
- `pi-job --task <t> set-plan-note --note TEXT` - set `task.plan.note`.
- `pi-job --task <t> acknowledge-edit --reason R` - refresh `orchestration.content_digest` after a legitimate hand-edit and record a decision (YAML only).
- `pi-job --task <t> set-slice --key K [--title T] [--goal G]` - update slice metadata (at least one of `--title` or `--goal` required; YAML only; refuses `done`/`skipped` slices).
- `pi-job --task <t> block-slice --key K --reason R` - mark a slice `blocked` and append the reason to its note (YAML only; refuses `done`/`skipped`; re-block appends again).
- `pi-job --task <t> unblock-slice --key K` - restore a `blocked` slice to `planned` without changing its note (YAML only).
- `pi-job --task <t> remove-slice --key K` - remove a slice from the plan. Refuses when:
  - another slice declares a `depends_on` reference to it
  - the orchestration cursor points at it (advance to another slice first)

### Introspection

These commands do not require `--task`.

- `pi-job profile [--json]` - show the active execution profile. Human output lists slice/step/toolbelt counts; `--json` dumps the full validated profile.
- `pi-job schema [--json]` - show the task document and bootstrap input schemas. Human output summarizes model counts; `--json` dumps a complete JSON Schema object with `task` and `bootstrap` keys.
- `pi-job kinds list [--json]` - list all slice kinds with their step templates. `--json` dumps the full slice_kinds catalog.
- `pi-job kinds show <kind> [--json]` - show one slice kind's details including expanded step entries (title, owner). `--json` adds resolved step metadata.

### advance

- Fails closed when the saved cursor's step is not `done`/`skipped`.
- `--force --reason '<why>'` marks the current step skipped before advancing.
- `--resync --reason '<why>'` realigns the cursor without changing step status; mutually exclusive with `--force`.
  Without `--slice/--step`, bypasses the unfinished-current guard and moves to computed `next`; fails closed when that would stay on the same unfinished step (pass explicit `--slice/--step` instead).
- Use `--slice` and `--step` to jump explicitly, or omit both to advance to computed next.

### Execution lifecycle

`start` and `finish` store provenance directly on the selected slice or step:

```yaml
execution:
  model: openai/gpt-5.6-sol
  started: "2026-07-21T10:00:00Z"
  ended: "2026-07-21T10:04:30Z"
```

- Model IDs should be fully qualified as `provider/model` so model-separation checks are meaningful.
- Timestamps are generated by `pi-job` as UTC ISO 8601 values.
- With no target flags, lifecycle commands operate on the saved cursor step.
- `--slice-only` targets the current slice; `--slice K` targets another slice; add `--step K` to target one of its steps.
- Start the slice with the orchestrator model, then start and finish each step with the model that directly performs it.
- `finish --skip --model <id> --reason '<why>'` records an atomic skip when no prior `start` exists.
- `finish --note '<evidence>'` appends completion evidence with a blank line when a note already exists; omitted preserves the existing note.
- `finish --replace --note '<evidence>'` overwrites the existing note instead of appending (`--replace` requires `--note` and cannot combine with `--skip`).
- `finish --reconcile --model <id> --note '<evidence>'` records completion for an `in_progress` target that was never started via pi-job; refuses `planned`/`done`/`skipped`.
- Normal `finish` without `--reconcile` still requires a prior `start` (unless `--skip`).
- `start` refuses `blocked` slices and blocked lifecycle targets; run `unblock-slice` first for slice-level blocks.
- Existing tasks without execution metadata remain readable; `validate` reports warnings instead of inventing historical data.
- Slice kinds may declare `required_steps` separately from `step_template`: persisted slices must satisfy the stable structural minimum, while later template additions produce migration warnings instead of invalidating old tasks.

### Independent vulnerability scan

Every new implement slice includes `vulnerability-scan` after acceptance evidence and before sharing.

1. The orchestrator asks the user whether the scan is required for that slice.
2. If accepted, the orchestrator selects a scanner model whose fully qualified ID differs from `edit-code.execution.model`.
3. The scanner reviews changed/generated code for vulnerabilities and records findings in the step note.
4. The step finishes only after findings are resolved or the remaining risk is explicitly accepted.
5. If the user declines, the orchestrator records `finish --skip --model <id> --reason '<user decision>'`.

`start` rejects the code-author model for this step, and `advance` rechecks recorded provenance so externally modified status cannot bypass the model-separation rule.
The CLI does not recognize `vulnerability-scan` by name; it applies the generic `requires_user_decision` and `different_model_from_step` fields declared on any step kind.

## Migrating CUE storage

Use the existing backend projection command to create a sibling YAML task:

```bash
pi-job --task projects/example/tasks/task.cue project \
  --to projects/example/tasks/task.yaml
```

The command detects both stores from their paths, validates the CUE export, writes YAML atomically, reads it back, and verifies semantic equality through `TaskDocument`.
It refuses to overwrite an existing YAML destination and never modifies, renames, or deletes the CUE source.

CUE reads and writes remain supported temporarily.
Every CUE invocation prints a deprecation warning containing the corresponding projection command.

## Migrating legacy CUE schemas

If a task file was created before `task-schema.cue` existed, it may have local copies of type declarations (`#Status`, `#Step`, `#Decision`, `#Artifact`, `#Slice`) at the top level.
These are now legacy - the shared `task-schema.cue` is unified into every legacy CUE load automatically, making local copies redundant and a source of confusion.

`pi-job migrate-task` diagnoses a CUE task for these legacy declarations and prints deletion or refactoring recommendations.
It does not perform storage migration; use `project --to <task>.yaml` for that.
It never modifies the file.
If an emergency manual cleanup is required, run `pi-job validate` immediately afterward.

Note: a bare `cue vet` or `cue export` invoked on the migrated task file *alone* will fail with missing reference errors like `reference "#Step" not found` - this is expected and not breakage.
Use `pi-job validate` rather than invoking `cue` directly.

### Migrating from v1 profile/phase model

v1 stored `task.orchestration.profile`, `cursor.phase`, and walked post-slice profile phases.
v2 replaces profiles with slice kinds on each slice and a two-field cursor only.
Remove `profile` and `phase` from task files; add `kind` to each slice; express setup and closing work as setup/closing slices in `plan.slices`.
See `projects/pi-agent-job-harness/workflow.md` in the weight-loss repo for the decision summary.

## Toolbelt and visualization

- `pi-job --task <t> toolbelt` - list planning aids whose `suits` includes a slice kind present on the task (or pass `--kind K` to filter).
- `pi-job --task <t> toolbelt add <key> [--path P] [--status S] [--note N]` - register/update a planning aid as an `#Artifact` under `task.orchestration.artifacts` (idempotent; validates `<key>` against the catalog).
- `pi-job --task <t> show [--all] [--started] [--full] [--short] [--status s1,s2] [--color auto|always|never]` - render the task as a cursor-focused slice/step tree with a toolbelt footer.
  `--short` collapses consecutive `done` slices onto one line (`✓ a, b, c`); skipped breaks the run; ignored with `--all`.
  By default only the current cursor slice expands.
  `done`/`skipped` slices are completely header-only (no deps, repo_work, or steps) and omit `[kind/n/m]` (footer still has totals).
  Executor models are omitted unless `--full`.
  `--started` additionally expands `in_progress`/`blocked` slices.
  `--all` expands every slice including finished ones.
  `--status` filters which slices are listed.
  `--color` tints status glyphs for humans (`✓` green, `✗` red, `▸` cyan, `⊘` yellow, `○`/`·` dim); default `auto` (TTY only, respects `NO_COLOR`).
- `pi-job --task <t> show --slice KEY` - render one slice in full: goal, slice note, every step (key, status, model, note), and repo_work.
  Does not dump task-level context, plan note, or decisions.
  Tree flags (`--all`, `--started`, `--status`, `--full`) are ignored when `--slice` is set.
- `pi-job --task <t> markdown [--chronological] [--summary | --slice KEY]` - render a portable Markdown preview to stdout.
  Loads through `TaskStore`, validates, and never writes back.
  Uninitialized tasks (no `orchestration`) preview when the document validates.
  Document order: title/status, project, prominent `## Decisions` (dated bullets; `_none_` when empty), context and remaining metadata (empty sections omitted), a default `## Contents` table of slices (key + title, with `(current)` and status; links to slice anchors), then slices/steps.
  Prefer Markdown when recording notes and decisions (`finish --note`, `add-decision`, `set-context`, `set-plan-note`).
  Decisions and nested notes render as blockquotes; context and plan notes render as Markdown prose.
  Titles and headings are escaped.
  The saved cursor slice and step are marked inline with `(current)`; orchestration is not dumped as a separate appendix.
  Default is the full dump.
  `--summary` keeps Decisions and Contents, then only slice headers and goals (no context/source/artifacts/steps/notes).
  `--slice KEY` keeps the document header and Decisions, then one slice in full; unknown keys fail closed.
  `--summary` and `--slice` are mutually exclusive.
  Default slice order follows `plan.slices`.
  `--chronological` sorts slices by the earliest non-empty `execution.started` or `execution.ended` on the slice or any step/final_step (no-timestamp slices after; plan order tie-break).
- Subagent instruction packets treat the emitted packet as sufficient context; they do not order inspecting the task store directly or opening full `profile.yaml`.
  Subagents must run `pi-job --task <t> markdown --slice <key>` first to load binding `## Decisions` plus that slice.
  For sibling-step evidence after decisions are loaded, the subagent prompt points at `pi-job --task <t> show --slice <key>`.
  Orchestrators dispatch subagents with the markdown --slice command; they do not paste a decisions dump.

The setup slice's `select-toolbelt` step picks aids suited to the task's slice kinds; `plan-slices` produces them.
The catalog lives in `profile.yaml` under `toolbelt`.

## Planning before code changes: create-plan / grill-plan / grill

**Setup slice** uses step key `grill` to interrogate overall task scope before implement slices exist.

**Implement and spike slices** must lead with `create-plan` then `grill-plan` before other work in that slice.
Sibling plan files are succinct constraint-and-behaviour contracts.
Full wording (required sections, grill axes, task-store boundary, naming, skip exception) lives in `plan_and_grill_guardrail` and the create-plan / grill-plan step guidance in `profile.yaml`.
Do not restate that contract here.

## Charting foggy work: wayfinder

When a task is too big and foggy to plan in one setup pass, the `setup` slice's `wayfinder` step (and any `fog` slice) charts the way to the destination one decision at a time, instead of forcing implement slices up front.
The map is the task file itself: `decisions` and slices, readable by any later session.

- `pi-job --task <t> wayfinder-context` - print the map reconstructed from the task file at the slice level (no step noise): the `DESTINATION` (`plan.note`), recorded `DECISIONS`, `IN PROGRESS / DONE` slices, the `FRONTIER` (planned slices whose dependencies are satisfied), and the `FOG` (planned slices still blocked, with their unmet dependencies).
  Read-only; reuses the same `is_actionable` logic as `next`.
- The `wayfinder` step drives the wayfinder skill (installed separately), using this task file as its issue tracker; the pi-job skill's Wayfinder section holds the map-to-task-file mapping.
  It loads the map with `wayfinder-context`, spawns as many subagents as needed to resolve unknowns (research the world, prototype to see, grill the user), records scope/architecture resolutions with `add-decision` (not PR/e2e/deploy chatter), and grows the plan with `add-slice`.
  It creates `fog` slices for areas still too foggy or decidable only after other work, and implement/research/spike slices for work now clear.
- A `fog` slice (`clarify-scope → wayfinder → plan-slices`) is a deferred decision-branch, scheduled by `depends_on` so it is charted only once its prerequisites land.
  Its `wayfinder` step recurses, so charting one area can spawn further fog slices for its sub-fog.
- `grill` sharpens what the user already knows; `wayfinder` charts what nobody knows yet and schedules a resolver for each unknown.
  Grill is one tool wayfinder dispatches, alongside research, prototype, and task.

## Syncing recorded state with reality: sync

- `pi-job --task <t> sync [--status s1,s2]` - print a structured pipeline of slices worth re-verifying: by default, any `in_progress`/`blocked` slice, or any slice carrying an open PR; `--status` overrides the selection.
- `pi-job` never spawns agents - `sync` only enumerates and prints instructions.
  The orchestrator dispatches subagents per listed slice, then runs the pipeline:
  1. checklist - verify PR/merge state and whether the recorded step/slice status still matches reality
  2. verify - `gh pr view <url>` + optional `git merge-base --is-ancestor <sha> main`
  3. `add-pr --status merged|closed` if the PR state changed
  4. `finish --note '<append-style evidence>'` to record what was found
  5. `advance` to move past the verified step
  6. Jira ticket status update if applicable

## Repo work: worktrees and PRs

- `pi-job --task <t> set-worktree --slice K --repo R --path P` - record/update the filesystem worktree path for a slice's repo work (upsert; not filesystem-validated).
- `pi-job --task <t> set-worktree --slice K --repo R --clear` - remove the recorded worktree path for an **existing** repo entry; PR records are unchanged; fails if the repo entry was never created.
- `--path` and `--clear` are mutually exclusive; exactly one is required.
- `pi-job --task <t> add-pr --slice K --repo R --url U --status open|merged|closed [--note N]` - record a PR for a slice's repo work, upserting by URL.
- `pi-job --task <t> show [--all]` - also renders each slice's `repo_work`: worktree path (or "not set") and each PR's status/url.

## Task storage backends

`--task` accepts `.yaml`, `.yml`, legacy `.cue`, or an existing directory.
`open_task_store()` selects the backend from that shape; no separate storage flag is needed.

- **`YamlTaskStore`** (default) - a strictly validated, deterministically serialized YAML document.
  Mutations hold an advisory lock across load, validation, mutation, and atomic replacement.
  The lock file lives under `$XDG_CACHE_HOME/pi-job/locks/` (default `~/.cache`), keyed by a hash of the resolved task path, so task directories stay free of sibling `.*.yaml.lock` sentinels.
  Atomic replacement preserves the task file's existing permission mode.
- **`CueTaskStore`** (deprecated) - a compatibility backend for existing CUE files.
  Reads and writes remain available, and each invocation prints migration guidance.
- **`FsTaskStore`** (experimental) - a directory-backed backend.
  `task.title`/`task.status`/etc become files; `task.plan.slices[]` become subdirectories; `depends_on` becomes a directory of symlinks.
  Ordered collections use gapped numeric-prefix directory names (`0010-`, `0020-`, …) so inserts never require renaming siblings.

All backends implement the same `TaskStore` protocol.
Task data from every backend passes through the documented Pydantic task contract.

`scaffold` supports YAML and legacy CUE task paths.
`migrate-task` remains a CUE-only schema-diagnosis command.

## Converting between backends: project

- `pi-job --task <source> project --to <dest>` - copies a task's full state into another backend.
- A new `.yaml` or `.yml` destination is published with atomic no-clobber semantics and verified against the source's canonical Pydantic representation.
- Existing YAML destinations are never overwritten.
- `<dest>` as a `.cue` path is bootstrapped from an empty skeleton if it doesn't exist yet.
  If `<dest>` already has slices or decisions, `project` refuses rather than risk shifting existing entries.
- `<dest>` as a directory is created if missing.

## Slice kinds (contract reference)

| Kind | Role |
|---|---|
| `setup` | Explore, clarify, grill scope, chart the fog, select toolbelt, plan implement slices - typically once, first |
| `implement` | One atomic repo-scoped change: plan, grill plan, build, verify, ship, wait for feedback |
| `closing` | Cross-slice bookkeeping once implement slices are done - typically once, last |
| `research` | Investigation without code changes |
| `spike` | Time-boxed prototype; create-plan/grill-plan apply like implement |
| `fog` | Deferred decision-branch charted later via `depends_on`; records decisions and spawns the slices to resolve a foggy sub-area (no code changes) |
| `follow-work` | Observe a peer's Jira item until landing; capture understanding; spawn or decline follow-ups (no code changes) |

Machine-readable templates and policies live under `slice_kinds` and `step_kinds` in `profile.yaml`.

## Example task file shape

Illustrative only - types and structure, not a real work file:

```yaml
# Managed by pi-job. Prefer pi-job commands over manual edits.
title: Example bounded change
status: in_progress
source:
  jira: ""
  discovered: "2026-01-01"
  context: Short discovery note for why this task exists.
project:
  key: example
  name: Example Project
  route: projects/example/workflow.md
  context: Where this work lives in the repository.
context: Free-form background the agent should read before acting.
orchestration:
  cursor:
    slice: wire-api
    step: grill-plan
  policy:
    coding_execution:
      subagent_required: true
      lower_power_model_preferred: true
      orchestrator_reviews_subagent: true
      exceptions: []
  artifacts: {}
decisions: []
plan:
  note: High-level plan note.
  slices:
    - key: wire-api
      kind: implement
      title: Wire API endpoint
      goal: Return expiration in the status response.
      status: in_progress
      note: ""
      repos: [graphius]
      depends_on: []
      repo_work: {}
      steps:
        - key: create-plan
          title: Create plan
          status: done
          note: "Plan file: task.plans/wire-api.md"
        - key: grill-plan
          title: Grill the plan file
          status: in_progress
          note: ""
      final_steps: []
```

### Task field contract

Every field below has a corresponding Pydantic `Field` description in `bin/pi-job`.
Models use strict types and reject unknown fields.

| Path | Type | Meaning |
|---|---|---|
| `title` | string | Human-readable task title. |
| `status` | status enum | Overall task lifecycle state. |
| `source` | object | Jira reference, discovery identifier, and discovery context. |
| `project` | object | Stable project key, name, workflow route, and project context. |
| `context` | string | Free-form background required before acting. |
| `orchestration` | object or null | Saved cursor, persisted execution policy, and artifacts. |
| `decisions[]` | decision | Date, product/scope rationale, and source for a binding decision (not step evidence). |
| `plan.note` | string | High-level plan context. |
| `plan.slices[]` | slice | Ordered atomic delivery units. |
| `slice.key` | string | Stable identity used by dependencies and the cursor. |
| `slice.kind` | string | Key looked up in `profile.yaml.slice_kinds`. |
| `slice.title` / `goal` | string | Human name and bounded completion outcome. |
| `slice.status` / `note` | status enum / string | Lifecycle state and recorded evidence. |
| `slice.execution` | execution or null | Slice-level executor model and UTC timestamps. |
| `slice.repos[]` | string list | Repositories changed by the slice. |
| `slice.depends_on[]` | string list | Slice keys that must finish first. |
| `slice.repo_work` | repository map | Worktree and pull-request state keyed by repository. |
| `slice.steps[]` | step list | Ordered primary work. |
| `slice.final_steps[]` | step list | Ordered terminal or cleanup work. |
| `step.key` / `title` | string | Stable step identity and human name. |
| `step.status` / `note` | status enum / string | Lifecycle state and evidence. |
| `step.execution` | execution or null | Executor model, start timestamp, and optional end timestamp. |

The task status enum is `planned`, `in_progress`, `blocked`, `done`, or `skipped`.
Pull-request status is `open`, `merged`, or `closed`.
Profile models document configuration layering, artifact rules and gates, toolbelt aids, step kinds, slice policies, and slice kinds in the same way.

What `pi-job` cares about most:

- `orchestration` - must exist after `init`; holds cursor, policy, and artifacts
- `orchestration.cursor` - saved resume point `{slice, step}` only
- `plan.slices[].kind` - selects slice-kind policies and explains step templates
- `plan.slices[].steps` plus `final_steps` - what `next` and `advance` walk
- `decisions` and `orchestration.artifacts` - durable notes and artifact gates

## Test

Behavior tests use YAML task fixtures; legacy CUE coverage lives only in explicitly named compatibility tests.

When contributing Python changes under this package, also run:

```bash
uvx ruff@latest check .
```

`pyproject.toml` configures ruff so extensionless chezmoi scripts (`bin/executable_pi-job`, `tests/executable_test_pi_job.py`) are included.

```bash
# from this package directory (chezmoi source):
uv run --with pydantic --with pyyaml python tests/executable_test_pi_job.py
# installed copy may name this tests/test_pi_job.py:
uv run --with pydantic --with pyyaml python tests/test_pi_job.py
uv run --script bin/executable_pi-job --task /tmp/example.yaml --help
```
