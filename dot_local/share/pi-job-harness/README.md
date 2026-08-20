# pi-job-harness

Portable deterministic job harness for machine-owned YAML task files.

`pi-job` keeps durable task state in exactly one `TaskStore` backend, with YAML as the default.
It validates task state and the package-local `profile.yaml` through documented Pydantic models, walks unfinished steps within the current slice (or jumps with an explicit cursor), and emits deterministic instruction packets.

pi-job started with CUE task files.
The main pain was updating CUE from pi-job through fragile regex rewrites, so it now uses YAML, an optional directory store, and Pydantic.

## Contents

```text
bin/pi-job              CLI entrypoint
profile.yaml            validated step kinds, slice kinds, toolbelt, and artifact rules
pyproject.toml          ruff config (includes extensionless chezmoi scripts)
tests/executable_test_pi_job.py   regression tests (may install as tests/test_pi_job.py)
```

## Dependencies

- Python 3.12+
- `uv`, which resolves the script's PEP 723 dependencies

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

- `list` - show readable, activity-sorted task blocks from the central task home (slug, title, status, updated time, active claims); no `--task` needed
- `archive` - move a home task bundle into the archive home (`$PI_JOB_ARCHIVE` or sibling `archive/`); frees the slug for a fresh `create`
- `create` - create and initialize a task file (`--from` intent YAML, or `--kind`/`--empty-plan` skeleton; also finishes init on an existing uninitialized file)
- `add-slice` / `remove-slice` - add or remove ordered slices with steps from the profile template
- `add-step` - append a step to a slice
- `set-project` / `set-context` / `set-plan-note` / `add-decision` / `set-slice` / `block-slice` / `unblock-slice` / `acknowledge-edit` - write task metadata and product/scope decisions without hand-editing the store
- `status` / `plan` / `show` - report where the work is, the Ready frontier, and slice detail
  (`status` also reports `Structure: ok` or a non-fatal `Structure: invalid` line from slice template lint; warns on oversized notes / large files)
- `show` / `show --slice KEY` / `show --full` / `show --short` / `show --work-first` / `show --graph` - tree view (compact by default), optional models, collapsed consecutive done names, work-first reorder (open on top newest-touched first; done/skipped last newest-completed first), Mermaid depends_on graph for termaid stdin, or a slice-local detail view (goal, notes, steps, repo_work)
- `markdown` / `markdown --chronological` / `markdown --summary` / `markdown --slice KEY` - read-only Markdown preview on stdout (works without orchestration init; never mutates the store)
- `loop` - print the manager fleet heartbeat from `profile.yaml` as one line (no `--task`; agents arm their own `/loop`); `loop --worker` prints `slice_worker_boot` for a spawned window
- `instruction` - emit a deterministic packet for the claim's derived active step (or pick-next when the claimed slice is exhausted)
- `claim` / `release` - take or drop an owned claim on a Ready slice (`orchestration.cursors[]`)
- `start` / `finish` - record the executing model and UTC timestamps while transitioning slice/step status (`finish --note` appends by default; `--replace` overwrites; `finish --slice-only` auto-releases when the slice is terminal)
- `advance` - **deprecated**; always fails with claim/instruction guidance (position is claim + derived step)
- `profile` / `schema` / `kinds` - inspect the active execution profile, task document schema, and slice kinds
- `toolbelt` - list or register planning aids
- `maintain` - list or register surfaces the orchestrator must keep current (`uri` + `note`)
- `sync` - print last-recorded slices to re-verify; orchestrator must run live checks (sync never calls gh/Jira)
- `set-worktree` / `add-pr` - manage worktree paths and pull request records
- YAML writes store a semantic `orchestration.content_digest`; hand-edits produce a loud warning until `acknowledge-edit --reason`

Same task state should yield the same instruction for the same claim/owner, independent of which model is orchestrating.

## Orchestrator loop

Assumption: a smart orchestrator model keeps calling `pi-job` instead of freelancing from chat memory.

**Role:** while a task file is active, the agent is the **orchestrator** (CLI-only for the store; pause on grill/clarify).
This supersedes any default workspace role such as Product Owner.

### Classic single-session loop

1. `pi-job --task <slug> status` (and usually `plan` / `show`)
2. `pi-job --task <slug> claim --slice KEY --owner ID` (Ready slice; one claim per owner)
3. `pi-job --task <slug> instruction` (derived active step, or pick-next when exhausted)
4. `pi-job --task <slug> start --model <provider/model>`
5. Do that step in the orchestrator session, or launch a subagent when the packet says so
6. Record evidence / decisions / blockers, then run `finish [--note ...]`
7. Repeat from `instruction`; on pick-next: `finish --slice-only` → `show` → claim next Ready → `instruction`

### Fleet mode (manager + slice workers)

Two loops, two packets (no tmux spawn code in the harness):

- **Manager:** run `pi-job loop` and arm `/loop` from that text. Watch Ready slices, keep a tmux session of worker windows, spawn/recover windows, inject worker boot. Do not execute slice steps in the manager session.
- **Slice worker:** each window starts from `pi-job loop --worker` (replace literal `OWNER` / `SLICE` / `TASK`). Bound to one owner and one slice. On slice exhaustion: `finish --slice-only` then stop. Do not pick-next or claim other slices.

Classic `instruction` → pick-next stays valid when no fleet is in use.

During every step, capture discovered future work, revisitable issues, technical debt, and unresolved doubts as explicit bounded slices with the appropriate kind and dependencies.
Do not leave actionable follow-up work only in notes.

`pi-job` answers "who holds which claim?" and "how should this step run?" when asked.
Across slices it lists Ready candidates; agents claim independently (parallel owners allowed).
The orchestrator owns model choice, tool use, and whether to keep consulting the harness.

## Picture

```text
  human / capture
        |
        v
  YAML task file <----------------------------- claim/release/finish (atomic rewrite)
  (concrete work: slices, steps, status, evidence, cursors[{owner,slice,…}])
        |
        | strict Pydantic validation
        v
     pi-job  ---- loads ----> profile.yaml
        |                     (step_kinds, slice_kinds, toolbelt, artifact_rules)
        |
        +-- status / plan / show
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

The task file stores **concrete work**: slice keys, titles, goals, per-step status and notes, decisions, artifacts, repo work, and owned claims.
The contract file stores **reusable live meaning**: step owners, guidance, validators, artifact gates, slice-kind policies, and default step templates for new slices.
Old tasks pick up contract updates without rewriting step bodies because metadata is looked up by step key at runtime.

```text
TASK FILE (durable state)                    CONTRACT (profile.yaml)
═════════════════════════                    ═════════════════════════════════

task.plan.slices[]                             slice_kinds: setup | implement | closing
  key, title, goal, status, note, execution      | research | spike
  kind  ───────────────────────────────►         policies, step_template (for add-slice / create)
  depends_on[]  ◄── guard: skip until deps done
  steps[] + final_steps[]                      step_kinds: keyed catalog
    key, title, status, note, execution           owner, guidance, validators,
    key ───────────────────────────────►         artifact_gates, skip_rule
  repo_work, decisions, artifacts

task.orchestration.cursors[]                   toolbelt: aids keyed by suits: [slice kinds]
  owner, slice, claimed_at, last_seen
  (active step = first non-terminal in slice)

WALK ORDER
══════════
Within a claimed slice: derived active step is first non-terminal in steps[] then final_steps[].
Across slices: agents claim among Ready slices (depends_on satisfied, unfinished, not blocked)
via `pi-job show` then `claim --slice/--owner`. Array order of plan.slices is not execution order.
When the claimed slice has no unfinished steps, instruction injects a pick-next packet;
`finish --slice-only` auto-releases the claim.
Closing work is a closing slice in the plan, not a post-slice phase tail.
When every slice is done/skipped, pick-next reports done.
```

Typical slice layout for an end-to-end implementation task:

```text
1. task-setup          [kind: setup]     explore → clarify → grill → wayfinder → select-toolbelt → plan-slices
2. wire-api            [kind: implement] create-plan → grill-plan → edit-code → verify → vulnerability-scan → … → pi-job-feedback → wait-for-feedback → e2e-evidence → ready-for-release
3. fix-follow-up       [kind: implement] …
4. task-closing        [kind: closing]   update-test-plan → update-docs → capture-metrics → update-task-file
```

Claim example: `{owner: "agent-a", slice: "wire-api", …}` with derived step `grill-plan`.

### Custom step keys

Steps may use keys not listed in `step_kinds`.
They remain valid in the task file and in cursor walks.
`instruction` defaults owner to `orchestrator` and omits step-kind guidance, validators, and artifact gates.
Add a contract entry when a step key should carry shared enforcement text.

### Guards vs validators

**Guards** are code paths that can refuse a command:

1. Claim eligibility (`is_actionable` + non-stale occupancy) - `claim` refuses non-Ready or fresh-foreign claims
2. `dependency_satisfied()` / `is_actionable()` - Ready frontier skips slices whose `depends_on` are not done/skipped
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
- The harness is deterministic and fail-closed: missing or uninitialized task → `create`; then `plan` / `instruction` / `advance`.
- `pi-job` emits instruction packets.
  It does not spawn agents.
  The orchestrator chooses models and launches subagents.
- Session todos should track the slice/step plan from `plan`, not a separate profile phase list.
- Prefer small context: `status` / `show --slice` / `markdown --slice` / `instruction` over loading the whole task file.
  Token smell: if a step needs a huge dump, shrink the contract or the slice.
- Sibling slice plans are succinct constraint-and-behaviour contracts (brief, intent, types and composition, call stacks, behaviour, constraints, verification).
  Persist product/scope/architecture/policy agreements with `add-decision` (and/or the grilled plan), not only in chat.
  Step evidence belongs in `finish --note`, not `add-decision`.
  Full wording lives in profile `plan_and_grill_guardrail` (thin pointer only here).
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
3. When a user names a task (slug or path), run `pi-job --task <slug-or-path> status` first.
   Prefer `pi-job list` to discover home slugs.
4. If the file is missing (or uninitialized), use `create` (`--from` intent YAML, or `--kind setup`).
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
curl -fsSL "$BASE/README.md" -o ~/.local/share/pi-job-harness/README.md
chmod +x ~/.local/share/pi-job-harness/bin/pi-job
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'exec uv run --script "$HOME/.local/share/pi-job-harness/bin/pi-job" "$@"' \
  > ~/.local/bin/pi-job
chmod +x ~/.local/bin/pi-job
# requires: uv with Python 3.12
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
# From an intent document (create + initialize in one transaction)
pi-job --task projects/example/tasks/task.yaml create --from task.intent.yaml

# Seed a kind and initialize in one shot
pi-job --task projects/example/tasks/task.yaml create --kind setup

# Empty plan only (add slices, then re-run create to initialize)
pi-job --task projects/example/tasks/task.yaml create --empty-plan
# ... edit plan.slices ...
pi-job --task projects/example/tasks/task.yaml create

pi-job --task projects/example/tasks/task.yaml status
pi-job --task projects/example/tasks/task.yaml validate
pi-job --task projects/example/tasks/task.yaml plan
pi-job --task projects/example/tasks/task.yaml show
pi-job --task projects/example/tasks/task.yaml instruction
pi-job --task projects/example/tasks/task.yaml start --model openai/gpt-5.6-sol
pi-job --task projects/example/tasks/task.yaml finish --note "Verification evidence recorded."
pi-job --task projects/example/tasks/task.yaml advance
```

If `--task` points at a missing file, commands fail closed and tell the agent how to `create` one.
A task without `task.orchestration` is not initialized; run `create` (with `--kind` if needed) before `plan`, `show`, `advance`, or `instruction`.

### validate

- `pi-job --task <t> validate` is the canonical way to check a task file.
- `pi-job --task <t> validate --slice KEY` checks only that slice against the active profile (kind plus required/template steps).
  On success, if other slices still have structure issues, pi-job prints a non-fatal note (`full-task: N legacy structure issue(s); use validate without --slice`) and still exits 0.
  Unknown slice keys fail closed with the known slice keys listed.
- YAML syntax is loaded with duplicate-key detection.
- Task and profile fields are checked through strict Pydantic models that reject unknown fields.
- Slice structure and live profile-template requirements are checked after document validation.
- `validate` and `status` warn (non-fatal) when slice or step notes exceed ~2000 characters or the task file exceeds ~100KB; keep long prose in slice plan files.

### create

`create` is the only way to bring a task file into existence and/or initialize orchestration.

```bash
# Intent document
pi-job --task projects/example/tasks/task.yaml create --from task.intent.yaml
pi-job --task projects/example/tasks/task.yaml create --from task.intent.yaml --dry-run
pi-job --task projects/example/tasks/task.yaml create --from task.intent.yaml --force

# Skeleton + initialize in one shot
pi-job --task projects/example/tasks/task.yaml create --kind setup
pi-job --task projects/example/tasks/task.yaml create   # default implement example

# Empty plan (no orchestration yet)
pi-job --task projects/example/tasks/task.yaml create --empty-plan
# ... add-slice ...
pi-job --task projects/example/tasks/task.yaml create

# Existing uninitialized file: finish init in place
pi-job --task projects/example/tasks/task.yaml create [--kind setup]
```

Intent document shape (`--from`):

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
- `--dry-run` prints the would-be YAML (or a unified diff for `--from`) without writing.
- Writes through a temporary file and atomic rename.
- Reads back and revalidates the written result; aborts on semantic mismatch.
- Returns the initialized cursor and the full instruction packet for the orchestrator to act on.
- When the plan introduces implement or spike slices (kinds whose template includes `create-plan`), prints a trailing `SEED SLICE PLAN FILES NOW` reminder.

### add-slice

- `add-slice --kind K --key … --title … --goal …` is required for every new slice; steps are filled from the template.
- After a successful non-dry-run `add-slice` for a qualifying kind, prints the same seed reminder for that slice only.

### Task mutation commands

These commands write task metadata and durable state without editing the YAML by hand:

- `pi-job --task <t> set-project --title T --key K --name N --route R --context C` - update `task.title` and/or merge into `task.project` (at least one flag required; `--title` must be non-empty).
- `pi-job --task <t> set-context --context TEXT` or `--file PATH` - replace `task.context`.
- `pi-job --task <t> add-decision --date YYYY-MM-DD --note RATIONALE --source ORIGIN` - append a product/scope decision (not step evidence; use `finish --note`; date defaults to today UTC; source defaults to `pi-job add-decision`).
- `pi-job --task <t> set-plan-note --note TEXT` - set `task.plan.note`.
- `pi-job --task <t> acknowledge-edit --reason R` - refresh `orchestration.content_digest` after a legitimate hand-edit and append the reason to the current cursor slice note (YAML only; not a decision).
- `pi-job --task <t> set-slice --key K [--title T] [--goal G]` - update slice metadata (at least one of `--title` or `--goal` required; YAML only; refuses `done`/`skipped` slices).
- `pi-job --task <t> block-slice --key K --reason R` - mark a slice `blocked` and append the reason to its note (YAML only; refuses `done`/`skipped`; re-block appends again).
- `pi-job --task <t> unblock-slice --key K` - restore a `blocked` slice to `planned` without changing its note (YAML only).
- `pi-job --task <t> remove-slice --key K` - remove a slice from the plan. Refuses when:
  - another slice declares a `depends_on` reference to it
  - the orchestration cursor points at it (advance to another slice first)

### Introspection

These commands do not require `--task`.

- `pi-job list` - one readable block per task bundle under `$PI_JOB_TASKS` (default `~/.local/share/pi-job/tasks`): slug, title, derived status, updated time, and active claim labels.
  Status groups are ordered `in_progress`, `blocked`, `planned`, `skipped`, then `done`.
  Within each group, newest activity appears first.
  Activity is the newest valid cursor `last_seen`, falling back to the `task.yaml` modification time when no cursor heartbeat is available.
  Exact ties use slug order.
  Ready frontier remains on `status` / `show`.
  Scans only immediate child bundle directories with a `task.yaml`; loose `*.yaml` files directly under the task home are never listed (`project` them into a bundle first).
  A bundle that fails to load is skipped with a stderr warning instead of aborting the whole listing.
- `pi-job --task <slug> archive [--to SLUG] [--dry-run]` - move a home bundle out of `$PI_JOB_TASKS` into the archive home.
  Archive home is `$PI_JOB_ARCHIVE` when set, otherwise `<parent of $PI_JOB_TASKS>/archive` (default `~/.local/share/pi-job/archive`).
  Only immediate children of the task home archive; loose YAML and path-opened bundles outside the home are refused.
  Destination keeps the slug unless `--to` renames it; existing destinations fail closed (no overwrite).
  After archive, `list` no longer shows the task and the slug is free for `create`.
- `pi-job profile [--json]` - show the active execution profile. Human output lists slice/step/toolbelt counts; `--json` dumps the full validated profile.
- `pi-job schema [--json]` - show the task document and create-intent input schemas. Human output summarizes model counts; `--json` dumps a complete JSON Schema object with `task` and `create` keys.
- `pi-job kinds list [--json]` - list all slice kinds with their step templates. `--json` dumps the full slice_kinds catalog.
- `pi-job kinds show <kind> [--json]` - show one slice kind's details including expanded step entries (title, owner). `--json` adds resolved step metadata.

### claim / release

- `claim --slice KEY --owner ID` takes a Ready slice (deps satisfied, not terminal) with no active non-stale claim.
- One claim per owner; stale foreign claims may be displaced (default TTL 24h via `orchestration_defaults.claim_stale_after_hours`).
- `release --owner ID` drops any claim (not self-only); mid-slice release leaves slice status unchanged.
- `finish --slice-only` to a terminal slice status auto-releases the claim on that slice.
- Owner may also come from `$PI_JOB_OWNER`; omit when there is exactly one active claim.

### advance (deprecated)

- Always fails with guidance to `claim` / `instruction` / `finish --slice-only`.
- Position is the owned claim plus the slice's first non-terminal step; there is no stored step cursor to move.

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
- With no target flags, lifecycle commands operate on the claim's derived active step (`--owner` / `$PI_JOB_OWNER` / sole claim).
- `--slice-only` targets the claimed (or explicit `--slice`) slice; `--slice K` targets another slice; add `--step K` to target one of its steps.
- Start the slice with the orchestrator model, then start and finish each step with the model that directly performs it.
- `finish --skip --model <id> --reason '<why>'` records an atomic skip when no prior `start` exists.
- `finish --note '<evidence>'` appends completion evidence with a blank line when a note already exists; omitted preserves the existing note.
- `finish --replace --note '<evidence>'` overwrites the existing note instead of appending (`--replace` requires `--note` and cannot combine with `--skip`).
- `finish --reconcile --model <id> --note '<evidence>'` records completion for an `in_progress` target that was never started via pi-job; refuses `planned`/`done`/`skipped`.
- When more than one unfinished step exists anywhere in the task, finishing a step requires explicit `--slice` and `--step` (bare claim finish is allowed only when exactly one unfinished step remains).
- Normal `finish` without `--reconcile` still requires a prior `start` (unless `--skip`).
- `start` refuses `blocked` slices and blocked lifecycle targets; run `unblock-slice` first for slice-level blocks.
- Existing tasks without execution metadata remain readable; `validate` reports warnings instead of inventing historical data.
- Slice kinds may declare `required_steps` separately from `step_template`: persisted slices must satisfy the stable structural minimum, while later template additions produce migration warnings instead of invalidating old tasks.

### Independent vulnerability scan

Every new implement slice includes `vulnerability-scan` after verify and before sharing.
Acceptance `e2e-evidence` is skippable and runs after `wait-for-feedback`, immediately before `ready-for-release`.

1. The orchestrator asks the user whether the scan is required for that slice.
2. If accepted, the orchestrator selects a scanner model whose fully qualified ID differs from `edit-code.execution.model`, and prefers a higher-reasoning / higher-capability model than the code author (not a second fast coding model).
3. The scanner reviews changed/generated code for vulnerabilities and records findings in the step note.
4. The step finishes only after findings are resolved or the remaining risk is explicitly accepted.
5. If the user declines, the orchestrator records `finish --skip --model <id> --reason '<user decision>'`.

`start` rejects the code-author model for this step, and `advance` rechecks recorded provenance so externally modified status cannot bypass the model-separation rule.
The CLI does not recognize `vulnerability-scan` by name; it applies the generic `requires_user_decision` and `different_model_from_step` fields declared on any step kind.

### Migrating from v1 profile/phase model

v1 stored `task.orchestration.profile`, `cursor.phase`, and walked post-slice profile phases.
v2 replaces profiles with slice kinds on each slice and a two-field cursor only.
Remove `profile` and `phase` from task files; add `kind` to each slice; express setup and closing work as setup/closing slices in `plan.slices`.
See `projects/pi-agent-job-harness/workflow.md` in the weight-loss repo for the decision summary.

## Toolbelt and visualization

- `pi-job --task <t> layers [show|add|set|remove|rename|reorder]` - manage ordered `task.layers` bands (`name`, `description`, `references`).
  When non-empty, implement/spike/research slices need exactly one `--layer`.
  `layers add` creates `references/bigpicture.txt` stub when missing (shape contract + fictional spine example + per-layer TODOs); later edits print a slice survival report (agent updates the bigpicture call spine).
  Setup selects layers for the complete current journey, including unchanged or idle systems that explain a handoff.
- `pi-job --task <t> toolbelt` - list planning aids whose `suits` includes a slice kind present on the task (or pass `--kind K` to filter).
- `pi-job --task <t> toolbelt add <key> [--path P] [--status S] [--note N]` - register/update a planning aid as an `#Artifact` under `task.orchestration.artifacts` (idempotent; validates `<key>` against the catalog).
  Artifact statuses are `planned`, `in_progress`, `blocked`, `done`, and `skipped`.
  Keep aid files, PR bodies, and Jira comments current via `maintain`, not artifact status.
- `pi-job --task <t> maintain [add|remove] [--uri U] [--note N]` - list or upsert `orchestration.maintain[]` (`{uri, note}`).
  `uri` is a path, PR URL, or ticket URL. `note` says what current means and when to update.
  Plan and instruction packets print this list on every step.
  Aid `bigpicture` is the cross-layer call stacktrace (distinct from `sequence-diagram`).
  Aid `domain-vocabulary` is the task glossary at `references/glossary.yaml` (machine-readable; grow from research and grill).
  Aid `decision-review-deck` is the async decision deck (skill `decision-review-deck`; dated project markdown).
- `pi-job --task <t> files [--relative]` - print artifact paths one per line (absolute by default): everything under `references/` and `plans/`, plus registered `orchestration.artifacts` paths (including files outside the bundle).
  With `--relative`, in-bundle paths are bundle-relative; out-of-bundle registered paths stay absolute.
  Pipe to `grep`, `fzf`, or `$EDITOR "$(...)"`.
- `pi-job --task <t> show [--all] [--started] [--full] [--short] [--status s1,s2] [--color auto|always|never]` - render the task as a cursor-focused slice/step tree with a toolbelt footer.
  `--short` collapses consecutive `done` slices onto one line (`✓ a, b, c`); skipped breaks the run; ignored with `--all`.
  By default only the current cursor slice expands.
  `done`/`skipped` slices are completely header-only (no deps, repo_work, or steps) and omit `[kind/n/m]` (footer still has totals), except with `--status`: set `repo_work.worktree` paths are printed so agents can inventory recorded worktrees without `--all`.
  Executor models are omitted unless `--full`.
  `--started` additionally expands `in_progress`/`blocked` slices.
  `--all` expands every slice including finished ones.
  `--status` filters which slices are listed (and surfaces set worktrees on done/skipped; see above).
  `--color` tints status glyphs for humans (`✓` green, `✗` red, `▸` cyan, `⊘` yellow, `○`/`·` dim); default `auto` (TTY only, respects `NO_COLOR`).
- `pi-job --task <t> show --graph [--by-layer] [--status s1,s2]` - emit a Mermaid `flowchart TD` of slice `depends_on` edges on stdout (no tree chrome).
  Intended for terminal viewers via stdin, e.g. `pi-job --task <t> show --graph | uvx termaid`.
  `classDef` colors: green `done`, blue `in_progress` (and the non-done cursor slice), gray `planned`, red `blocked`, yellow `skipped`; unknown dep keys are orange `missing`.
  `--by-layer` groups nodes into subgraphs by `task.layers` order; unlayered kinds sit outside.
  Mutually exclusive with `--slice`; tree flags are ignored.
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

The setup slice builds understanding before it asks the user to clarify or grill scope:

1. `confirm-layers` lists all catalog bands and confirms the complete current-journey stack.
2. `select-toolbelt` recommends aids, separates current-state aids from planning-only aids, and confirms the selection with the user.
3. `map-current-state` always fills the AS-IS bigpicture when layers exist and produces the other selected understanding aids.
4. `clarify-scope` and `grill` ask decisions grounded in those aids, not factual questions that repository evidence can answer.
5. `plan-slices` produces planning-only aids and can add clearly labelled TO-BE content without replacing the AS-IS baseline.

The catalog lives in `profile.yaml` under `toolbelt`.
Catalog entries may include an `example` build instruction; follow it when writing the aid file.

## Planning before code changes: create-plan / grill-plan / grill

**Setup slice** maps current behaviour before step `grill` interrogates overall task scope.

**Implement and spike slices** must lead with `create-plan` then `grill-plan` before other work in that slice.
Sibling plan files are succinct constraint-and-behaviour contracts
(brief, intent, types and composition, call stacks, behaviour, constraints, verification).
Full wording (required sections, grill axes, task-store boundary, naming, skip exception) lives in `plan_and_grill_guardrail` and the create-plan / grill-plan step guidance in `profile.yaml`.
Do not restate that contract here.

## Charting foggy work: wayfinder

When a task is too big and foggy to plan in one setup pass, the `setup` slice's `wayfinder` step (and any `fog` slice) charts the way to the destination one decision at a time, instead of forcing implement slices up front.
The map is the task file itself: `decisions` and slices, readable by any later session.

- `pi-job --task <t> wayfinder-context` - print the map reconstructed from the task file at the slice level (no step noise): the `DESTINATION` (`plan.note`), recorded `DECISIONS`, `IN PROGRESS / DONE` slices, the `FRONTIER` (planned slices whose dependencies are satisfied), and the `FOG` (planned slices still blocked, with their unmet dependencies).
  Read-only; reuses the same `is_actionable` logic as the Ready frontier.
- The `wayfinder` step drives the wayfinder skill (installed separately), using this task file as its issue tracker; the pi-job skill's Wayfinder section holds the map-to-task-file mapping.
  It loads the map with `wayfinder-context`, spawns as many subagents as needed to resolve unknowns (research the world, prototype to see, grill the user), records scope/architecture resolutions with `add-decision` (not PR/e2e/deploy chatter), and grows the plan with `add-slice`.
  It creates `fog` slices for areas still too foggy or decidable only after other work, and implement/research/spike slices for work now clear.
- A `fog` slice (`clarify-scope → wayfinder → plan-slices`) is a deferred decision-branch, scheduled by `depends_on` so it is charted only once its prerequisites land.
  Its `wayfinder` step recurses, so charting one area can spawn further fog slices for its sub-fog.
- `grill` sharpens what the user already knows; `wayfinder` charts what nobody knows yet and schedules a resolver for each unknown.
  Grill is one tool wayfinder dispatches, alongside research, prototype, and task.

## Syncing recorded state with reality: sync

- `pi-job --task <t> sync [--status s1,s2]` - print a structured pipeline of slices worth re-verifying: by default, any `in_progress`/`blocked` slice, or any slice carrying an open PR; `--status` overrides the selection.
- `sync` is a pure task-file read: it never calls GitHub or Jira.
  The printed list is last-recorded state, not live remote status.
  The orchestrator (or per-slice subagents) must immediately run the pipeline for each listed slice; do not treat the listing alone as current status:
  1. checklist - verify PR/merge state and whether the recorded step/slice status still matches reality
  2. verify - `gh pr view <url>` + optional `git merge-base --is-ancestor <sha> main`
  3. `add-pr --status merged|closed` if the PR state changed
  4. `finish --note '<append-style evidence>'` to record what was found
  5. `advance` to move past the verified step when the current step is complete
  6. Jira ticket status update if applicable
  Do not report the list to the user as current until step 2 has run for every open PR (and every listed in_progress/blocked slice has been checked).
  Authoritative wording lives in `profile.yaml` `sync_pipeline_instructions` (emitted at the top of `sync` output).
## Repo work: worktrees and PRs

- `pi-job --task <t> set-worktree --slice K --repo R --path P` - record/update the filesystem worktree path for a slice's repo work (upsert; not filesystem-validated).
- `pi-job --task <t> set-worktree --slice K --repo R --clear` - remove the recorded worktree path for an **existing** repo entry; PR records are unchanged; fails if the repo entry was never created.
- `--path` and `--clear` are mutually exclusive.
  Neither is strictly required by argparse: omitting both prints the recommended path (see below) and exits non-zero without writing anything; recording still requires an explicit `--path`.
- Worktree convention: `$PI_JOB_WORKTREES/<slug>/<slice>/<repo>` (default worktree home `~/.local/share/pi-job/worktrees`; `PI_JOB_WORKTREES` overrides).
  The slug segment is the task bundle's directory name; a loose (non-bundle) YAML task has no slug, so the recommendation omits that segment and adds a note about projecting the task into the central home first.
  Recommendations are advisory only - `pi-job` never creates the directory or a git worktree.
- `pi-job --task <t> add-pr --slice K --repo R --url U --status open|merged|closed [--note N]` - record a PR for a slice's repo work, upserting by URL.
- `pi-job --task <t> show [--all]` - also renders each slice's `repo_work`: worktree path (or "not set") and each PR's status/url.
- Agents listing recorded worktrees: `show --status done` (set paths only), or `show --all` / `show --slice KEY` for full `repo_work`.

## Task storage backends

`--task` accepts `.yaml`, `.yml`, or an existing directory.
`open_task_store()` selects the backend from that shape; no separate storage flag is needed.

- **`YamlTaskStore`** (default) - a strictly validated, deterministically serialized YAML document.
  Mutations hold an advisory lock across load, validation, mutation, and atomic replacement.
  The lock file lives under `$XDG_CACHE_HOME/pi-job/locks/` (default `~/.cache`), keyed by a hash of the resolved task path, so task directories stay free of sibling `.*.yaml.lock` sentinels.
  Atomic replacement preserves the task file's existing permission mode.
- **`FsTaskStore`** (experimental) - a directory-backed backend.
  `task.title`/`task.status`/etc become files; `task.plan.slices[]` become subdirectories; `depends_on` becomes a directory of symlinks.
  Ordered collections use gapped numeric-prefix directory names (`0010-`, `0020-`, …) so inserts never require renaming siblings.

All backends implement the same `TaskStore` protocol.
Task data from every backend passes through the documented Pydantic task contract.

`create` supports YAML task paths.

## Migrating loose YAML into a bundle: project

- `pi-job --task <loose-source> project --to <slug-or-bundle-path>` - converts an old loose YAML task (and its sibling `<stem>.plans/` and other artifacts) into a new task bundle.
- Source must be loose `YamlTaskLayout`; bundles and directory stores are refused (nothing to convert).
- `--to` is a task slug (resolved under `$PI_JOB_TASKS`), a bundle directory, or its `task.yaml`; the destination is always a fresh `BundleTaskLayout`, never a loose YAML file or the experimental directory store.
- The destination `task.yaml` must not already exist (no `--force`).
- Document state is copied and verified against the source's canonical Pydantic representation; `<stem>.plans/` merges into `plans/`, other sibling directories are copied to the bundle root under their own name, and sibling files land under `references/`.
- On success, only the source yaml and its `<stem>.plans/` are deleted; other copied sibling dirs/files remain at the old location for manual cleanup.
- Any failure rolls back the freshly scaffolded destination bundle and leaves the source untouched.

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
| `status` | status enum | Persisted overall status (compat only). Display ignores it and derives from slice statuses. |
| `source` | object | Jira reference, discovery identifier, and discovery context. |
| `project` | object | Stable project key, name, workflow route, and project context. |
| `context` | string | Free-form background required before acting. |
| `orchestration` | object or null | Saved cursors, policy, artifacts, maintain list, and content digest. |
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

The status enum is `planned`, `in_progress`, `blocked`, `done`, or `skipped`.
`status` / `list` / `markdown` report overall task status derived from `plan.slices[].status` (blocked > in_progress > terminals > planned); the top-level `status` field is ignored.
Pull-request status is `open`, `merged`, or `closed`.
Profile models document configuration layering, artifact rules and gates, toolbelt aids, step kinds, slice policies, and slice kinds in the same way.

What `pi-job` cares about most:

- `orchestration` - must exist after `create`; holds cursors, policy, artifacts, and maintain
- `orchestration.cursors[]` - owned claims `{owner, slice, claimed_at, last_seen}` (hard cut; no single `cursor`)
- `plan.slices[].kind` - selects slice-kind policies and explains step templates
- `plan.slices[].status` - authority for overall task status in status/list/markdown
- `plan.slices[].steps` plus `final_steps` - sequential work; active step is derived as first non-terminal
- `decisions` and `orchestration.artifacts` - durable notes and artifact gates
- `orchestration.maintain[]` - `{uri, note}` surfaces the orchestrator must keep current

## Agent dev notes

Notes for agents (and humans) changing the harness Python, not for orchestrating tasks.

Prefer this shape when extending interrupt / sidecar / render behaviour.
Put side effects on the edge; keep the middle pure.

### Functional style

Write new `pi-job` Python in a functional style.

- Prefer pure functions that take data in and return new data out.
- Keep transforms immutable: build new mappings, lists, and dataclasses; do not mutate task dicts or slice objects in place outside the store write path.
- Put I/O, clocks, argparse, printing, and process exits only at the edge (`cmd_*`, store open/close, `atomic_write_*`).
- Prefer small named helpers over a static utility class when the surface is a set of transforms (example: layer registry helpers, not a `TaskLayers` class).
- Use a `@dataclass(frozen=True)` (or plain dataclass) for grouped return values when a tuple is unclear.
- Avoid hidden mutable module state for feature logic; cache only for loaders such as `load_profile_contract()`.

### Shape

| Layer | Owns | Does not |
|---|---|---|
| `YamlTaskLayout` / `TaskLayout` | Pure path arithmetic under `<stem>.plans/` and task siblings | Clock, I/O, profile text |
| `YamlTaskStore` (and other stores) | Task YAML + sibling `.plans/` reads/writes under `exclusive()`; atomic replace | Hardcoded instruction/packet bodies |
| `render_*` / packet helpers | Pure strings from task mappings, injected plan bodies, and profile templates | Disk reads, mutations |
| `cmd_*` | Argparse, validate-at-edge, open store, load bytes for render, print | Business mutation logic duplicated outside the store |

### Sibling `.plans/` (YAML)

| Concern | Store / layout API | Notes |
|---|---|---|
| Slice plan stub | `ensure_slice_plan_stub` | Profile `slice_plan_stub`; create-plan kinds only; atomic write under lock |
| Findings log | `add_finding` → `layout.findings_file()` (`_findings.md`) | Append-only; header from `findings_file_header` |
| Long decision spill | `add_decision(..., spill_body=, spill_path=)` | Soft-limit or `--plan-file`; layout path needs caller `stamp` |
| Block + optional gate | `block_slice(..., gate=)` | One mutation (status/note + `depends_on`) |

CLI must not `write_text` these sidecars directly.
Clocks (`utc_now`) and path stamps live in the store/cmd edge, not in layout.

### Profile vs Python

Instruction and coaching bodies live in `profile.yaml` (`instruction_packets`, `cli_help`, `interrupt_park_steps`).
Python loads and formats them; it must not hardcode parallel copy.
Examples: `status_interrupt_hint`, `investigate_interrupt`, `orchestrator_heartbeat`, `slice_worker_boot`, `slice_plan_stub`, `findings_file_header`, `bigpicture_stub`.

### Render

`markdown --slice` is lean by default (slice body + optional injected plan file).
Opt in with `--with-decisions` / `--with-preamble`.
`cmd_markdown` reads plan file bytes at the edge and passes `plan_bodies` / `plan_labels` into pure `render_task_markdown`.

### Class boundaries

Default to pure free functions (see Functional style).
Use a named class only when one object owns a coherent feature surface (formatting, export, layout, policy) with shared construction state.
Keep free functions for thin wiring (`cmd_*`, argparse, store open/close) and for pure transforms.
Example: `SliceDependencyMermaid` owns all Mermaid `depends_on` graph formatting; `show --graph` only constructs it and prints `.render(task)`.
Do not scatter matching helpers (`node_id`, `classDef`, edge assembly) beside unrelated `show` tree code.
Follow the same pattern when adding similar exporters or viewers.

### Residual (acceptable)

- `load_profile_contract()` inside packet/`render_*` helpers (cached; same pattern as the rest of the harness).
- Findings append is read-modify-write under the task lock (required for append-only markdown).
- Nested `exclusive()` reuses `_lock_depth` (spill write + YAML mutate in one outer lock).

### Check

Run `uvx ruff@latest check .` from this package directory after Python edits (see Test below).

## Test

Behavior tests cover YAML task fixtures and the directory-backed store.

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
