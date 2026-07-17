# pi-job-harness

Portable deterministic job harness for CUE task files.

`pi-job` keeps a task's durable state in exactly one `TaskStore` backend - a CUE file by default; see [Task storage backends](#task-storage-backends) for the directory-backed one.
It reads the task via that backend, loads package-local `profile-contract.cue` for `step_kinds` and `slice_kinds`, computes the next unfinished slice/step, updates the orchestration cursor `{slice, step}`, and emits deterministic instruction packets for the orchestrating model.

## Contents

```text
bin/pi-job              CLI entrypoint
profile-contract.cue    step_kinds / slice_kinds contract (filename is historical)
task-schema.cue         shared #Slice / #Step types (unified into every command)
tests/executable_test_pi_job.py   regression tests (may install as tests/test_pi_job.py)
```

## Dependencies

- Python 3.11+
- `cue` CLI on `PATH`

No third-party Python packages are required.

Recommended: use [`uv`](https://docs.astral.sh/uv/) to install and pin the Python version.
`pi-job` itself has no Python package deps, but `uv` keeps a working `python3` available across machines without fighting system Python.

```bash
# install uv: https://docs.astral.sh/uv/getting-started/installation/
uv python install 3.12
uv python pin 3.12   # optional, in a project directory
```

## What pi-job does

`pi-job` is a small CLI that answers orchestration questions from durable state.
It does **not** run the agent session and does **not** spawn subagents.

Given a CUE task file and package-local `profile-contract.cue`, it can:

- `scaffold` - create a missing task file from the generic example shape
- `init` - initialize `task.orchestration` and set the cursor; optionally seed the first slice from a slice kind template
- `add-slice` - append a slice whose steps come from `slice_kinds[K].step_template`
- `status` / `next` / `plan` - report where the work is and what slices/steps remain
- `instruction` - emit a deterministic packet for the current or next cursor (owner, validators, gates, todo reminders)
- `advance` - write the next cursor back into the task file after evidence lands; fails closed if the current step is not `done`/`skipped` unless `--force --reason '<why>'` is given

Same task state should yield the same next action and instruction, independent of which model is orchestrating.

## Orchestrator loop

Assumption: a smart orchestrator model keeps calling `pi-job` instead of freelancing from chat memory.

1. `pi-job --task <file> status` (and usually `plan`)
2. `pi-job --task <file> instruction` (current or next)
3. Do that step in the orchestrator session, or launch a subagent when the packet says so
4. Record evidence / decisions / blockers in the task file
5. `pi-job --task <file> advance`
6. Repeat until `next` is `done`

`pi-job` only answers "what next?" and "how should this step run?" when asked.
The orchestrator owns model choice, tool use, and whether to keep consulting the harness.

## Picture

```text
  human / capture
        |
        v
  CUE task file  <----------------------------- advance (write cursor)
  (concrete work: slices, steps, status, evidence, cursor {slice, step})
        |
        | cue export
        v
     pi-job  ---- loads ----> profile-contract.cue
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
TASK FILE (durable state)                    CONTRACT (profile-contract.cue)
═════════════════════════                    ═════════════════════════════════

task.plan.slices[]                             slice_kinds: setup | implement | closing
  key, title, goal, status, note                 | research | spike
  kind  ───────────────────────────────►         policies, step_template (for add-slice / init)
  depends_on[]  ◄── guard: skip until deps done
  steps[] + final_steps[]                      step_kinds: keyed catalog
    key, title, status, note                     owner, guidance, validators,
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
1. task-setup          [kind: setup]     explore → clarify → grill → select-toolbelt → plan-slices
2. wire-api            [kind: implement] create-plan → grill-plan → edit-code → verify → … → wait-for-feedback
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

**Validators** are descriptive strings on a `#StepKind`.
They appear in `instruction` and `plan` for orchestrator self-check.
`pi-job` does not parse or verify validator strings except where step-order blocking already applies.

## Principles

- Task state lives in exactly one `TaskStore` backend (CUE file by default) - no parallel YAML cursor, no agent memory as state.
- Slice kinds and step kinds are configuration in `profile-contract.cue`, not hardcoded Python.
- The harness is deterministic and fail-closed: missing task → `scaffold`; missing orchestration → `init`; then `plan` / `instruction` / `advance`.
- `pi-job` emits instruction packets.
  It does not spawn agents.
  The orchestrator chooses models and launches subagents.
- Session todos should track the slice/step plan from `plan`, not a separate profile phase list.

## How an agent should know about it

1. Global hint: chezmoi [`AGENTS.md`](../../../AGENTS.md) points agents at `pi-job` for CUE-task orchestration.
2. On PATH after chezmoi apply: `pi-job` -> `~/.local/bin/pi-job`.
3. When a user names a task file, or work clearly belongs in a CUE task, run `pi-job --task <file> status` first.
4. If the file is missing, follow the error to `scaffold`, edit the scaffold, then `init [--kind setup]`.
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
curl -fsSL "$BASE/profile-contract.cue" -o ~/.local/share/pi-job-harness/profile-contract.cue
curl -fsSL "$BASE/task-schema.cue" -o ~/.local/share/pi-job-harness/task-schema.cue
curl -fsSL "$BASE/README.md" -o ~/.local/share/pi-job-harness/README.md
chmod +x ~/.local/share/pi-job-harness/bin/pi-job
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'exec uv run --python 3.12 "$HOME/.local/share/pi-job-harness/bin/pi-job" "$@"' \
  > ~/.local/bin/pi-job
chmod +x ~/.local/bin/pi-job
# requires: uv (with Python 3.12), cue on PATH
```

If you already have a suitable system `python3`, the wrapper can call the script directly instead of `uv run`.

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
# If the task file does not exist yet:
pi-job --task projects/example/tasks/task.cue scaffold
pi-job --task projects/example/tasks/task.cue init --kind setup

pi-job --task projects/example/tasks/task.cue status
pi-job --task projects/example/tasks/task.cue plan
pi-job --task projects/example/tasks/task.cue next
pi-job --task projects/example/tasks/task.cue instruction --current
pi-job --task projects/example/tasks/task.cue advance
```

If `--task` points at a missing file, commands fail closed and tell the agent to run `scaffold`, then edit and `init`.
A task without `task.orchestration` is not initialized; run `init` before `plan`, `next`, `advance`, or `instruction`.

### init and add-slice

- `init [--kind K]` creates `task.orchestration` with cursor at the first actionable slice/step.
  When `plan.slices` is empty and `--kind` is supplied, seeds one slice from `slice_kinds[K].step_template`.
- `add-slice --kind K --key … --title … --goal …` is required for every new slice; steps are filled from the template.

### advance

- Fails closed when the saved cursor's step is not `done`/`skipped`.
- `--force --reason '<why>'` marks the current step skipped before advancing.
- Use `--slice` and `--step` to jump explicitly, or omit both to advance to computed next.

## Migrating legacy task files

If a task file was created before `task-schema.cue` existed, it may have local copies of type declarations (`#Status`, `#Step`, `#Decision`, `#Artifact`, `#Slice`) at the top level.
These are now legacy - the shared `task-schema.cue` is unified into every `pi-job` command automatically (both files are passed together to `cue export`), making local copies redundant and a source of confusion during edits.

`pi-job migrate-task` diagnoses a task file for these legacy declarations and prints safe deletion or refactoring recommendations.
It never modifies the file - you use your normal editor to apply the changes.
After editing, run `pi-job status` and `pi-job show` to confirm the migrated file still validates (these commands automatically load both the task file and shared schema).

Note: a bare `cue vet` or `cue export` invoked on the migrated task file *alone* will fail with missing reference errors like `reference "#Step" not found` - this is expected and not breakage.
Only `pi-job` subcommands (or `cue` explicitly passed both files) validate the migrated file correctly.

### Migrating from v1 profile/phase model

v1 stored `task.orchestration.profile`, `cursor.phase`, and walked post-slice profile phases.
v2 replaces profiles with slice kinds on each slice and a two-field cursor only.
Remove `profile` and `phase` from task files; add `kind` to each slice; express setup and closing work as setup/closing slices in `plan.slices`.
See `projects/pi-agent-job-harness/workflow.md` in the weight-loss repo for the decision summary.

## Toolbelt and visualization

- `pi-job --task <t> toolbelt` - list planning aids whose `suits` includes a slice kind present on the task (or pass `--kind K` to filter).
- `pi-job --task <t> toolbelt add <key> [--path P] [--status S] [--note N]` - register/update a planning aid as an `#Artifact` under `task.orchestration.artifacts` (idempotent; validates `<key>` against the catalog).
- `pi-job --task <t> show [--all] [--started] [--status s1,s2]` - render the task as a cursor-focused slice/step tree with a toolbelt footer.
  By default only the current cursor slice expands.
  `done`/`skipped` slices are completely header-only (no deps, repo_work, or steps).
  `--started` additionally expands `in_progress`/`blocked` slices.
  `--all` expands every slice including finished ones.
  `--status` filters which slices are listed.

The setup slice's `select-toolbelt` step picks aids suited to the task's slice kinds; `plan-slices` produces them.
The catalog lives in `profile-contract.cue#toolbelt`.

## Planning before code changes: create-plan / grill-plan / grill

**Setup slice** uses step key `grill` to interrogate overall task scope before implement slices exist.

**Implement and spike slices** must lead with two steps before other work in that slice:

1. `create-plan` - write the detailed implementation plan for that slice to a sibling Markdown plan file (approach, files/functions touched, key tradeoffs).
   Do not inline the plan body in the task file.
   Directory beside the task file: `<task-stem>.plans/`.
   File: `<slice-key>.md`.
   Example: task `projects/foo/tasks/bar.cue` + slice `wire-api` → `projects/foo/tasks/bar.plans/wire-api.md`.
   The `create-plan` step `note` is only a pointer: `Plan file: <task-stem>.plans/<slice-key>.md` (relative to the task file's directory).
2. `grill-plan` - interrogate that plan file with the grill-me skill (`skills/grill-me/SKILL.md`) before writing code.
   If grilling surfaces a gap, revise the plan file, grill again, and mark `grill-plan` done only once the plan survives.
   The step note records what was challenged and what changed.

`grill-plan` is distinct from setup's `grill` (overall scope vs per-slice plan).
Advance refuses to pass an incomplete step, so later steps stay unreachable until both are done or explicitly skipped.
A genuinely trivial single-file edit may skip both (`status: skipped`, note with reason) under the slice kind's `coding_execution.exceptions`.
See `profile-contract.cue#plan_and_grill_guardrail`.

## Syncing recorded state with reality: sync

- `pi-job --task <t> sync [--status s1,s2]` - print a checklist of slices worth re-verifying: by default, any `in_progress`/`blocked` slice, or any slice carrying an open PR; `--status` overrides the selection.
- `pi-job` never spawns agents - `sync` only enumerates and prints instructions.
  The orchestrator dispatches subagents per listed slice, then records findings via hand-editing or `pi-job add-pr`.

## Repo work: worktrees and PRs

- `pi-job --task <t> set-worktree --slice K --repo R --path P` - record/update the filesystem worktree path for a slice's repo work (upsert; not filesystem-validated).
- `pi-job --task <t> add-pr --slice K --repo R --url U --status open|merged|closed [--note N]` - record a PR for a slice's repo work, upserting by URL.
- `pi-job --task <t> show [--all]` - also renders each slice's `repo_work`: worktree path (or "not set") and each PR's status/url/note.

## Task storage backends

`--task` accepts either a `.cue` file or a directory; `open_task_store()` picks the backend from that shape, no separate flag needed.

- **`CueTaskStore`** (default) - the CUE file described throughout this README.
  All commands work as documented.
- **`FsTaskStore`** (experimental) - a directory-backed backend.
  `task.title`/`task.status`/etc become files; `task.plan.slices[]` become subdirectories; `depends_on` becomes a directory of symlinks.
  Ordered collections use gapped numeric-prefix directory names (`0010-`, `0020-`, …) so inserts never require renaming siblings.

Both backends implement the same `TaskStore` protocol - command handlers never touch CUE-file or filesystem-tree mechanics directly.

`scaffold`, `init`, and `migrate-task` remain CUE-specific; they have no settled meaning for a directory-backed task yet.

## Converting between backends: project

- `pi-job --task <source> project --to <dest>` - copies a task's full state from `<source>` into `<dest>` using only `TaskStore` methods, in either direction between `CueTaskStore` and `FsTaskStore`.
- `<dest>` as a `.cue` path is bootstrapped from an empty skeleton if it doesn't exist yet.
  If `<dest>` already has slices or decisions, `project` refuses rather than risk shifting existing entries.
- `<dest>` as a directory is created if missing.

## Slice kinds (contract reference)

| Kind | Role |
|---|---|
| `setup` | Explore, clarify, grill scope, select toolbelt, plan implement slices - typically once, first |
| `implement` | One atomic repo-scoped change: plan, grill plan, build, verify, ship, wait for feedback |
| `closing` | Cross-slice bookkeeping once implement slices are done - typically once, last |
| `research` | Investigation without code changes |
| `spike` | Time-boxed prototype; create-plan/grill-plan apply like implement |

Machine-readable templates and policies: `profile-contract.cue#slice_kinds` and `#step_kinds`.

## Example task file shape

Illustrative only - types and structure, not a real work file:

```cue
package task

task: {
	title:  "Example bounded change"
	status: "in_progress"

	source: {
		jira:       ""
		discovered: "2026-01-01"
		context:    "Short discovery note for why this task exists."
	}

	project: {
		key:     "example"
		name:    "Example Project"
		route:   "projects/example/workflow.md"
		context: "Where this work lives in the repo."
	}

	orchestration: {
		cursor: {
			slice: "wire-api"
			step:  "grill-plan"
		}
		policy: {
			coding_execution: {
				subagent_required:             true
				lower_power_model_preferred:   true
				orchestrator_reviews_subagent: true
			}
		}
		artifacts: {
			test-case-table: #Artifact & {status: "planned", note: "Selected during setup."}
		}
	}

	context: """
		Free-form background the agent should read before acting.
		"""

	decisions: [
		#Decision & {
			date:   "2026-01-01"
			note:   "Durable choice worth recording for later review."
			source: "chat:2026-01-01"
		},
	]

	plan: {
		note: "High-level plan note."
		slices: [
			#Slice & {
				key:    "task-setup"
				kind:   "setup"
				title:  "Task setup"
				goal:   "Explore, clarify, and plan implement slices."
				status: "done"
				note:   ""
				steps: [
					#Step & {key: "explore-context", title: "Explore context", status: "done", note: ""},
					#Step & {key: "plan-slices", title: "Plan slices", status: "done", note: ""},
				]
				final_steps: []
			},
			#Slice & {
				key:    "wire-api"
				kind:   "implement"
				title:  "Wire API endpoint"
				goal:   "Return expiration in status response."
				status: "in_progress"
				note:   ""
				repos:  ["graphius"]
				steps: [
					#Step & {key: "create-plan", title: "Create plan", status: "done", note: "Plan file: task.plans/wire-api.md"},
					#Step & {key: "grill-plan", title: "Grill the plan file", status: "in_progress", note: ""},
					#Step & {key: "edit-code", title: "Edit the code", status: "planned", note: ""},
				]
				final_steps: [
					#Step & {key: "verify", title: "Run checks", status: "planned", note: ""},
					#Step & {key: "share-with-team", title: "Share with team", status: "planned", note: ""},
				]
			},
			#Slice & {
				key:    "task-closing"
				kind:   "closing"
				title:  "Task closing"
				goal:   "Reconcile docs and test plan."
				status: "planned"
				note:   ""
				depends_on: ["wire-api"]
				steps: [
					#Step & {key: "update-docs", title: "Update documentation", status: "planned", note: ""},
					#Step & {key: "update-task-file", title: "Update this task file", status: "planned", note: ""},
				]
				final_steps: []
			},
		]
	}
}
```

What `pi-job` cares about most:

- `task.orchestration` - must exist after `init`; holds cursor, policy, artifacts
- `task.orchestration.cursor` - saved resume point `{slice, step}` only
- `task.plan.slices[].kind` - selects slice-kind policies and explains step templates
- `task.plan.slices[].steps` + `final_steps` - what `next` / `advance` walk
- `task.decisions` / `task.orchestration.artifacts` - durable notes and artifact gates

## Test

```bash
# from this package directory (chezmoi source):
python3 tests/executable_test_pi_job.py
# installed copy may name this tests/test_pi_job.py:
uv run --python 3.12 python tests/test_pi_job.py
cue eval profile-contract.cue >/dev/null
```
