# pi-job-harness

Portable deterministic job harness for CUE task files.

`pi-job` keeps a task's durable state in exactly one `TaskStore` backend - a CUE file by default; see [Task storage backends](#task-storage-backends) for the experimental directory-backed one.
It reads `task` via that backend, loads package-local `profile-contract.cue` for valid profiles and phase ownership, requires an explicit profile before execution, computes the next unfinished slice/step, updates the orchestration cursor, prints the profile phase plan, and emits deterministic instruction packets for the orchestrating model.

## Contents

```text
bin/pi-job              CLI entrypoint
profile-contract.cue    portable profile/phase/artifact contract
tests/test_pi_job.py    regression tests for the CLI
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
- `init` - record an explicit profile on the task
- `status` / `next` / `plan` - report where the work is and what the profile expects
- `instruction` - emit a deterministic packet for the current or next cursor (owner, validators, gates, todo reminders)
- `advance` - write the next cursor back into the task file after evidence lands; fails closed if the current cursor's step is not `done`/`skipped` unless `--force --reason '<why>'` is given, and rejects unknown `--phase` values for the profile

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
  (source of truth: plan, decisions, artifacts, cursor)
        |
        | cue export
        v
     pi-job  ---- loads ----> profile-contract.cue
        |                     (profiles, owners, validators, gates)
        |
        +-- status / plan / next
        |
        +-- instruction ----> orchestrator agent
                                   |
                                   +-- session todos (track plan phases)
                                   |
                                   +-- maybe subagent (lower-cost model)
                                   |
                                   +-- evidence back into task file
```

## Principles

- Task state lives in exactly one `TaskStore` backend (CUE file by default; see [Task storage backends](#task-storage-backends)) - no parallel YAML cursor, no agent memory as state.
- Profiles are configuration in `profile-contract.cue`, not hardcoded Python. Repo/user layering can evolve; contract wins for phase meaning.
- The harness is deterministic and fail-closed: missing task -> `scaffold`; missing profile -> `init`; then `plan` / `instruction` / `advance`.
- `pi-job` emits instruction packets. It does not spawn agents. The orchestrator chooses models and launches subagents.
- Slice progress (`next` / `advance`) and profile phases (`plan`) are related but not the same yet. Agents should keep session todos aligned with `plan` while advancing the task cursor for concrete steps.

## How an agent should know about it

1. Global hint: chezmoi [`AGENTS.md`](../../../AGENTS.md) points agents at `pi-job` for CUE-task orchestration.
2. On PATH after chezmoi apply: `pi-job` -> `~/.local/bin/pi-job`.
3. When a user names a task file, or work clearly belongs in a CUE task, run `pi-job --task <file> status` first.
4. If the file is missing, follow the error to `scaffold`, edit the scaffold, then `init --profile`.
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

When kept inside a product repo, a thin wrapper such as `scripts/pi-job` can point at a local package copy.

## Usage

Run from the repository that owns the task file.
The current working directory is reported as the repository root in instruction packets.

```bash
# If the task file does not exist yet:
pi-job --task projects/example/tasks/task.cue scaffold
pi-job --task projects/example/tasks/task.cue init --profile small

pi-job --task projects/example/tasks/task.cue status
pi-job --task projects/example/tasks/task.cue plan
pi-job --task projects/example/tasks/task.cue next
pi-job --task projects/example/tasks/task.cue instruction --current
pi-job --task projects/example/tasks/task.cue advance
```

If `--task` points at a missing file, commands fail closed and tell the agent to run `scaffold` (writes the generic example CUE shape), then `init --profile`.
A task with `Profile: <unset>` is not initialized; run `init --profile <profile>` before `plan`, `next`, `advance`, or `instruction`.

## Migrating legacy task files

If a task file was created before `task-schema.cue` existed, it may have local copies of type declarations (`#Status`, `#Step`, `#Decision`, `#Artifact`, `#Slice`) at the top level.
These are now legacy — the shared `task-schema.cue` is unified into every `pi-job` command automatically (both files are passed together to `cue export`), making local copies redundant and a source of confusion during edits.

`pi-job migrate-task` diagnoses a task file for these legacy declarations and prints safe deletion or refactoring recommendations.
It never modifies the file — you use your normal editor to apply the changes.
After editing, run `pi-job status` and `pi-job show` to confirm the migrated file still validates (these commands automatically load both the task file and shared schema).

Note: a bare `cue vet` or `cue export` invoked on the migrated task file *alone* will fail with missing reference errors like `reference "#Step" not found` — this is expected and not breakage. Only `pi-job` subcommands (or `cue` explicitly passed both files) validate the migrated file correctly.

`plan` prints the selected profile's phase order from `profile-contract.cue` and tells the orchestrator to track those phases with session todos.
`instruction` reminds the orchestrator to keep the current cursor todo in progress until validators pass.
`next`/`advance` walk `task.plan` slices under the profile's slice-work contract phase (e.g. `implement_slices` for `full`, `implement` for `small`/`spike-prototype`) — the saved `cursor.phase` is always a real contract `#PhaseKey`, not a slice-key derivation.
Once every slice/step is `done`/`skipped`, `next`/`advance` continue walking the remaining profile phases in contract order (e.g. `review` → `decision_review_deck` → `share_with_team` → … for `full`), anchored on the saved cursor's phase, until the profile's last phase is reached — only then does `next` report `done`.
`advance` refuses to move past an unfinished step (status not `done`/`skipped`) unless `--force --reason '<why>'` records an explicit skip, and refuses unknown `--phase` values for the selected profile.

## Toolbelt & visualization

- `pi-job --task <t> toolbelt` — list planning aids whose `suits` includes the task's profile, with each aid's registered status.
- `pi-job --task <t> toolbelt add <key> [--path P] [--status S] [--note N]` — register/update a planning aid as an `#Artifact` under `task.orchestration.artifacts` (idempotent; validates `<key>` against the catalog).
- `pi-job --task <t> show [--all] [--started] [--status s1,s2]` — render the task as a cursor-focused slice/step tree with a toolbelt footer. By default only the current cursor's slice expands; `--started` additionally expands any slice whose status isn't `planned`; `--all` expands every slice regardless of status. `--status` is a separate filter on which slices are listed at all.

The `full` profile runs a `select_toolbelt` phase before `plan_slices`: the model picks the aids that help write the plan and registers them (`--status planned`); each is produced during `plan_slices` and reconciled against shipped code by the `reconcile-artifacts` terminal step. The catalog lives in `profile-contract.cue#toolbelt`.

## Planning before code changes: `create-plan` / `grill-plan`

Every coding profile (`small`, `full`, `spike-prototype` - the three that walk `task.plan.slices` under a slice-work phase) requires each slice's `steps` to lead with two steps, in order, before anything else in that slice starts:

1. `create-plan` - the detailed implementation plan for that slice specifically (approach, files/functions touched, key tradeoffs), not a restatement of the slice's goal.
2. `grill-plan` - interrogate that plan with the grill-me skill (`skills/grill-me/SKILL.md`) before writing code. If grilling surfaces a real gap, the loop is: revise `create-plan`'s note, grill again, only mark `grill-plan` done once the plan survives.

No new `pi-job` logic enforces this - it's the same step-ordering gate `advance`/`next` already apply to any step, just pointed at two new conventional step keys. A genuinely trivial single-file edit may skip both (status `skipped`, note recording why), the same exception class `coding_execution.exceptions` already covers. This is distinct from the `full` profile's task-level `grill_plan` *phase* (which grills overall scope before slices exist) - `create-plan`/`grill-plan` are the finer-grained, per-slice equivalent, and the only grilling `small`/`spike-prototype` slices get at all. See `profile-contract.cue#plan_and_grill_guardrail`.

## Repo work: worktrees & PRs

- `pi-job --task <t> set-worktree --slice K --repo R --path P` — record/update the filesystem
  worktree path for a slice's repo work (upsert; not filesystem-validated).
- `pi-job --task <t> add-pr --slice K --repo R --url U --status open|merged|closed [--note N]` —
  record a PR for a slice's repo work, upserting by URL (re-running with the same `--url` and a
  new `--status` updates the existing entry rather than duplicating it).
- `pi-job --task <t> show [--all]` — also renders each slice's `repo_work`: worktree path (or
  "not set") and each PR's status/url/note.

## Task storage backends

`--task` accepts either a `.cue` file or a directory; `open_task_store()` picks the backend from that shape, no separate flag needed.

- **`CueTaskStore`** (default) - the CUE file described throughout this README. All commands work as documented.
- **`FsTaskStore`** (experimental) - a directory-backed, "everything is a file" backend. `task.title`/`task.status`/etc become files; `task.plan.slices[]` become subdirectories; `depends_on` becomes a directory of symlinks, each pointing at the sibling slice directory it depends on. Ordered collections nothing external ever references (steps, final_steps, decisions, PRs) use gapped numeric-prefix directory names (`0010-`, `0020-`, ...) so inserting between two existing entries never requires renaming them. Slices instead use a stable key-named directory plus a `.order` file for sequence, since slice directories are also `depends_on` symlink targets and must never be renamed.

Both backends implement the same `TaskStore` protocol (`read`, `describe`, `set_cursor`, `set_profile`, `init_task`, `set_plan_note`, `add_decision`, `add_slice`, `add_step`, `set_worktree`, `add_pr`, `write_artifact`) - command handlers never touch CUE-file or filesystem-tree mechanics directly.

`scaffold`, `init`, and `migrate-task` remain CUE-specific; they have no settled meaning for a directory-backed task yet.

## Converting between backends: `project`

- `pi-job --task <source> project --to <dest>` - copies a task's full state from `<source>` into `<dest>` using only `TaskStore` methods, so it works in either direction between `CueTaskStore` and `FsTaskStore`.
- `<dest>` as a `.cue` path is bootstrapped from an empty, placeholder-free skeleton if it doesn't exist yet - not `scaffold`'s example shape, which ships one slice that `project` would then append after rather than replace. If `<dest>` already has slices or decisions, `project` refuses rather than risk shifting existing entries.
- `<dest>` as a directory is created if missing.

Verified against a real production task file (10 slices, 24 decisions, PRs and repo_work): CUE -> fresh directory -> fresh CUE file round-trips with zero semantic difference under `cue export`.

## How it works

1. A repo keeps work in a CUE **task file** (`task.orchestration`, `task.plan.slices`, decisions, artifacts).
2. `pi-job` reads that task with `cue export` and loads package-local `profile-contract.cue` for valid profiles, phase owners, validators, and artifact gates.
3. You pick a profile once (`init --profile …`). Until then, `plan` / `next` / `advance` / `instruction` fail closed.
4. `plan` prints the profile phase order and tells the orchestrating agent to track those phases with session todos.
5. `next` / `advance` walk unfinished `task.plan` slices/steps and update the saved cursor in the CUE file.
6. `instruction` emits a deterministic packet for the current/next cursor: owner (orchestrator vs subagent), validators, artifact gates, and todo-tracking reminders.

## Profiles

V0 profiles:

- `small`
- `full`
- `research`
- `review-only`
- `share-only`
- `spike-prototype`

The machine-readable profile and artifact rules are in `profile-contract.cue`.
`pi-job` loads that file at runtime via `cue export`; it does not hardcode the profile set or phase owners.

## Example task file shape

Illustrative only - types and structure, not a real work file:

```cue
package task

#Status: "planned" | "in_progress" | "blocked" | "done" | "skipped"

#Step: {
	key:    string
	title:  string
	status: #Status
	note:   string
}

#Decision: {
	date:   string
	note:   string
	source: string
}

#Artifact: {
	status: #Status
	path?:  string
	note:   string
}

#Slice: {
	key:    string
	title:  string
	goal:   string
	status: #Status
	note:   string
	steps: [...#Step]
	final_steps: [
		#Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence or record the gap"},
		#Step & {key: "update-task-file", title: "Update this task file plan"},
	]
}

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
		profile: "small"
		cursor: {
			phase: "implement"
			slice: "do-the-change"
			step:  "edit-code"
		}
		policy: {
			coding_execution: {
				subagent_required:             true
				lower_power_model_preferred:   true
				orchestrator_reviews_subagent: true
			}
		}
		artifacts: {
			decision_review_deck: #Artifact & {status: "skipped", note: "No async review needed."}
			daily_boo:            #Artifact & {status: "planned", note: "Append only on a real aha."}
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
				key:    "do-the-change"
				title:  "Do the change"
				goal:   "Ship the bounded edit with verification."
				status: "in_progress"
				note:   ""
				steps: [
					#Step & {key: "edit-code", title: "Edit the code", status: "planned", note: ""},
					#Step & {key: "verify", title: "Run checks", status: "planned", note: ""},
				]
				final_steps: [
					#Step & {key: "e2e-evidence", title: "Provide e2e/acceptance evidence or record the gap", status: "planned", note: ""},
					#Step & {key: "update-task-file", title: "Update this task file plan", status: "planned", note: ""},
				]
			},
		]
	}
}
```

What `pi-job` cares about most:

- `task.orchestration.profile` - required before `plan` / `next` / `advance` / `instruction`
- `task.orchestration.cursor` - saved resume point (`phase` / `slice` / `step`)
- `task.plan.slices[].steps` + `final_steps` - what `next` / `advance` walk
- `task.decisions` / `task.orchestration.artifacts` - durable notes and artifact gates

## Test

```bash
# from this package directory, or via a repo wrapper:
uv run --python 3.12 python tests/test_pi_job.py
# or: python3 tests/test_pi_job.py
cue eval profile-contract.cue >/dev/null
```
