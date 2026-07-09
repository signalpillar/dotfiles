# pi-job-harness

Portable deterministic job harness for CUE task files.

`pi-job` keeps the CUE task file as the source of truth.
It reads `task` with `cue export`, loads package-local `profile-contract.cue` for valid profiles and phase ownership, requires an explicit profile before execution, computes the next unfinished slice/step, updates the orchestration cursor, prints the profile phase plan, and emits deterministic instruction packets for the orchestrating model.

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

## What pi-job does

`pi-job` is a small CLI that answers orchestration questions from durable state.
It does **not** run the agent session and does **not** spawn subagents.

Given a CUE task file and package-local `profile-contract.cue`, it can:

- `scaffold` - create a missing task file from the generic example shape
- `init` - record an explicit profile on the task
- `status` / `next` / `plan` - report where the work is and what the profile expects
- `instruction` - emit a deterministic packet for the current or next cursor (owner, validators, gates, todo reminders)
- `advance` - write the next cursor back into the task file after evidence lands

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

- The CUE task file is the only durable source of truth. No parallel YAML cursor, no agent memory as state.
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
BASE=https://raw.githubusercontent.com/signalpillar/dotfiles/master/dot_local/share/pi-job-harness
mkdir -p ~/.local/share/pi-job-harness/bin ~/.local/bin
curl -fsSL "$BASE/bin/executable_pi-job" -o ~/.local/share/pi-job-harness/bin/pi-job
curl -fsSL "$BASE/profile-contract.cue" -o ~/.local/share/pi-job-harness/profile-contract.cue
curl -fsSL "$BASE/README.md" -o ~/.local/share/pi-job-harness/README.md
chmod +x ~/.local/share/pi-job-harness/bin/pi-job
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'exec "$HOME/.local/share/pi-job-harness/bin/pi-job" "$@"' > ~/.local/bin/pi-job
chmod +x ~/.local/bin/pi-job
# requires: python3, cue on PATH
```

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

`plan` prints the selected profile's phase order from `profile-contract.cue` and tells the orchestrator to track those phases with session todos.
`instruction` reminds the orchestrator to keep the current cursor todo in progress until validators pass.
`next`/`advance` still walk `task.plan` slices; phase-cursor orchestration is deferred.

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
python3 tests/test_pi_job.py
cue eval profile-contract.cue >/dev/null
```
