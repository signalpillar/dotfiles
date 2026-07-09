This project contains all my dotfiles that are moved from box to box.

## Manually installed

- [AltTab - Windows alt-tab on macOS](https://alt-tab-macos.netlify.app/)
- [exelban/stats: macOS system monitor in your menu bar](https://github.com/exelban/stats)
- [MeetingBar - Simplify Meetings on macOS with One-Click Access](https://meetingbar.app/)
- [kitty](https://sw.kovidgoyal.net/kitty/)
- [dwarvesf/hidden: An ultra-light MacOS utility that helps hide menu bar icons](https://github.com/dwarvesf/hidden)
- Slack
- Parallels (before VirtualBox)
- Dropbox
- https://matthewpalmer.net/vanilla/
- [Proxyman · Debug, intercept & mock HTTP with Proxyman](https://proxyman.io/)

## Android Command line tools
- Download and unpack the dir `~/proj/cmdline-tools/latest`.
- Add to path
- Run `sdkmanager`

## pi-job harness

Portable deterministic job harness for CUE task files, managed by chezmoi under:

- source: [`dot_local/share/pi-job-harness/`](dot_local/share/pi-job-harness/)
- installed: `~/.local/share/pi-job-harness/`
- wrapper: [`dot_local/bin/executable_pi-job`](dot_local/bin/executable_pi-job) → `~/.local/bin/pi-job`

Package docs: [`dot_local/share/pi-job-harness/README.md`](dot_local/share/pi-job-harness/README.md).

### Picture

```text
  human / capture
        |
        v
  CUE task file  <----------------------------- advance (write cursor)
  (SoT: plan, decisions, artifacts, cursor)
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

### Principles

- The CUE task file is the only durable source of truth. No parallel YAML cursor, no agent memory as state.
- Profiles are configuration in `profile-contract.cue`, not hardcoded Python. Repo/user layering can evolve; contract wins for phase meaning.
- The harness is deterministic and fail-closed: missing task -> `scaffold`; missing profile -> `init`; then `plan` / `instruction` / `advance`.
- `pi-job` emits instruction packets. It does not spawn agents. The orchestrator chooses models and launches subagents.
- Slice progress (`next` / `advance`) and profile phases (`plan`) are related but not the same yet. Agents should keep session todos aligned with `plan` while advancing the task cursor for concrete steps.

### How an agent should know about it

1. Global hint: this chezmoi [`AGENTS.md`](AGENTS.md) points agents at `pi-job` for CUE-task orchestration.
2. On PATH after chezmoi apply: `pi-job` -> `~/.local/bin/pi-job`.
3. When a user names a task file, or work clearly belongs in a CUE task, run `pi-job --task <file> status` first.
4. If the file is missing, follow the error to `scaffold`, edit the scaffold, then `init --profile`.
5. Use `plan` to create session todos, `instruction` before acting, and `advance` only after evidence or a recorded blocker.
6. Prefer package docs at `~/.local/share/pi-job-harness/README.md` over inventing a parallel workflow.

### How it works

1. A repo keeps work in a CUE **task file** (`task.orchestration`, `task.plan.slices`, decisions, artifacts).
2. `pi-job` reads that task with `cue export` and loads package-local `profile-contract.cue` for valid profiles, phase owners, validators, and artifact gates.
3. You pick a profile once (`init --profile …`). Until then, `plan` / `next` / `advance` / `instruction` fail closed.
4. `plan` prints the profile phase order and tells the orchestrating agent to track those phases with session todos.
5. `next` / `advance` walk unfinished `task.plan` slices/steps and update the saved cursor in the CUE file.
6. `instruction` emits a deterministic packet for the current/next cursor: owner (orchestrator vs subagent), validators, artifact gates, and todo-tracking reminders. The harness does **not** spawn agents; the orchestrator launches any subagent.

Typical flow from a repo that owns the task:

```bash
# Missing task file? scaffold writes the generic example shape first.
pi-job --task projects/example/tasks/task.cue scaffold
pi-job --task projects/example/tasks/task.cue init --profile small
pi-job --task projects/example/tasks/task.cue plan
pi-job --task projects/example/tasks/task.cue instruction --current
# …do the step, record evidence…
pi-job --task projects/example/tasks/task.cue advance
```

If the task path does not exist, `status` / `plan` / `instruction` / etc. fail closed and point at `scaffold`, then `init --profile`.

V0 profiles: `small`, `full`, `research`, `review-only`, `share-only`, `spike-prototype`.

Prototype also lives in weight-loss as `packages/pi-job-harness/`; chezmoi is the portable install path across machines.

### Example task file shape

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

## Emacs

- [Config of Gergely Nagy](https://github.com/algernon/emacs.d/blob/master/.spacemacs)

## Fonts

- Cascadia Mono ([download](https://github.com/microsoft/cascadia-code))
- IBM Plex ([download](https://github.com/IBM/plex/releases/))

## ML
- https://mlflow.org/

## Nix

- [Intro to flakes](https://serokell.io/blog/practical-nix-flakes)
- [Tutorial](https://www.tweag.io/blog/2020-05-25-flakes/)
  - `nix-env -f '<nixpkgs>' -iA nixUnstable`
- [NixOS in VMWare Fusion](https://dev.to/ryuheechul/quickest-way-to-run-nixos-on-your-vmware-fusion-4dn7)

## Inspiration
https://omakub.org

## Maybe Later
- [sq | wrangle data](https://sq.io/) (like jq/yq but for SQL)

## History

### June 2026
Moved completely from nix-darwin to Homebrew and chezmoi for macOS configuration. 
Idempotency is now handled via `Brewfile` and `run_onchange_*.sh` scripts.
We use `mise` for installing and managing different versions of programming languages.

### Sept 2025
Enjoying Ubuntu in VM.

For history

- [Android Command line tools](https://developer.android.com/studio#cmdline-tools)
- [super-productivity](https://github.com/johannesjo/super-productivity)
- [Spark](https://sparkmailapp.com/) or [Mimestream](https://mimestream.com/)
- [Kap](https://getkap.co/)
- [Hammerspoon](https://www.hammerspoon.org/)
- [Welcome to xbar](https://xbarapp.com/)

### June 2023
nix-shell and darwin-nix

### Sept 2021
Development happens only in NixOS running in VMWare Fusion. I feel I am far from
using flake though.

### March 2021
Slow migration to nix-darwin started.
All non-gui apps can be there.

### Jan 2020
Project restructured to work with [chezmoi](https://github.com/twpayne/chezmoi/blob/master/docs/HOWTO.md)

### June 2019
An attempt to adopt Nix package manager.

### May 2015

* Moved to [spacemacs](https://github.com/syl20bnr/spacemacs) as a replacement for Prelude.
  It is a Emacs Kit, focused on integration with Evil mode.

* No need to use `nsenter` to enter docker container, using `docker exec` instead.

# NixOS

- `xset r rate 200 25`
- `npm config set prefix '~/mutable_node_modules'`
