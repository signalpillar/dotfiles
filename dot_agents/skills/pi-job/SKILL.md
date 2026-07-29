---
name: pi-job
description: >-
  Run durable multi-step work through the pi-job CLI task store (status, plan,
  instruction, start/finish/advance). Use when orchestrating agents against a
  pi-job task file, when tempted to open or hand-edit the task YAML/store, or
  when bootstrapping/scaffolding a missing task.
---

# pi-job

Prefer the CLI over opening the task store.
Full-file reads burn tokens; hand-edits bypass validation and digest checks.

## Cold start

```bash
pi-job --task <file> status
```

If the file is missing, follow the scaffold/bootstrap hint from the CLI.
Deep reference: `~/.local/share/pi-job-harness/README.md` (chezmoi source: `dot_local/share/pi-job-harness/README.md`).
Install: see that README's agent self-install section (uv + harness files only).

## Orchestrator loop

1. `status` / `plan` - where you are; align session todos with `plan`
2. `instruction` / `instruction --current` - deterministic step packet
3. `start --model <provider/model>` - before work
4. Do the step (subagent when the packet says so)
5. `finish` (with evidence note) or `finish --skip --reason ...`
6. `advance` - only after evidence or an explicit skip

Start the slice with `start --slice-only --model <orchestrator>` when needed.

## Reads (do not open the store)

- `status`, `plan`, `show`, `show --slice KEY`, `instruction [--current]`
- Use `show --slice KEY` when you need that slice's goal, notes, steps, or repo_work
- Do not dump or browse the whole task document into context

## Writes (mutation commands only)

- Lifecycle: `start`, `finish` (default `--note` appends with a blank line; `--replace` overwrites), `advance`
- Shape: `add-slice`, `add-step`, `set-slice`, `block-slice`, `unblock-slice`, `remove-slice`, `set-worktree`, `add-pr`
- Metadata: `set-project`, `set-context`, `add-decision`, `set-plan-note`
- Out-of-band hand-edit: `acknowledge-edit --reason '<why>'` (clears the digest warning; do not silent-ignore it)
- See `pi-job --help` for the rest

Put slice plans in `<task-stem>.plans/<slice-key>.md`, not in endless notes.
Those files are succinct constraint-and-behaviour contracts.
Full wording lives in `plan_and_grill_guardrail` in `~/.local/share/pi-job-harness/profile.yaml` (chezmoi source: `dot_local/share/pi-job-harness/profile.yaml`).
`validate` / `status` warn on oversized notes (~2000 chars) and large task files (~100KB); they do not refuse `finish`.

## Harness Python contributions

From `dot_local/share/pi-job-harness/` (or the applied package dir):

```bash
uvx ruff@latest check .
```
