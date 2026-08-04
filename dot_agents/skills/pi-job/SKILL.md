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

## Discover commands (do not memorize encyclopedias here)

```bash
pi-job --help
pi-job <command> --help
pi-job profile          # kinds + where the contract lives
pi-job profile --json   # full validated profile (packets, cli_help, …)
```

Channel rules (decision vs `finish --note` vs plan file), packet wording, and CLI help
snippets live in `~/.local/share/pi-job-harness/profile.yaml` (chezmoi:
`dot_local/share/pi-job-harness/profile.yaml`). Read them via help/profile - do not
restate them from this skill.

## Cold start

```bash
pi-job --task TASK_FILE status
```

If the file is missing, follow the scaffold/bootstrap hint from the CLI.
Deep reference / install: `~/.local/share/pi-job-harness/README.md`.

After bootstrap, scaffold+init, or any `instruction` packet: enter the orchestrator loop immediately.
Do not wait for the user to say "continue".
Pause only for user-decision steps (clarify/grill/requires_user_decision) or a recorded blocker.
Follow the packet's `NEXT ACTION` checklist (command hints use `TASK_FILE` / `SLICE_KEY`;
the packet header names the real task path).

## Orchestrator loop

1. `status` / `plan` - where you are; align session todos with `plan`
2. `instruction` / `instruction --current` - deterministic step packet
3. `start --model <provider/model>` - before work
4. Do the step (subagent when the packet says so)
5. `finish` (with evidence note) or `finish --skip --reason ...`
6. `advance` - only after evidence or an explicit skip
7. Repeat from `instruction --current` until the task is done or blocked on the user

Start the slice with `start --slice-only --model <orchestrator>` when needed.

## Reads (do not open the store)

Prefer packet guidance. Typical shape:

- `status` | `plan` | `markdown [--slice SLICE_KEY]` | `show [--slice SLICE_KEY]` | `instruction [--current]`
- Subagent-owned steps: the packet orders `markdown --slice` first for binding `## Decisions`
- Do not dump the whole task document into context

Writes: use mutation commands from `pi-job --help` only (never hand-edit the store).
Slice plans: `<task-stem>.plans/<slice-key>.md` (constraint contracts; see profile
`plan_and_grill_guardrail`).

## Wayfinder

Foggy work: use the wayfinder skill with this task file as the map.

```bash
pi-job --task TASK_FILE wayfinder-context
```

Details and kind mapping: `pi-job wayfinder-context --help` and the harness README.

## Harness Python contributions

From `dot_local/share/pi-job-harness/` (or the applied package dir):

```bash
uvx ruff@latest check .
```
