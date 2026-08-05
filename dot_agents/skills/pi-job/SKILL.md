---
name: pi-job
description: >-
  Run durable multi-step work through the pi-job CLI task store (status, plan,
  instruction, start/finish/advance). Use when orchestrating agents against a
  pi-job task file, when tempted to open or hand-edit the task YAML/store, or
  when creating a missing task.
---

# pi-job

## Role

When this skill is active (a pi-job task file is in play): **role = orchestrator**.
CLI-only for the task store; pause only for grill / clarify / `requires_user_decision` or a recorded blocker.
This supersedes any default workspace role (including Product Owner) for the duration of the task.

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

If the file is missing, follow the create hint from the CLI (`create --kind` or `create --from`).
Deep reference / install: `~/.local/share/pi-job-harness/README.md`.

Trust `Cursor` for the active step and `Ready` for candidates.
Array order of slices is not execution order.
If the current slice has no unfinished steps (or instruction emits pick-next):
run `show`, choose a Ready slice, `advance --slice KEY --step STEP`, then `instruction`.

After create or any `instruction` packet: enter the orchestrator loop immediately.
Do not wait for the user to say "continue".
Pause only for user-decision steps (clarify/grill/requires_user_decision) or a recorded blocker.
Follow the packet's `NEXT ACTION` checklist (command hints use `TASK_FILE` / `SLICE_KEY`;
the packet header names the real task path).

## Orchestrator loop

1. `status` / `plan` / `show` - where you are; align session todos with `plan`
2. `instruction` - step packet for the saved cursor, or pick-next when the slice is exhausted
3. `start --model <provider/model>` - before work
4. Do the step (subagent when the packet says so)
5. `finish` (with evidence note) or `finish --skip --reason ...`
6. `advance` - within-slice next step; if pick-next: `show` → choose Ready → `advance --slice/--step`
7. Repeat from `instruction` until the task is done or blocked on the user

Start the slice with `start --slice-only --model <orchestrator>` when needed.

## Reads (do not open the store)

Prefer packet guidance. Typical shape:

- `status` | `plan` | `markdown [--slice SLICE_KEY]` | `show [--slice SLICE_KEY]` | `instruction`
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

Follow README **Agent dev notes**: put a coherent feature surface (e.g. Mermaid export) behind a named class boundary; keep `cmd_*` as thin wiring.
Example in-tree: `SliceDependencyMermaid` for `show --graph`.
