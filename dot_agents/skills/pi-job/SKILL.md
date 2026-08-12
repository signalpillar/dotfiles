---
name: pi-job
description: >-
  Run durable multi-step work through the pi-job CLI task store (status, plan,
  instruction, claim/release, start/finish). Use when orchestrating agents against a
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

Claims live in `orchestration.cursors[]` (`{owner, slice, claimed_at, last_seen}`).
Active step is derived: first non-terminal step of the claimed slice.
Trust `status`/`show` for claims + Ready frontier.
Array order of slices is not execution order.

```bash
pi-job --task TASK_FILE claim --slice KEY --owner ID
# optional: export PI_JOB_OWNER=ID  (omit --owner when unambiguous / sole claim)
```

After create or any `instruction` packet: enter the orchestrator loop immediately.
Do not wait for the user to say "continue".
Pause only for user-decision steps (clarify/grill/requires_user_decision) or a recorded blocker.
Follow the packet's `NEXT ACTION` checklist (command hints use `TASK_FILE` / `SLICE_KEY`;
the packet header names the real task path).

## Orchestrator loop

1. `status` / `plan` / `show` - where you are; align session todos with `plan`
2. `claim --slice KEY --owner ID` for a Ready slice (one claim per owner)
3. `instruction` - step packet for the claim's derived active step, or pick-next when exhausted
4. `start --model <provider/model>` - before work
5. Do the step (subagent when the packet says so)
6. `finish` (with evidence note) or `finish --skip --reason ...`
7. Repeat from `instruction` until the claimed slice is exhausted
8. On pick-next: `finish --slice-only` (auto-releases claim) → `show` → claim next Ready → `instruction`

Start the slice with `start --slice-only --model <orchestrator>` when needed.
`advance` is deprecated; do not use it.

## Reads (do not open the store)

Prefer packet guidance. Typical shape:

- `status` | `plan` | `markdown [--slice SLICE_KEY] [--with-decisions]` | `show [--slice SLICE_KEY]` | `instruction`
- Subagent-owned steps: the packet orders `markdown --slice --with-decisions` first for binding `## Decisions`
- Interrupt/RCA while a claim is parked: `investigate` / `add-finding` (appends `.plans/_findings.md`); do not release/claim-hop unless needed
- Do not dump the whole task document into context

Writes: use mutation commands from `pi-job --help` only (never hand-edit the store).
Slice plans: `<task-stem>.plans/<slice-key>.md` (constraint contracts with types/composition
and call stacks; see profile `plan_and_grill_guardrail`).

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
