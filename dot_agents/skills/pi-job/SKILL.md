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
pi-job list                              # home bundles under $PI_JOB_TASKS
pi-job --task SLUG status                # preferred: slug under the task home
# path still works for loose YAML or an explicit bundle:
# pi-job --task ./legacy.task.yaml status
# pi-job --task ~/.local/share/pi-job/tasks/SLUG status
```

Create a new home task (always a bundle: `task.yaml` + `plans/` + `references/`):

```bash
pi-job --task SLUG create --kind setup --goal "Bootstrap the task home"
```

Convert a legacy loose YAML into the home:

```bash
pi-job --task ./legacy.task.yaml project --to SLUG
```

If the store is missing, follow the create hint from the CLI.
Deep reference / install: `~/.local/share/pi-job-harness/README.md`.

Claims live in `orchestration.cursors[]` (`{owner, slice, claimed_at, last_seen}`).
Active step is derived: first non-terminal step of the claimed slice.
Named owners resolve their claim even when other owners hold sibling claims.
Duplicate active rows for one named owner fail closed.
Overall `Status:` in `status` / `list` / `markdown` is derived from slice statuses; ignore top-level `task.status` in the file.
Trust `status`/`show` for claims + Ready frontier.
Array order of slices is not execution order.

```bash
pi-job --task SLUG claim --slice KEY --owner ID
# optional: export PI_JOB_OWNER=ID  (omit --owner when unambiguous / sole claim)
```

After create or any `instruction` packet: enter the orchestrator loop immediately.
Do not wait for the user to say "continue".
Pause only for user-decision steps (clarify/grill/requires_user_decision) or a recorded blocker.
Follow the packet's `NEXT ACTION` checklist (command hints use `TASK_FILE` / `SLICE_KEY`;
the packet header shows `Task:` as slug when under the home, else a path).
After create, run `pi-job loop` and arm your own `/loop` from that instruction (resolve TASK).

Fleet mode (manager + tmux workers): `pi-job loop` is the manager metronome; `pi-job loop --worker` is the first prompt for a spawned slice window.
Classic single-session pick-next loop stays unchanged when no fleet is in use.

## Orchestrator loop

1. `status` / `plan` / `show` - where you are; align session todos with `plan`
2. `claim --slice KEY --owner ID` for a Ready slice (one claim per owner)
3. `instruction` - step packet for the claim's derived active step, or pick-next when exhausted
4. `start --model <provider/model>` - before work
5. Do the step (subagent when the packet says so)
6. `finish` (with evidence note) or `finish --skip --reason ...`
7. Repeat from `instruction` until the claimed slice is exhausted
8. On pick-next: `finish --slice-only` (auto-releases claim) → `show` → claim next Ready → `instruction`

Packet `Owner:` and `Claim:` identify the session claim.
Packet `Role:` comes from the profile step owner.
`start --model` records attribution only.
Start the slice with `start --slice-only --model <orchestrator>` when needed.
`advance` is deprecated; do not use it.

Slice-worker windows follow `pi-job loop --worker`: one owner, one slice, stop after `finish --slice-only`; do not pick-next.

## Reads (do not open the store)

Prefer packet guidance. Typical shape:

- `status` | `plan` | `markdown [--slice SLICE_KEY] [--with-decisions]` | `show [--slice SLICE_KEY]` | `instruction`
- Subagent-owned steps: the packet orders `markdown --slice --with-decisions` first for binding `## Decisions`
- Interrupt/RCA while a claim is parked: `investigate` / `add-finding` (appends `.plans/_findings.md`); do not release/claim-hop unless needed
- Cross-agent contact: `msg --to manager|slice:KEY --note TEXT`; consume with `msg --read --to ADDRESS`
- Do not dump the whole task document into context

Writes: use mutation commands from `pi-job --help` only (never hand-edit the store).
Register the first layer with repeatable `layers add --bind SLICE=LAYER` flags.
This command adds the band and binds all existing implement/spike/research slices atomically.
Append dependencies with `set-slice --slice CONSUMER --depends-on PRODUCER`.
Clear them with `set-slice --slice CONSUMER --clear-depends-on`.
Slice plans live under the layout: bundle `plans/<slice-key>.md`, or legacy
`<task-stem>.plans/<slice-key>.md` until projected (constraint contracts with
types/composition and call stacks; see profile `plan_and_grill_guardrail`).

## Wayfinder

Foggy work: use the wayfinder skill with this task file as the map.

```bash
pi-job --task SLUG wayfinder-context
```

Details and kind mapping: `pi-job wayfinder-context --help` and the harness README.

## Harness Python contributions

From `dot_local/share/pi-job-harness/` (or the applied package dir after chezmoi apply):

Verify and test from the source tree or slice worktree, not `~/.local/bin/pi-job`.
See README **Verify harness changes** for commands, chezmoi apply policy, and the verify vs apply vs PATH table.

```bash
cd dot_local/share/pi-job-harness/   # chezmoi source, or slice worktree with same layout
uvx ruff@latest check .
env -u PI_JOB_OWNER uv run --with pydantic --with pyyaml python tests/executable_test_pi_job.py
```

The test runner strips `$PI_JOB_OWNER` before subprocess calls.
Use `env -u PI_JOB_OWNER` when you run tests or CLI commands manually.

When you change store, task models, or CLI behaviour, update README, this skill, and root `AGENTS.md` together (docs-with-model-cuts).

Follow README **Agent dev notes**: put a coherent feature surface (e.g. Mermaid export) behind a named class boundary; keep `cmd_*` as thin wiring.
Example in-tree: `SliceDependencyMermaid` for `show --graph`.
Mailbox CLI lives in `pi_job_harness/messaging/cli.py`; `app.py` only registers it.
