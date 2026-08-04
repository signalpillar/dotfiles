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
Developer experience and agent experience share the same constructs: clear names, small packets, and machine-readable contracts help both.

## Cold start

```bash
pi-job --task <file> status
```

If the file is missing, follow the scaffold/bootstrap hint from the CLI.
Deep reference: `~/.local/share/pi-job-harness/README.md` (chezmoi source: `dot_local/share/pi-job-harness/README.md`).
Install: see that README's agent self-install section (uv + harness files only).

After bootstrap, scaffold+init, or any `instruction` packet: enter the orchestrator loop immediately.
Do not wait for the user to say "continue".
Pause only for user-decision steps (clarify/grill/requires_user_decision) or a recorded blocker.
Follow the packet's `NEXT ACTION` checklist.

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
Persist durable agreements with `add-decision` (and/or the grilled plan), not only in chat.
Token smell: if a step needs a huge dump to proceed, shrink the contract or the slice.
Full wording lives in `plan_and_grill_guardrail` in `~/.local/share/pi-job-harness/profile.yaml` (chezmoi source: `dot_local/share/pi-job-harness/profile.yaml`).
`validate` / `status` warn on oversized notes (~2000 chars) and large task files (~100KB); they do not refuse `finish`.

## Wayfinder: chart foggy work into the task file

When work is too big and foggy to plan in one setup pass, use the wayfinder skill (installed separately) with this task file as its issue tracker.
The map is the task file - do not keep a parallel map anywhere else.

Load the map before charting:

```bash
pi-job --task <file> wayfinder-context
```

It prints the destination (`plan.note`), recorded decisions, in-progress/done slices, and the planned work split into FRONTIER (takeable now) vs FOG (blocked by unfinished dependencies).

Map wayfinder's constructs onto pi-job:

| Wayfinder | pi-job |
|---|---|
| the map | this task file (destination = `plan.note`) |
| decisions so far | `decisions` via `add-decision` |
| research / prototype / decision ticket | a slice: `add-slice --kind research` / `spike` / `fog` |
| implementation ticket | `add-slice --kind implement` |
| blocking relationship | `--depends-on` |
| frontier vs fog | actionable slices vs dependency-blocked ones |
| resolving a ticket | record a decision, then `finish` / `advance` |

The `setup` slice's `wayfinder` step charts the first map.
A `fog` slice defers a decision-branch via `depends_on` and recurses through its own `wayfinder` step, so charting one area can spawn further fog slices.
Grill sharpens what the user already knows; wayfinder charts what nobody knows yet and picks a resolver (research, prototype, grill, or task) for each unknown.
The task is never frozen - it grows as slices are discovered.

## Harness Python contributions

From `dot_local/share/pi-job-harness/` (or the applied package dir):

```bash
uvx ruff@latest check .
```
