# heartbeat-cli

## Goal
Add thin `pi-job loop [--interval 5m]` that prints one Cursor `/loop …` line sourced from `orchestrator_heartbeat` with literal `TASK` placeholder (no auto-arm, no `--task` required). Pure render helper (+ optional small class boundary), `cmd_loop` wiring, tests, README mention.

## Depends on
heartbeat-profile

## Intent
- Mirror the investigate print-only pattern: profile body → pure render helper → thin `cmd_*`.
- CLI is a printer only: stdout is one `/loop <interval> <packet-body>` line for the orchestrator to arm manually.
- Default `--interval` is `5m`; accept simple duration tokens compatible with Cursor `/loop` (e.g. `30s`, `5m`, `2h`).
- Profile body is static (no `{interval}` format); CLI alone owns the armed cadence in the `/loop` prefix.
- Do not require `--task`; do not read or mutate any task file.
- Do not auto-arm Cursor loops, spawn background sleepers, or integrate with the loop skill runtime.

## Types and composition
```ts
type LoopInterval = string // CLI token, default "5m"; used only in /loop prefix

// pure render (function or small class with one method):
render_orchestrator_heartbeat(): string
  // loads instruction_packets.orchestrator_heartbeat
  // returns body as-is (no .format); strip internal newlines for single-line stdout

// cmd edge:
cmd_loop(args): void
  // interval = args.interval or "5m"
  // body = render_orchestrator_heartbeat().strip()
  // print(f"/loop {interval} {body}")
```

Boundaries:
- Render helper is pure: profile template only; no disk beyond profile load (same as `render_investigate_interrupt`).
- `cmd_loop` does not call `require_task`, `open_task_store`, or `resolve_claim_for_command`.
- Literal `TASK` stays in the template; CLI must not substitute a real path.
- Optional class boundary (e.g. `OrchestratorHeartbeatRenderer`) is acceptable if it keeps `cmd_loop` thin; prefer matching investigate's function shape unless README dev-notes class pattern clearly fits this one-liner surface.

## Call stacks
```ts
pi-job loop [--interval 5m]
  -> parse --interval (default 5m)
  -> render_orchestrator_heartbeat()
    -> load_profile_contract()["instruction_packets"]["orchestrator_heartbeat"]
  -> print(f"/loop {interval} {body_single_line}")

// contrast investigate (do not copy task coupling):
pi-job investigate --task FILE --topic T
  -> require_task; open store; optional claim context
  -> render_investigate_interrupt(task_file=real path, ...)
  -> print(render output, end="")
```

## System behaviour
- Invoking `pi-job loop` with no args prints a line starting with `/loop 5m ` followed by the static packet body (single physical line).
- `--interval 10m` changes only the `/loop` prefix token (body unchanged).
- stdout contains literal `TASK` where the packet places it; caller replaces with the real task path when arming.
- stderr is silent on success; non-zero exit only on bad args or profile load/validation failure.
- No side effects: no task YAML writes, no claims touched, no findings, no background processes.
- Help text describes print-only behaviour and points at profile packet ownership.

## Constraints
- Must not require `--task` or open a task store.
- Must not auto-arm `/loop` in Cursor or start shell sleep loops.
- Must not add `--max-idle`, `--until`, or other duration CLI flags in v0.
- Must not interpolate `{task_file}` / `{interval}` into the profile body or substitute a concrete task path.
- Must not hardcode heartbeat prose in Python.
- Must not mutate task state or claims (stricter than investigate, which optionally reads claim for `{cursor_label}`).
- Must not add live `/loop` integration tests against Cursor.
- Land in chezmoi harness sources on master; no branch/worktree/PR unless repo convention changes.

## Verification
- Unit tests in `tests/executable_test_pi_job.py`:
  - `pi-job loop` succeeds with no `--task` and no task file in cwd.
  - stdout matches `/loop 5m ` prefix by default.
  - stdout contains literal `TASK` from the static body.
  - `pi-job loop --interval 10m` updates prefix only; body unchanged.
  - stdout is a single line (strip internal newlines from template in render/cmd).
  - optional: invalid profile missing packet fails loudly (covered by profile slice + load path).
- Direct unit test of `render_orchestrator_heartbeat` with a stub profile or contract load (mirror investigate render tests if present).
- `uvx ruff@latest check .` in harness package.
- Manual smoke: run `pi-job loop`, paste output into a chat, confirm it reads as a valid `/loop` invocation with resolvable `TASK`.

## Touch surface
- `dot_local/share/pi-job-harness/bin/executable_pi-job` (`render_orchestrator_heartbeat`, `cmd_loop`, argparse subparser)
- `dot_local/share/pi-job-harness/tests/executable_test_pi_job.py`
- `dot_local/share/pi-job-harness/README.md` (command mention + profile packet example list alongside `investigate_interrupt`)
- applied copy under `~/.local/share/pi-job-harness` via chezmoi apply as needed

## Open questions
- Interval validation strictness: reject malformed tokens vs pass through verbatim to `/loop` (prefer light validation: non-empty string; defer strict parsing unless harness already has a shared helper).
- Whether to strip internal newlines from packet body always, or require profile author to keep body single-line (prefer cmd/render strip for robustness).
- Exact `/loop` prefix formatting if body already starts with punctuation or needs quoting (implement detail).

## Decisions
- Product decisions live in the task `decisions` list (thin printer; default 5m; TASK placeholder not `--task`; no duration flags; agents arm own `/loop`; mirror investigate print-only stack).
