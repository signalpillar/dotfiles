# heartbeat-profile

## Goal
Add `instruction_packets.orchestrator_heartbeat` to `profile.yaml` as a short manager-metronome nudge (role + interval/idle blurb; literal `TASK`; no ownership tutorial). Extend `InstructionPacketsDocument`; validate via `pi-job profile --json` and profile contract tests.

## Depends on
orchestrator-loop

## Intent
- Own heartbeat nudge wording in the profile; Python must not hardcode parallel copy.
- Mirror the `investigate_interrupt` sidecar pattern: profile-owned body; render helper and CLI live in sibling slices.
- Packet is a short nudge, not a reprint of the orchestrator loop: role (manager metronome / Ready fan-out reminder) plus fixed interval and idle-frontier blurb.
- Interval guidance (3–5m active, 10–15m waiting on deps/PRs) is fixed advisory prose; armed cadence lives only in the CLI `/loop <interval>` prefix (heartbeat-cli). No `{interval}` placeholder in this packet.
- Idle-frontier guidance (~5h keep-alive on empty Ready, stop on user, do not stop on first empty tick) stays in packet prose only.
- Packet body uses literal `TASK` (not `{task_file}` interpolation); orchestrator resolves the real path when arming.
- Do not teach multi-cursor ownership; do not duplicate the full status/show/finish checklist already in orchestrator packets / skill.

## Types and composition
```ts
// profile.yaml
instruction_packets:
  orchestrator_heartbeat: str // new required field; no format placeholders

// executable_pi-job
class InstructionPacketsDocument(StrictDocument):
  orchestrator_heartbeat: str = Field(
    description=(
      "Body printed by `pi-job loop` after the `/loop <interval>` prefix. "
      "No format placeholders. Use literal TASK in command hints (not {task_file}). "
      "Profile is the only body; Python must not hardcode the packet."
    )
  )
```

Boundaries:
- Wording lives in `profile.yaml`; no Python fallback string.
- No `{interval}` or `{task_file}` placeholders; body is static prose with literal `TASK`.
- Armed interval is CLI-owned (`/loop <interval> …`); body may still mention fixed advisory ranges (3–5m / 10–15m).
- Packet is not an instruction step kind; it is a sidecar coaching block like `investigate_interrupt`.
- Idle ceiling (~5h) and stop-on-user are packet prose only; no profile keys for duration policy in v0.

## Call stacks
```ts
profile.yaml (orchestrator_heartbeat prose)
  -> InstructionPacketsDocument.model_validate on load
  -> load_profile_contract()["instruction_packets"]["orchestrator_heartbeat"]

// downstream (heartbeat-cli slice; not implemented here):
render_orchestrator_heartbeat() -> str
  -> return profile body as-is (no .format)
  -> cmd_loop prints `/loop {interval} {body}` (prefix CLI-owned; body from profile)
```

## System behaviour
- Profile load rejects missing `orchestrator_heartbeat` (same strictness as `investigate_interrupt`).
- Packet content is short (fits one `/loop` line after CLI wrap) and covers:
  - Metronome role: stay in manager mode; fan out Ready work; not a Ralph never-exit loop.
  - Fixed interval hints: 3–5m while agents run; 10–15m when waiting on deps or PR review (advisory; CLI default remains 5m in the `/loop` prefix).
  - Empty Ready: keep alive ~5h; stop on user request; do not stop on the first empty tick.
  - Literal `TASK` where a task path is mentioned.
- Packet must not expand into claim/owner tutorials or a full tick checklist (status/show/finish belongs in existing orchestrator packets).
- Exact wording is an implement detail within this short-nudge shape.

## Constraints
- Must not add `{interval}`, `{task_file}`, or other format placeholders to this packet; literal `TASK` only.
- Must not embed `/loop` prefix in the packet body (CLI-owned in heartbeat-cli slice).
- Must not add ownership / multi-cursor tutorial prose.
- Must not reprint the full orchestrator tick checklist in this packet (short nudge only).
- Must not add duration policy keys (`max_idle`, TTL fields) to profile or orchestration defaults in v0.
- Must not hardcode packet body in Python (including tests beyond contract presence checks).
- Must not add auto-arm or Ralph loop semantics to the profile.
- Must not require a task file to validate the profile entry.
- Must not implement `render_orchestrator_heartbeat`, `cmd_loop`, skill one-liner, or create CLI echo (sibling slices / out of v0).
- Must not add live `/loop` integration tests in this slice.
- Land in chezmoi harness sources on master; no branch/worktree/PR unless repo convention changes.

## Verification
- `pi-job profile --json` includes non-empty `instruction_packets.orchestrator_heartbeat`.
- Extend `test_profile_requires_slice_plan_stub_and_findings_header` (or adjacent profile contract test) to assert:
  - template contains literal `TASK` (or `TASK` in a command hint);
  - template does not contain `{interval}`, `{task_file}`, or a leading `/loop` prefix;
  - add `orchestrator_heartbeat` to the required-field deletion loop (same pattern as `investigate_interrupt`);
  - deleting the field fails `ProfileDocument.model_validate`.
- Spot-check packet themes manually: short metronome nudge, interval hints, empty-frontier guidance, no ownership tutorial, no full checklist dump.
- `uvx ruff@latest check .` in harness package after Python model change.

## Touch surface
- `dot_local/share/pi-job-harness/profile.yaml` (`instruction_packets.orchestrator_heartbeat`)
- `dot_local/share/pi-job-harness/bin/executable_pi-job` (`InstructionPacketsDocument` field only in this slice)
- `dot_local/share/pi-job-harness/tests/executable_test_pi_job.py` (profile contract assertions)
- applied copy under `~/.local/share/pi-job-harness` via chezmoi apply as needed

## Open questions
- Exact short prose wording (implement detail within the short-nudge shape).

## Decisions
- Product decisions live in the task `decisions` list (three-piece shape; default 5m; TASK placeholder; no ownership tutorial; ~5h idle / stop-on-user in packet only; no duration CLI flags; agents arm own `/loop`).
- Grill: heartbeat packet is a short role + interval/idle nudge; full tick checklist stays in existing orchestrator packets / skill (2026-08-12).
- Grill: no `{interval}` placeholder in profile body; armed cadence is CLI `/loop` prefix only; body keeps fixed advisory ranges (2026-08-12).
