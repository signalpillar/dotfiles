# cursor-model-cli

## Goal
Replace orchestration.cursor with owned cursors[]; add claim/release; wire --owner and PI_JOB_OWNER through start/finish/advance/instruction; auto-release on slice terminal; stale via last_seen with 24h profile TTL; update status/show. Land on master; no branch/worktree/PR.

## Depends on
(none)

## Intent
- Make parallel agents safe on one task file by owning Ready slices through explicit claims.
- Keep within-slice step walking sequential under that claim; derive the active step, do not store it.
- Hard-cut the store off a single saved cursor.

## Types and composition
```ts
type OwnerId = string // agent-chosen; or PI_JOB_OWNER

type OwnedCursor = {
  owner: OwnerId
  slice: string
  claimed_at: string // UTC ISO
  last_seen: string  // UTC ISO; bumped on mutating cmds for this owner
}

type Orchestration = {
  cursors: OwnedCursor[] // replaces cursor
  policy: ...
  artifacts: ...
  content_digest: string | null
}

// derived, not persisted:
// activeStep(claim) = first non-terminal step in claim.slice (steps then final_steps)
```

Boundaries:
- `YamlTaskStore.exclusive()` remains the atomicity boundary for claim/release/displace.
- Claim eligibility uses existing Ready frontier rules + non-stale occupancy.
- Stale is a derived view (`now - last_seen > TTL`), not a persisted enum (status may print a flag).
- Active step is derived from slice step statuses; claim never stores `step`.

## Call stacks
```ts
claim(--owner,--slice)
  -> resolveOwner(cli|env)
  -> exclusive()
    -> load task
    -> assert owner has no other claim
    -> assert Ready(slice) && (no active claim || claim.stale)
    -> upsert OwnedCursor{owner,slice,now,now}
    -> write

start|finish|instruction(--owner?)
  -> resolveOwner(cli|env|sole-claim)
  -> if ambiguous (multiple claims, no owner): refuse
  -> findCursor(owner) or refuse
  -> activeStep = firstNonTerminal(cursor.slice)
  -> bump last_seen
  -> existing step mutation / packet render
  -> if finish made slice terminal: drop that owner's claim

release(--owner)
  -> exclusive(); remove claim; leave slice status untouched
```

Agent loop (no advance): status/show → claim → instruction/start/finish until slice terminal → auto-release → claim next Ready.

## System behaviour
- One owner, one active claim, one whole slice. A second claim by the same owner refuses until the first is released or auto-released.
- `claim` before `start`; no silent claim inside `start`.
- Active step is always the claimed slice's first non-terminal step; no `advance` in the agent loop.
- Auto-release when claimed slice becomes `done` or `skipped`.
- Mid-slice `release --owner` abandons claim only; slice becomes claimable again.
- Stale claim may be displaced by a new `claim`; surface displacement in CLI output.
- Any caller may `release --owner` (not self-only).
- Default TTL 24h from profile; no heartbeat daemon.
- Prefer `--owner` / `PI_JOB_OWNER`, but omit is allowed when the claim is unambiguous.
- Existing tasks with `orchestration.cursor` are not migrated; stop using that field.

## Constraints
- Must not require `advance` after finish; remove or leave advance unused pending cleanup.
- Must not store `step` on the claim.
- Must not keep a single-cursor read shim or migrate-task converter.
- Must not invent auto owner ids (cwd/ppid) in v1.
- Must not require branches, worktrees, or PRs; edit harness on master.
- Must not allow two non-stale claims on the same slice.
- Must not allow two active claims for the same owner.
- Prefer `--owner` / `PI_JOB_OWNER`, but allow omit when unambiguous; refuse when ambiguous.

## Verification
- Unit/regression tests in `tests/executable_test_pi_job.py` covering: claim Ready ok; claim non-Ready refuse; double claim refuse; one-owner-two-slices refuse; stale displace; release; auto-release on slice terminal; owner omit with one claim / refuse when ambiguous; last_seen bump; derived active step; status/show list cursors with stale flag.
- `uvx ruff@latest check .` in harness package.
- Manual smoke: two owners claim two independent Ready slices on a fixture task without clobbering each other.

## Touch surface
- `dot_local/share/pi-job-harness/bin/executable_pi-job` (models, cmds, status/show/instruction)
- `dot_local/share/pi-job-harness/profile.yaml` (TTL default)
- `dot_local/share/pi-job-harness/tests/executable_test_pi_job.py`
- applied copy under `~/.local/share/pi-job-harness` via chezmoi apply as needed

## Open questions
- (none blocking)

## Decisions
- Product decisions live in the task `decisions` list (including SUPERSEDES for claim shape without step; no advance in agent loop).
