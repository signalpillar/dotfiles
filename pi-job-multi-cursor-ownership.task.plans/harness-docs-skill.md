# harness-docs-skill

## Goal
Update README, profile, and pi-job skill orchestrator loop for multi-cursor claim/release/owner binding. Depends on cursor-model-cli. Land on master; no branch/worktree/PR.

## Depends on
cursor-model-cli

## Intent
- Teach orchestrators and agents the claim → work → release loop.
- Keep docs aligned with the hard-cut multi-cursor store (no single-cursor language).

## Types and composition
```ts
// docs/skill surface only — behaviour owned by cursor-model-cli
type AgentLoop = {
  claim: "pi-job claim --slice KEY --owner ID"
  work: "start|instruction|finish|advance with --owner or PI_JOB_OWNER"
  release: "auto on slice terminal | pi-job release --owner ID"
}
```

Boundaries:
- Skill describes CLI usage; does not re-implement store rules.
- Profile holds TTL default and any packet wording for owner-bound steps.

## Call stacks
```ts
orchestrator session
  -> status/show (lists cursors + stale)
  -> claim Ready slice
  -> instruction/start/finish/advance --owner
  -> auto-release or explicit release
  -> pick next Ready / fan out other owners
```

## System behaviour
- Document `--owner` / `PI_JOB_OWNER`, claim eligibility, stale displacement, release authority.
- Update README orchestration section: `cursors[]` not `cursor`.
- Update pi-job skill orchestrator loop: claim before start; owner on mutations.
- Skip share-with-team / worktree / PR guidance for this effort's delivery notes where it would contradict master-only landing.

## Constraints
- Must not document a compatibility shim or migrate path for old `cursor`.
- Must not document auto owner-id helpers as if shipped.
- Must land on master with cursor-model-cli behaviour already present.

## Verification
- README + skill + profile wording reviewed against task decisions.
- `pi-job --help` / claim/release help snippets match docs.
- Spot-check: cold-start skill instructions still work for a single agent (one owner).

## Touch surface
- `dot_local/share/pi-job-harness/README.md`
- `dot_local/share/pi-job-harness/profile.yaml`
- `~/.agents/skills/pi-job/SKILL.md` (and chezmoi source if mirrored)

## Open questions
- (none blocking)

## Decisions
- Inherits product decisions from the task; docs must not invent new policy.
