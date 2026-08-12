# heartbeat-skill

## Goal
Add one line to the pi-job skill after create/cold-start pointing at `pi-job loop` (agents arm their own `/loop`). Update chezmoi `dot_agents/skills/pi-job/SKILL.md`.

## Depends on
heartbeat-cli

## Intent
- Close the v0 three-piece delivery: profile packet + CLI printer + skill pointer.
- After create or cold-start, remind the orchestrator to arm the manager metronome via `pi-job loop`.
- Skill stays thin: one new line only; do not restate packet prose or loop-skill mechanics.
- Agent copies printed output, resolves `TASK` to the real `--task` path, and arms `/loop` itself.

## Types and composition
```ts
// No new types. Markdown skill edit only.

// SKILL.md structure (existing):
## Cold start
  ... existing bullets ...
  + ONE new line: run `pi-job loop`; arm printed `/loop …` (resolve TASK)

// Boundaries:
// - Wording lives in profile.yaml (orchestrator_heartbeat)
// - Printable line lives in `pi-job loop` (heartbeat-cli)
// - Skill only bridges cold-start → loop command
```

Boundaries:
- Edit is limited to `dot_agents/skills/pi-job/SKILL.md` in chezmoi (applied to `~/.agents/skills/pi-job/SKILL.md`).
- Do not duplicate interval policy, empty-frontier rules, or Ready fan-out checklist in the skill.
- Do not document Cursor loop skill sleep/watch mechanics here.
- Do not add a new skill file; extend the existing pi-job skill only.

## Call stacks
```ts
orchestrator cold start / post-create
  -> read SKILL.md Cold start section
  -> pi-job --task TASK_FILE status   (existing)
  -> pi-job loop                      (new one-liner)
  -> user/agent pastes `/loop …` into Cursor with TASK resolved
  -> Cursor loop skill runs tick prompt on interval
```

## System behaviour
- The new line appears in the **Cold start** section immediately after the existing create/instruction "enter orchestrator loop" guidance (or adjacent to it without restructuring the doc).
- Line instructs: run `pi-job loop`, copy the printed `/loop …` line, replace literal `TASK` with the task file path, arm in Cursor.
- Existing orchestrator loop numbered steps remain unchanged; the one-liner is metronome arming, not a replacement for claim/instruction/finish flow.
- No changes to skill front matter, role rules, or harness Python.

## Constraints
- Must add exactly one substantive line (a short sentence or bullet); no new section.
- Must not restate profile packet content (interval hints, ~5h idle, ownership, fan-out details).
- Must not instruct auto-arm or harness-managed timers.
- Must not require PR/branch/worktree for chezmoi dotfiles (land on master like multi-cursor skill updates).
- Must not create `dot_agents/skills/loop/` or merge loop-skill content into pi-job skill.
- Must reference `pi-job loop`, not raw profile keys.

## Verification
- Read `dot_agents/skills/pi-job/SKILL.md` Cold start section: exactly one new line about `pi-job loop` and manual `/loop` arming with `TASK` resolution.
- Confirm no duplicate paragraphs from profile or README were pasted into the skill.
- Manual smoke: follow Cold start + new line on a fixture task; `pi-job loop` output is armable after `TASK` substitution.
- chezmoi diff shows only the intended skill path changed in this slice.

## Touch surface
- `dot_agents/skills/pi-job/SKILL.md` (one line in Cold start)

## Open questions
- Exact one-line wording (implement detail; keep under ~25 words; point at `pi-job loop` and TASK resolution).
- Whether the line is a bullet under Cold start or a sentence appended to the existing "enter orchestrator loop" bullet (prefer minimal diff: single new bullet).

## Decisions
- Product decisions live in the task `decisions` list (skill one-liner after create; agents arm own `/loop`; no auto-arm; TASK placeholder).
