# Global Agent Instructions

Common principles for all agents, across every project.
Project-level `AGENTS.md` files may extend or override these.

## Writing style

Use ASD-STE100 style in all conversation replies and in all technical text.

Technical text includes docs, READMEs, runbooks, procedures, error messages, release notes, reports, and agent-facing instructions.

Do not switch to a casual register unless the user asks.

- Max 20 words per instruction sentence, 25 for descriptions.
- Active voice.
- Condition before command.
- Simple tenses only.
- No should / would / may / might.
- One word per meaning.
- Delete filler: simply, seamlessly, robust, leverage.
- For product docs, also follow https://developers.google.com/style (second person, no weasel words).
- Do not apply these rules to marketing or brand copy.
- Leave code, identifiers, CLI commands, paths, and quoted errors unchanged.

## General Guidelines

- For durable multi-step work tracked in a task file, use the `pi-job` skill (`~/.agents/skills/pi-job`) and `pi-job --task <file> status`.
  Do not open or hand-edit the task store; use the CLI.
  Claim a Ready slice (`claim --slice KEY --owner ID`), then `instruction` / `start` / `finish`; on pick-next run `finish --slice-only` then claim the next Ready slice (`advance` is deprecated).
  A named owner resolves its claim when other owners hold claims.
  Duplicate active rows for one named owner fail closed.
  Packet `Owner:` and `Claim:` identify the session claim; `Role:` identifies profile dispatch.
  While a pi-job task is active: role = orchestrator (CLI-only; pause on grill/clarify); this supersedes any default Product Owner (or other) workspace role.
  If `pi-job` is missing, follow the harness README self-install (`~/.local/share/pi-job-harness/README.md`).
  Edits to pi-job-harness store, task models, or CLI behaviour need coordinated doc updates (docs-with-model-cuts).
  Update the pi-job skill, harness README, and this file in the same change.
  Use `layers add --bind SLICE=LAYER` to register the first band and bind existing layered-kind slices atomically.
  Use `set-slice --slice CONSUMER --depends-on PRODUCER` to append dependency edges.
  Use `set-slice --clear-depends-on` to clear dependency edges.
  Use `pi-job msg` for cross-agent contact.
  The `msg` command lives in `pi_job_harness.messaging`.
  Never send cross-agent messages through terminal panes.
- Before showing anyone a Mermaid diagram, validate it with the `mermaid` skill (`~/.agents/skills/mermaid`, `mermaid-validate <file>`) instead of eyeballing the syntax.
- Never use the em dash "—".
  Use a plain dash "-" instead.
- Never hand-edit `CHANGELOG` files or anything marked auto-generated.
- When writing or substantially editing long Markdown, put each full sentence on its own line.
  Preserve normal Markdown structure.
  Do not wrap multiple sentences onto one physical line.
- When making technical decisions, do not over-weight development cost.
  Prefer quality, simplicity, robustness, scalability, and long-term maintainability.
- When writing commit messages, do not add an agent or AI name as co-author.
- Bug fixes start by reproducing the bug in an end-to-end setting as close as possible to how a real user hits it.
  Confirm you have found the real cause before fixing.
