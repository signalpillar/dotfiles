# Global Agent Instructions

Common principles for all agents, across every project.
Project-level `AGENTS.md` files may extend or override these.

## General Guidelines

- For durable multi-step work tracked in a task file, use the `pi-job` skill (`~/.agents/skills/pi-job`) and `pi-job --task <file> status`.
  Do not open or hand-edit the task store; use the CLI.
  While a pi-job task is active: role = orchestrator (CLI-only; pause on grill/clarify); this supersedes any default Product Owner (or other) workspace role.
  If `pi-job` is missing, follow the harness README self-install (`~/.local/share/pi-job-harness/README.md`).
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
