# Global Agent Instructions

Common principles for all agents, across every project.
Project-level `AGENTS.md` files may extend or override these.

## General Guidelines

- For durable multi-step work tracked in a CUE task file, use `pi-job` (installed as `~/.local/bin/pi-job`).
  Start with `pi-job --task <file> status`.
  If the file is missing, follow the scaffold hint.
  Use `plan` for session todos, `instruction` before acting, and `advance` only after evidence.
  Full docs: `~/.local/share/pi-job-harness/README.md` (also `dot_local/share/pi-job-harness/README.md` in this repo).
  If `pi-job` is not installed, follow the agent self-install in `~/.local/share/pi-job-harness/README.md` (or the raw GitHub copy of that README): prefer `uv` to install/pin Python 3.12, curl only the harness files into `~/.local/share/pi-job-harness`, and wrap `~/.local/bin/pi-job` with `uv run` - do not clone the whole dotfiles repo.
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
