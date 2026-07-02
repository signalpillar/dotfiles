# Global Agent Instructions

Common principles for all agents, across every project.
Project-level `AGENTS.md` files may extend or override these.

## General Guidelines

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
