---
type: Architecture
title: "Global agent skills"
description: "The skills CLI installs third-party skills globally. Chezmoi declares the list. You run the update."
tags: [agents, skills, chezmoi]
status: stable
sources:
  - id: agent-skills-global-txt
    resource: /dot_config/agent-skills/global.txt
  - id: executable-skills-global
    resource: /dot_local/bin/executable_skills-global
  - id: run-onchange-install-agent-skills
    resource: /run_onchange_install-agent-skills.sh.tmpl
  - id: dot-agents-skills-test-audit
    resource: /dot_agents/skills/test-audit/SKILL.md
---

# Global agent skills

Agents read global skills from `~/.agents/skills/<name>/SKILL.md`.

## Owned skills

Put skills written for this repo in `dot_agents/skills/`.
`chezmoi apply` copies them to `~/.agents/skills/`.

## Third-party skills

The [skills CLI](https://github.com/vercel-labs/skills) installs upstream skills.
It writes one canonical copy under `~/.agents/skills/<name>/`.
It symlinks that copy into each detected agent, including `~/.claude/skills/`.
It records the source in `~/.agents/.skill-lock.json`.
Chezmoi does not track the copy or the lock.

List each skill in `dot_config/agent-skills/global.txt` as `source skill`.
`chezmoi apply` runs `npx skills add <source> --skill <name> -g -y` for each line.
The `skills-global` command is a Babashka script.
The installer gives the skills CLI the terminal, so the CLI does not read the manifest.
It skips a skill when chezmoi already manages `~/.agents/skills/<name>`.
Run `skills-global update` to refresh every global skill in the lock.
Run `skills-global update <name>` to refresh one skill.

Add a line to the manifest and run `chezmoi apply` to install another skill.
Remove a line, then run `npx skills remove -g <name>`, to drop one.
