---
type: Reference
title: Chezmoi dotfiles overview
description: What this chezmoi source tree manages, its two target environments, and its main toolchains.
tags: [overview, chezmoi, dotfiles]
status: stable
sources:
  - id: readme
    resource: /README.md
    title: Root README
  - id: agents
    resource: /AGENTS.md
    title: Global agent instructions (deployed to ~/AGENTS.md)
  - id: ignore
    resource: /.chezmoiignore
    title: Files that chezmoi does not deploy
  - id: brewfile
    resource: /Brewfile
    title: Homebrew packages for macOS
  - id: mise
    resource: /dot_config/mise/config.toml
    title: Shared language runtimes and CLIs
---

# What it is

This repo manages user configuration for macOS hosts and Ubuntu Linux VMs with chezmoi.
The chezmoi source directory is `~/.local/share/chezmoi`.

# Environments

| Environment | Setup | Notes |
|-------------|-------|-------|
| macOS host | `Brewfile`, `run_onchange_brew.sh.tmpl`, `run_onchange_osx.sh.tmpl` | Homebrew installs tools, apps and Nerd Fonts. Ghostty and Starship are the terminal stack. |
| Linux VM (Ubuntu) | `run_onchange_setup_box.sh.tmpl`, `run_onchange_linux-resource-control.sh.tmpl` | Sway and i3 window managers. Docker environments run through `dockerise/justfile`. |

Mise manages shared runtimes and portable CLIs on both systems.

# Rules

- Chezmoi deploys every non-ignored top-level path to `$HOME`.
  Add repo-only directories, such as `okf`, `linux` and `dockerise`, to `.chezmoiignore`.
- `.chezmoiignore` patterns match target paths, not source paths.
- `AGENTS.md` is the global agent instruction file.
  Chezmoi deploys it to `~/AGENTS.md`, so it applies to every project.
- Do not add `~/.cursor/cli-config.json` or the `~/.omp` directory to chezmoi.
  Both hold machine-local state.

# Toolchains

- Spacemacs (`dot_spacemacs.d`, `dot_emacs.d`) and Doom Emacs (`dot_config/doom`).
- The `pi-job` harness (`dot_local/share/pi-job-harness`) and the `mermaid-validate` tool.
- Custom agent skills in `dot_agents/skills`.
- The `okf-validate` tool (`dot_local/bin/executable_okf-validate`) validates this bundle.
