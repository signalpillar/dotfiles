# Dotfiles and System Configuration

This repository manages user configuration across macOS and Linux virtual machines with chezmoi.

## Architecture

The repository manages two environments:

### macOS Host

- Homebrew manages command-line tools, applications, and Nerd Fonts through [Brewfile](Brewfile).
- The script [run_onchange_brew.sh.tmpl](run_onchange_brew.sh.tmpl) runs `brew bundle install` when the Brewfile hash changes.
- The script [run_onchange_osx.sh.tmpl](run_onchange_osx.sh.tmpl) configures macOS system defaults and creates the Emacs application symlink.
- Ghostty serves as the primary terminal emulator with Starship prompt.

### Linux VM (Ubuntu)

- The script [run_onchange_setup_box.sh.tmpl](run_onchange_setup_box.sh.tmpl) installs system tools, desktop fonts, and development runtimes.
- The script [run_onchange_linux-resource-control.sh.tmpl](run_onchange_linux-resource-control.sh.tmpl) installs cgroup memory caps, systemd-oomd swap watch, and zram.
- Window managers include Sway and i3.
- Docker container environments run through the [dockerise/justfile](dockerise/justfile).

## Repository Structure

- [dot_agents/skills/](dot_agents/skills/): Custom agent skills.
- [dot_config/](dot_config/): Configurations for Doom Emacs, i3, Sway, Kitty, Yazi, and Mise.
- [dot_cursor/](dot_cursor/): Cursor CLI status line integration.
- [dot_local/bin/](dot_local/bin/): User executable scripts.
- [dot_local/share/](dot_local/share/): Standalone harnesses and toolchains.
- [dot_spacemacs.d/](dot_spacemacs.d/): Spacemacs configuration files.
- [bin/](bin/): Utility scripts for tmux, clipboard, and macOS apps.
- [docs/](docs/): Internal technical notes and post-mortems.
- [Brewfile](Brewfile): Declarative package specification for macOS.

## Built-in Toolchains and Agent Tools

### pi-job harness

The repository includes a deterministic job harness for YAML task files.

- Source: [dot_local/share/pi-job-harness/](dot_local/share/pi-job-harness/)
- Installed path: `~/.local/share/pi-job-harness/`
- PATH install: [run_onchange_install-pi-job.sh.tmpl](run_onchange_install-pi-job.sh.tmpl) owns `~/.local/bin/pi-job` through an editable `uv tool install`
- Documentation: [dot_local/share/pi-job-harness/README.md](dot_local/share/pi-job-harness/README.md)

### mermaid-validate

This tool runs parse-only validation against Mermaid diagrams using the official grammar parser.

- Source: [dot_local/share/mermaid-validate/](dot_local/share/mermaid-validate/)
- Installed path: `~/.local/share/mermaid-validate/`
- CLI wrapper: [dot_local/bin/executable_mermaid-validate](dot_local/bin/executable_mermaid-validate) -> `~/.local/bin/mermaid-validate`
- Documentation: [dot_local/share/mermaid-validate/README.md](dot_local/share/mermaid-validate/README.md)

### Agent Instructions and Skills

Global agent instructions reside in [AGENTS.md](AGENTS.md).
Claude Code configuration symlinks to this file through [dot_claude/symlink_CLAUDE.md.tmpl](dot_claude/symlink_CLAUDE.md.tmpl).
Custom agent skills reside in [dot_agents/skills/](dot_agents/skills/).

### Cursor CLI Status Line

The script [dot_cursor/executable_statusline.sh](dot_cursor/executable_statusline.sh) provides a status line for Cursor agent sessions.
The setup script [run_onchange_cursor-statusline.sh](run_onchange_cursor-statusline.sh) merges the status line key into `~/.cursor/cli-config.json`.
Do not add `~/.cursor/cli-config.json` directly to chezmoi.

### Oh My Pi

Managed file: [dot_omp/private_agent/private_config.yml](dot_omp/private_agent/private_config.yml).
This is `~/.omp/agent/config.yml`.
The custom status line pins `session` (resume id) and `cost` on the right.
Do not add the `~/.omp` directory. Sessions, caches, and sqlite files stay local.

## Shell, Editors, and Tools

- **Shell**: Zsh with Starship prompt, Zoxide, FZF, and Direnv.
- **Runtimes**: Mise manages language runtimes such as Node, Bun, Go, and Babashka.
- **Emacs**: Doom Emacs in [dot_config/doom](dot_config/doom) and Spacemacs in [dot_spacemacs.d](dot_spacemacs.d).
- **Neovim**: LazyVim starter configuration.
- **Multiplexer**: Tmux with TPM plugins installed by [run_once_install-tmux-plugins.sh](run_once_install-tmux-plugins.sh).

### Homebrew and mise

Darwin apply uses [Brewfile](Brewfile) plus [run_onchange_brew.sh.tmpl](run_onchange_brew.sh.tmpl).
Mise owns shared language versions and portable CLIs through [dot_config/mise/config.toml](dot_config/mise/config.toml).
Both OS load that mise file.
`mise activate` in [dot_zshrc](dot_zshrc) prepends shims after Homebrew `shellenv`, so overlapping tools follow mise.

Known overlaps: `fd`, `jq`, `yazi`, `go`, `direnv`, `kubectl`, `1password-cli`, `antigravity-cli`.
Pin tools that lack a short name in the installed mise registry with an explicit backend (`aqua:`, `github:`).
Brew keeps a host fallback.
Mise keeps the versioned shim.

## Manual Applications

When you configure a new machine, install these macOS applications manually:

- [AltTab](https://alt-tab-macos.netlify.app/)
- [Parallels Desktop](https://www.parallels.com/)
- [Dropbox](https://www.dropbox.com/)
- [Slack](https://slack.com/)
- [Proxyman](https://proxyman.io/)

Homebrew casks manage all other desktop applications and Nerd Fonts.

## Documentation Guides

- [Parallels Host-Reachable IP Guide](docs/parallels-host-reachable-ip.md)
- [Tmux Choose-Tree Activity Guide](docs/tmux-choose-tree-activity.md)
- [Linux Resource Control Guide](docs/linux-resource-control.md)
- [eza, ls aliases, and the mise registry](docs/eza-ls-mise-registry.md)

## History

- **June 2026**: Replaced nix-darwin with Homebrew and chezmoi on macOS.
- **September 2025**: Standardized development environment on Ubuntu virtual machines.
- **June 2023**: Migrated to nix-shell and nix-darwin.
- **January 2020**: Restructured dotfiles to use chezmoi.
- **May 2015**: Adopted Spacemacs with Evil mode.
