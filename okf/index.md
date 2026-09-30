---
okf_version: "0.2"
---

# Chezmoi dotfiles

Knowledge bundle for this chezmoi source tree.
The tree manages user configuration for macOS hosts and Linux VMs.
Concept IDs are bundle-relative paths without `.md` (for example `architecture/emacs-fonts`).
Each concept lists the repo files it describes in `sources`.
`sources` paths are chezmoi source-state paths, such as `/dot_zshrc`.

# Overview

* [Overview](overview.md) - What the repo manages, the two environments, and the main toolchains.

# Architecture

* [architecture/](architecture/index.md) - How managed parts are built and why: Emacs fonts, early-init, terminal speed layer, Linux resource control.

# Playbooks

* [playbooks/](playbooks/index.md) - Step-by-step procedures.

# Investigations

* [investigations/](investigations/index.md) - Post-mortems and research notes from real debug sessions.

# Log

* [Bundle update log](log.md)
