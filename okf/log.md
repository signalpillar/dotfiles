# Bundle Update Log

## 2026-10-09

* **Update**: `overview`. Removed Starship from the terminal stack. The zsh prompt is now the zsh default.

## 2026-10-06

* **Update**: `architecture/global-agent-skills`. `skills-global install` no longer feeds the manifest to the skills CLI on stdin. It skips a skill chezmoi already manages.
* **Update**: `skills-global` is a Babashka script. The install and update commands stay the same.

## 2026-10-05

* **Add**: `architecture/global-agent-skills`. Third-party skills are listed in `dot_config/agent-skills/global.txt`. `npx skills add -g` installs them. `skills-global update` refreshes them.

## 2026-10-03

* **Add**: Registered `proj/audio-transcribe` as `architecture/audio-transcribe` with sources for all modules.
* **Add**: Logged the diarization debug chain as `investigations/audio-transcribe-diarization-failures` (six failures: native lib, torch pins, hub flag, weights-only globals, mkv input, gated models).

## 2026-10-02

* **Add**: `investigations/counsel-rg-pipe-stdin.md`. Post-mortem: the macOS `start-process` pipe advice made `counsel-rg` hang because `rg` read stdin. Fix appends `.` to `counsel-rg-base-command` in `dot_spacemacs.d/init.el`.
* **Add**: `investigations/tsgo-inline-completion-null.md`. Post-mortem: lsp-mode sends `inlineCompletion` as JSON null, and tsgo rejects `initialize`. Fix advises `lsp--client-capabilities` in `dot_spacemacs.d/init.el`.

## 2026-09-30

* **Creation**: Established OKF v0.2 bundle at `okf/` with overview, architecture, playbooks and investigations sections.
* **Move**: Moved 6 notes from `docs/` and 9 notes from `dot_spacemacs.d/docs/` into `okf/`. Added frontmatter and `sources` to each.
* **Move**: Notes that describe a managed component went to `architecture/`. Post-mortems and research notes went to `investigations/`. The Parallels guide went to `playbooks/`.
* **Tooling**: `okf-validate` (Babashka, `dot_local/bin/executable_okf-validate`) checks frontmatter, `sources` paths, links and index reachability.
* **Note**: `dot_spacemacs.d/docs/` no longer exists in the source tree. Chezmoi no longer deploys the notes to `~/.spacemacs.d/docs/`.
