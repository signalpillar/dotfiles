# Bundle Update Log

## 2026-10-03

* **Add**: Registered `proj/audio-transcribe` as `architecture/audio-transcribe` with sources for all modules.
* **Add**: Logged the diarization debug chain as `investigations/audio-transcribe-diarization-failures` (six failures: native lib, torch pins, hub flag, weights-only globals, mkv input, gated models).

## 2026-09-30

* **Creation**: Established OKF v0.2 bundle at `okf/` with overview, architecture, playbooks and investigations sections.
* **Move**: Moved 6 notes from `docs/` and 9 notes from `dot_spacemacs.d/docs/` into `okf/`. Added frontmatter and `sources` to each.
* **Move**: Notes that describe a managed component went to `architecture/`. Post-mortems and research notes went to `investigations/`. The Parallels guide went to `playbooks/`.
* **Tooling**: `okf-validate` (Babashka, `dot_local/bin/executable_okf-validate`) checks frontmatter, `sources` paths, links and index reachability.
* **Note**: `dot_spacemacs.d/docs/` no longer exists in the source tree. Chezmoi no longer deploys the notes to `~/.spacemacs.d/docs/`.
