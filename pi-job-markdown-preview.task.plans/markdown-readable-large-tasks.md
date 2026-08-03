# Readable large-task markdown previews

## Intent

Keep `pi-job markdown` useful when a task file is large.
Add compact and slice-scoped modes while keeping today's full dump as the default.

## System behaviour

- Default: full preview (unchanged).
- `--summary`: title/status, project, Decisions, Contents, then each slice as header + goal only (no steps, notes, repo_work, context, source, artifacts, or plan note).
- `--slice KEY`: title/status, project, Decisions, Contents for that slice only, then that slice in full detail. Unknown keys fail closed like `show --slice`.
- `--summary` and `--slice` are mutually exclusive.
- `--chronological` still reorders whichever slice set is shown.
- Output stays Markdown-only on stdout; still read-only.

## Constraints

- Do not weaken Decisions prominence in any mode.
- Prefer these two flags only; no collapsed-notes flag in this slice.
- Nested Markdown notes stay blockquotes when shown (full / `--slice`).
- Tests cover `--summary`, `--slice`, mutual exclusion, full dump still works, read-only.

## Touch surface

- `dot_local/share/pi-job-harness/bin/executable_pi-job` markdown renderer/CLI
- `dot_local/share/pi-job-harness/tests/executable_test_pi_job.py`
- `dot_local/share/pi-job-harness/README.md`

## Verification

- E2E for `--summary` and `--slice` on a multi-slice fixture.
- E2E that default full dump still includes steps/notes.
- Mutual exclusion error on stderr, non-zero exit.
- Read-only hash check still passes.
- Ruff + markdown test subset green.
