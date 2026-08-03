# Markdown task preview

## Intent

Provide a read-only `markdown` subcommand that turns any supported pi-job task store into a portable Markdown preview on standard output.
Keep rendering separate from storage and command dispatch so the behavior is straightforward to test and extend.

## System behaviour

- `pi-job --task <path> markdown` loads and validates the task through the existing `TaskStore` interface, then writes Markdown to stdout only.
- Uninitialized tasks (no orchestration) are still previewable when the document validates.
- Document order: title and status, project, prominent Decisions (dated bullets; `_none_` when empty), then context and remaining metadata, then slices/steps.
- Omit empty optional sections (blank context, empty repos/deps/artifacts/step notes, empty final_steps).
- Prefer Markdown when recording notes and decisions.
  Decisions and nested notes render as blockquotes; context and plan notes render as Markdown prose.
  Escape title/heading text so structural labels stay intact.
- Mark the cursor slice and step inline with a `(current)` badge; do not put orchestration in a separate appendix.
- Default slice order is plan order.
- Optional `--chronological` sorts slices oldest-changed-first.
  A slice's change time is the earliest non-empty `execution.started` or `execution.ended` on the slice or any of its steps/final_steps.
  Slices with no such timestamps sort after timestamped ones; equal timestamps keep plan order.
- Invalid or missing stores use existing CLI errors on stderr and exit non-zero.
- Preview never mutates the task store, digest, timestamps, cursor, or repository.

## Constraints

- Successful stdout is Markdown only - no status preamble, ANSI, or backend serialization details.
- Output is deterministic for the same validated task state and sort mode across supported backends.
- Reuse canonical task models and shared helpers; do not duplicate backend parsing.
- Only `--chronological` is in scope beyond the required `--task` global; no other markdown flags in v1.

## Touch surface

- CLI parser and command dispatch in `dot_local/share/pi-job-harness/bin/executable_pi-job`.
- End-to-end behavior tests in `dot_local/share/pi-job-harness/tests/executable_test_pi_job.py`.
- User documentation and examples in `dot_local/share/pi-job-harness/README.md`.

## Verification

- End-to-end tests for representative and minimal tasks, including decisions prominence and `(current)` badges.
- Assert heading order, omit-empty policy, fencing/escaping, and chronological sort flag behavior.
- Capture stdout/stderr separately for success and validation failures.
- Hash or byte-compare the source task before and after previewing to prove read-only.
- Exercise supported store backends where fixtures exist.
- Run the harness test suite and Ruff checks.
