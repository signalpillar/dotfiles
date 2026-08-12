# Intent

Make pi-job a YAML and directory-store task harness.
CUE task files are unsupported and rejected at the storage-selection boundary.
Remove the CUE implementation, schema, migration command, CUE-specific tests, and active documentation.
Keep the README's CUE origin only as a short historical lesson, without migration instructions.
The lesson centers on mutation pain: pi-job updated CUE via fragile regex rewrites.

## Types and composition

```text
TaskStore :=
    YamlTaskStore(task: *.yaml | *.yml)
  | FsTaskStore(task: existing directory)

open_task_store(path) -> TaskStore | die
project(source: TaskStore, destination: TaskStore) -> None
```

`YamlTaskStore` remains the machine-owned file backend.
`FsTaskStore` remains the experimental directory backend and continues to implement the common `TaskStore` protocol.
`project` retains only YAML-to-directory and directory-to-YAML projections.
No CUE type, parser, formatter, schema, subprocess call, or CUE-derived helper participates in the runtime.

## Call stacks

```text
CLI command
  -> require task path
    -> open_task_store(path)
      -> .yaml/.yml: YamlTaskStore
      -> existing directory: FsTaskStore
      -> .cue or any other path: clear unsupported-storage failure
  -> store.read()/mutation
  -> Pydantic validation and YAML or filesystem persistence
```

Tests invoke the executable through subprocess and import it only to exercise the YAML and filesystem stores directly.

## System behaviour

- Existing YAML and directory tasks retain their current read, validation, mutation, and projection behaviour.
- `--task anything.cue` and `project --to *.cue` hard-fail with a short unsupported-storage message (use `.yaml`/`.yml` or a directory) before any CUE CLI invocation or task mutation.
- `create` accepts only YAML task-file destinations.
- `project --to` accepts only YAML destinations or directory destinations.
- `migrate-task` is absent from CLI parsing and help.
- `schema` and `validate` describe Pydantic/YAML and directory validation only.
- The package has no `task-schema.cue` file and no runtime dependency on the `cue` executable.

## Constraints

- Must implement directly on `master` for user review, with no worktree or feature branch.
- Must skip ticket and PR; user reviews the master diff.
- Must delete `CueTaskStore`, `task-schema.cue`, CUE dispatch, `cue export`/`cue fmt` paths, scaffold/rendering code, local-schema diagnostics, and every helper used only by those paths.
- Must remove `cue_escape` rather than retain an unused compatibility helper.
- Must remove CUE fixtures, allowlists, round-trip coverage, and CUE entries from the manual test runner.
- Must add focused negative coverage that `.cue` task and project-destination paths are rejected without requiring CUE.
- Must update active README installation, backend, command, and migration text to YAML plus directory-store reality.
- Must state in one short README paragraph that CUE was the first store and the main pain was regex-based updates from pi-job; no migration docs.
- Must not document a CUE migration path.
- Must not change `AGENTS.md` or `profile.yaml` unless a verified active CUE claim exists.
- Must not edit the pi-job task YAML store during this slice.

## Verification

- Run the harness regression suite from `dot_local/share/pi-job-harness/`.
- Run `uvx ruff@latest check .` from the harness directory.
- Exercise `create`, `validate`, and a representative mutation on a YAML task.
- Exercise YAML to directory and directory to YAML `project` round trips with semantic equality.
- Assert `.cue` input and `.cue` `project --to` produce the defined unsupported-storage failure and do not invoke CUE.
- Search the harness source, tests, and active README for `CueTaskStore`, `task-schema.cue`, `migrate-task`, and live `.cue` support references; permit only the bounded README historical note.

## Touch surface

- `dot_local/share/pi-job-harness/bin/executable_pi-job`
- `dot_local/share/pi-job-harness/task-schema.cue` (delete)
- `dot_local/share/pi-job-harness/tests/executable_test_pi_job.py`
- `dot_local/share/pi-job-harness/README.md`
