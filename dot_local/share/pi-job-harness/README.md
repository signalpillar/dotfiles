# pi-job-harness

Portable deterministic job harness for CUE task files.

`pi-job` keeps the CUE task file as the source of truth. It reads `task` with `cue export`, loads package-local `profile-contract.cue` for valid profiles and phase ownership, requires an explicit profile before execution, computes the next unfinished slice/step, updates the orchestration cursor, prints the profile phase plan, and emits deterministic instruction packets for the orchestrating model.

## Contents

```text
bin/pi-job              CLI entrypoint
profile-contract.cue    portable profile/phase/artifact contract
tests/test_pi_job.py     regression tests for the CLI
```

## Dependencies

- Python 3.11+
- `cue` CLI on `PATH`

No third-party Python packages are required.

## Install

Copy this directory anywhere, for example into chezmoi:

```bash
~/.local/share/chezmoi/dot_local/share/pi-job-harness/
```

Then either add `bin/` to `PATH` or symlink the executable:

```bash
ln -s ~/.local/share/pi-job-harness/bin/pi-job ~/.local/bin/pi-job
```

When kept inside this repo, `scripts/pi-job` is a thin wrapper around `packages/pi-job-harness/bin/pi-job`.

## Usage

Run from the repository that owns the task file. The current working directory is reported as the repository root in instruction packets.

```bash
pi-job --task projects/example/tasks/task.cue status
pi-job --task projects/example/tasks/task.cue init --profile small
pi-job --task projects/example/tasks/task.cue plan
pi-job --task projects/example/tasks/task.cue next
pi-job --task projects/example/tasks/task.cue instruction --current
pi-job --task projects/example/tasks/task.cue advance
```

A task with `Profile: <unset>` is not initialized. Run `init --profile <profile>` before `plan`, `next`, `advance`, or `instruction`; those commands fail closed until a valid profile is recorded.

`plan` prints the selected profile's phase order from `profile-contract.cue` and tells the orchestrator to track those phases with session todos. `instruction` reminds the orchestrator to keep the current cursor todo in progress until validators pass. `next`/`advance` still walk `task.plan` slices; phase-cursor orchestration is deferred.

## Profiles

V0 profiles:

- `small`
- `full`
- `research`
- `review-only`
- `share-only`
- `spike-prototype`

The machine-readable profile and artifact rules are in `profile-contract.cue`. `pi-job` loads that file at runtime via `cue export`; it does not hardcode the profile set or phase owners.

## Test

From a checkout containing this package:

```bash
packages/pi-job-harness/tests/test_pi_job.py
cue eval packages/pi-job-harness/profile-contract.cue >/dev/null
```
