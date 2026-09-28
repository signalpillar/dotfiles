# pi-job-viewer

Read-only end-user viewer for pi-job task bundles.
FastAPI plus htmx fragments, Adwaita-style skin, Markdown rendering, Mermaid graph.

## Run

```bash
pi-job-serve --port 8137
pi-job-serve --task my-slug --port 8137
```

Open `http://127.0.0.1:8137/`.

## Install

Chezmoi applies `run_onchange_install-pi-job-viewer.sh.tmpl`.
That script runs an editable `uv tool install` of this package.
By hand:

```bash
uv tool install --force --editable ~/.local/share/pi-job-viewer
```

The viewer imports the installed `pi_job_harness` for reads.
Install `pi-job` first (harness README covers that).

## Verify

From this directory:

```bash
uvx ruff@latest check .
uv run --with pytest --with httpx python -m pytest
```

## Scope

GET routes only.
No mutations, no locks, no digest writes.
Harness code, skill files, and `AGENTS.md` stay untouched.
