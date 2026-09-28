"""`pi-job-serve` entry point: serve the read-only viewer over HTTP."""

from __future__ import annotations

import argparse
from pathlib import Path

import uvicorn

from pi_job_viewer.app import create_app


def build_parser() -> argparse.ArgumentParser:
    """CLI flags: bind host, port, tasks home, and optional single-bundle mode."""
    parser = argparse.ArgumentParser(description="Read-only viewer for pi-job task bundles.")
    parser.add_argument("--host", default="127.0.0.1", help="Bind host (default 127.0.0.1).")
    parser.add_argument("--port", type=int, default=8137, help="Bind port (default 8137).")
    parser.add_argument("--tasks-home", default=None, help="Task home (default $PI_JOB_TASKS).")
    parser.add_argument("--task", default=None, help="Pin the viewer to one bundle slug.")
    return parser


def main() -> None:
    """Launch uvicorn with the viewer app (never mutates task state)."""
    args = build_parser().parse_args()
    if not 1 <= args.port <= 65535:
        build_parser().error("--port must be 1-65535")
    home = Path(args.tasks_home).expanduser() if args.tasks_home else None
    if home is not None and not home.is_dir():
        build_parser().error(f"--tasks-home is not a directory: {home}")
    if args.host not in ("127.0.0.1", "localhost", "::1"):
        # No auth exists; say so loudly before exposing task text to a network.
        print(f"pi-job-serve: WARNING: no authentication; binding beyond loopback on {args.host}")
    app = create_app(home, only_slug=args.task)
    uvicorn.run(app, host=args.host, port=args.port)


if __name__ == "__main__":
    main()
