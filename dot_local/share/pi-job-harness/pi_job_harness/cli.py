"""Console entry for `pi-job`."""

from __future__ import annotations

import sys
from pathlib import Path

from pi_job_harness.layout import PiJobLayout


def _ensure_repo_on_path() -> None:
    root = Path(__file__).resolve().parent.parent
    if str(root) not in sys.path:
        sys.path.insert(0, str(root))


def main(
    argv: list[str] | None = None,
    *,
    layout: PiJobLayout | None = None,
) -> int:
    _ensure_repo_on_path()
    from pi_job_harness.app import main as app_main

    if argv is not None:
        sys.argv = [sys.argv[0], *argv]
    app_main(layout=layout)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
