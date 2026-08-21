"""CLI output edge: stdout or a file, not both."""

from __future__ import annotations

from pathlib import Path


def emit_output(body: str, out_path: str | None) -> None:
    """Write `body` to PATH when out_path is set; otherwise print to stdout.

    When writing a file, do not also print the body.
    """
    text = body if body.endswith("\n") else body + "\n"
    if out_path:
        path = Path(out_path).expanduser()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return
    print(text, end="")
