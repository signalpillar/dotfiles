"""Small text helpers used by stores and CLI mutations."""

from __future__ import annotations

from datetime import UTC, datetime


def utc_now() -> str:
    return datetime.now(UTC).isoformat().replace("+00:00", "Z")


def merge_note(existing: str, addition: str, *, replace: bool = False) -> str:
    if replace:
        return addition
    if not existing:
        return addition
    return existing + "\n\n" + addition

