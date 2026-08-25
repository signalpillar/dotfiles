"""Durable mailbox message records and serialization."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path

import yaml


@dataclass(frozen=True)
class Message:
    """One durable mailbox message. Unread state comes from its directory."""

    # Stable file identifier used for acknowledgement.
    id: str
    # Canonical recipient label: manager or slice:KEY.
    to: str
    # Provenance label recorded by the sending command.
    from_: str
    # UTC send timestamp from the message front matter.
    sent_at: str
    # Markdown payload after the front matter.
    body: str
    # Source file. This remains useful when front matter is damaged.
    path: Path
    # True when the source file is in the new directory.
    unread: bool

    @classmethod
    def from_text(cls, text: str, *, path: Path, unread: bool) -> Message:
        """Parse front matter without letting damaged messages disappear."""
        empty = cls(
            id=path.stem,
            to="",
            from_="",
            sent_at="",
            body="",
            path=path,
            unread=unread,
        )
        try:
            if not text.startswith("---\n"):
                return empty
            header_text, separator, body = text[4:].partition("\n---\n")
            if not separator:
                return empty
            header = yaml.safe_load(header_text)
            if not isinstance(header, Mapping):
                return empty
            return cls(
                id=path.stem,
                to=str(header.get("to") or ""),
                from_=str(header.get("from") or ""),
                sent_at=str(header.get("sent_at") or ""),
                body=body.lstrip("\n"),
                path=path,
                unread=unread,
            )
        except (TypeError, ValueError, yaml.YAMLError):
            return empty


def render_message_entry(
    *,
    ident: str,
    to: str,
    sender: str,
    sent_at: str,
    note: str,
) -> str:
    """Render one Markdown message file with YAML front matter."""
    header = yaml.safe_dump(
        {"id": ident, "to": to, "from": sender, "sent_at": sent_at},
        allow_unicode=True,
        sort_keys=False,
    ).rstrip()
    return f"---\n{header}\n---\n\n{note.rstrip()}\n"
