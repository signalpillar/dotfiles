"""Private mailbox filesystem operations."""

from __future__ import annotations

import os
import re
from pathlib import Path
from uuid import uuid4

from pi_job_harness.messaging.message import Message, render_message_entry
from pi_job_harness.messaging.paths import MailboxPaths
from pi_job_harness.store.text import utc_now
from pi_job_harness.store.yaml_io import atomic_write_text


def send_message(
    paths: MailboxPaths,
    *,
    slug: str,
    to: str,
    note: str,
    sender: str,
) -> Path:
    """Write one unique mailbox file without taking the task lock."""
    sent_at = utc_now()
    compact = re.sub(r"[-:]", "", sent_at.split(".", 1)[0].removesuffix("Z"))
    ident = f"{compact}Z-{uuid4().hex}"
    path = paths.message_file(slug, ident=ident)
    atomic_write_text(
        path,
        render_message_entry(
            ident=ident,
            to=to,
            sender=sender,
            sent_at=sent_at,
            note=note,
        ),
    )
    return path


def list_messages(
    paths: MailboxPaths,
    *,
    slug: str | None = None,
    unread_only: bool = True,
) -> list[Message]:
    """Snapshot mailbox files. A damaged message remains a visible row."""
    if slug is None:
        try:
            mailboxes = sorted(path for path in paths.inbox_dir.iterdir() if path.is_dir())
        except FileNotFoundError:
            mailboxes = []
    else:
        mailboxes = [paths.mailbox_dir(slug)]

    messages: list[Message] = []
    states = (("new", True),) if unread_only else (("new", True), ("read", False))
    for mailbox in mailboxes:
        for state, unread in states:
            directory = mailbox / state
            try:
                message_paths = sorted(
                    path
                    for path in directory.glob("*.md")
                    if not path.name.startswith(".")
                )
            except OSError:
                message_paths = []
            for path in message_paths:
                try:
                    text = path.read_text(encoding="utf-8")
                except FileNotFoundError:
                    continue
                except (OSError, UnicodeError):
                    text = ""
                messages.append(Message.from_text(text, path=path, unread=unread))
    return sorted(messages, key=lambda message: (message.path.name, str(message.path)))


def mark_read(paths: MailboxPaths, *, slug: str, ids: list[str]) -> list[Message]:
    """Move only the supplied snapshot ids from new to read."""
    if not ids:
        return []
    marked: list[Message] = []
    destination_dir = paths.mailbox_read_dir(slug)
    destination_dir.mkdir(parents=True, exist_ok=True)
    for ident in ids:
        if not ident or Path(ident).name != ident:
            continue
        source = paths.message_file(slug, ident=ident)
        destination = destination_dir / f"{ident}.md"
        try:
            os.replace(source, destination)
        except FileNotFoundError:
            continue
        try:
            text = destination.read_text(encoding="utf-8")
        except FileNotFoundError:
            continue
        except (OSError, UnicodeError):
            text = ""
        marked.append(Message.from_text(text, path=destination, unread=False))
    return marked
