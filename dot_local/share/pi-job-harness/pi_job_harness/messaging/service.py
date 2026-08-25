"""Mailbox send, list, and read facade."""

from __future__ import annotations

from pathlib import Path
from typing import Protocol

from pi_job_harness.messaging.address import Address, address_slug
from pi_job_harness.messaging.io import list_messages, mark_read, send_message
from pi_job_harness.messaging.message import Message
from pi_job_harness.messaging.paths import MailboxPaths


class PlansLayout(Protocol):
    """Layout fields required by messaging."""

    @property
    def plans_dir(self) -> Path: ...

    @property
    def plans_pointer(self) -> str: ...


class MessageService:
    """Own mailbox operations for one task plans root."""

    def __init__(self, paths: MailboxPaths) -> None:
        self.paths = paths

    @classmethod
    def from_layout(cls, layout: PlansLayout) -> MessageService:
        return cls(
            MailboxPaths(
                layout.plans_dir,
                pointer_root=layout.plans_pointer,
            )
        )

    def send(self, *, to: Address, note: str, sender: str) -> Path:
        return send_message(
            self.paths,
            slug=address_slug(to),
            to=to.label,
            note=note,
            sender=sender,
        )

    def list(
        self,
        *,
        slug: str | None = None,
        unread_only: bool = True,
    ) -> list[Message]:
        return list_messages(self.paths, slug=slug, unread_only=unread_only)

    def mark_read(self, *, slug: str, ids: list[str]) -> list[Message]:
        return mark_read(self.paths, slug=slug, ids=ids)
