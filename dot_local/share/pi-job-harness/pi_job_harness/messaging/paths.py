"""Pure mailbox path arithmetic."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class MailboxPaths:
    """Paths below one task layout's plans root."""

    INBOX_NAME = "_inbox"

    # Filesystem root that owns task plan sidecars.
    plans_dir: Path
    # Task-relative plans pointer used in CLI output.
    pointer_root: str

    @property
    def inbox_dir(self) -> Path:
        return self.plans_dir / self.INBOX_NAME

    def mailbox_dir(self, slug: str) -> Path:
        return self.inbox_dir / slug

    def mailbox_new_dir(self, slug: str) -> Path:
        return self.mailbox_dir(slug) / "new"

    def mailbox_read_dir(self, slug: str) -> Path:
        return self.mailbox_dir(slug) / "read"

    def message_file(self, slug: str, *, ident: str) -> Path:
        return self.mailbox_new_dir(slug) / f"{ident}.md"

    def inbox_pointer(self, slug: str) -> str:
        return f"{self.pointer_root}/{self.INBOX_NAME}/{slug}"
