"""Mailbox address parsing and serialization."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Literal

ADDRESS_SLICE_KEY_RE = re.compile(r"^[A-Za-z0-9._-]+$")


@dataclass(frozen=True)
class Address:
    """A routing address for the task mailbox: the manager role, or one slice."""

    # Closed address variant. The manager is a task-scoped role.
    kind: Literal["manager", "slice"]
    # Slice key for the slice variant. Empty only for the manager variant.
    key: str = ""

    @property
    def label(self) -> str:
        return "manager" if self.kind == "manager" else f"slice:{self.key}"


def parse_address(raw: str) -> Address:
    """Parse mailbox address syntax without task or process policy."""
    legal = f"invalid address {raw!r}: must be exactly `manager` or `slice:KEY`"
    if raw == "manager":
        return Address(kind="manager")
    if not raw.startswith("slice:"):
        raise ValueError(legal)
    key = raw.removeprefix("slice:")
    if key in {".", ".."} or not ADDRESS_SLICE_KEY_RE.fullmatch(key):
        raise ValueError(legal)
    return Address(kind="slice", key=key)


def address_slug(address: Address) -> str:
    return "manager" if address.kind == "manager" else f"slice-{address.key}"
