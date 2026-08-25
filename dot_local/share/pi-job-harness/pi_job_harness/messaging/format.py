"""CLI formatting for mailbox messages."""

from __future__ import annotations

from collections.abc import Sequence

from pi_job_harness.messaging.message import Message


def render_message_status_line(message: Message) -> str:
    """Render one compact status row, including the path for damaged files."""
    first_line = next(
        (" ".join(line.split()) for line in message.body.splitlines() if line.strip()),
        "",
    )
    if len(first_line) > 80:
        first_line = first_line[:77] + "..."
    target = message.to
    sender = message.from_
    marker = str(message.path) if not (target and sender) else message.id
    return f"  {target} <- {sender}  {first_line}  [{marker}]"


def format_read_messages(messages: Sequence[Message]) -> str:
    """Format complete message records for a text consumer."""
    lines: list[str] = []
    for message in messages:
        lines.extend(
            (
                f"--- {message.id} ---",
                f"to: {message.to}",
                f"from: {message.from_}",
                f"sent_at: {message.sent_at}",
                f"path: {message.path}",
            )
        )
        if message.body:
            lines.extend(("", message.body.rstrip()))
    return "\n".join(lines)
