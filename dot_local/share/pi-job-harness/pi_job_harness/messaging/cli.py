"""Argparse and process-edge command for the task mailbox."""

from __future__ import annotations

import argparse
import inspect
import os
from collections.abc import Mapping
from pathlib import Path
from typing import Any, NoReturn, Protocol

from pi_job_harness.messaging.address import address_slug, parse_address
from pi_job_harness.messaging.format import format_read_messages
from pi_job_harness.messaging.service import MessageService


class MsgHost(Protocol):
    """Process helpers the msg command needs from the CLI host module."""

    def die(self, message: str) -> NoReturn: ...

    def require_task(self, task_arg: Path | None, *, cmd: str) -> Path: ...

    def open_task_store(self, path: Path, layout: Any) -> Any: ...

    def require_initialized(self, task_file: Path, task: dict[str, Any]) -> None: ...

    def resolve_claim_for_command(
        self,
        task: Mapping[str, Any],
        args: argparse.Namespace,
        *,
        cmd: str,
        required: bool,
    ) -> Any: ...

    def task_slices(self, task: dict[str, Any]) -> Any: ...


def _default_host() -> MsgHost:
    from pi_job_harness import app

    return app


def _require_plans_layout(store: Any, *, die: Any) -> Any:
    layout = getattr(store, "layout", None)
    if layout is None or not hasattr(layout, "plans_dir"):
        die("msg requires a YAML task file")
    return layout


def _open_task_store(host: MsgHost, task_file: Path, layout: Any) -> Any:
    """Call the host's store opener with the injected layout when it accepts one."""

    params = tuple(inspect.signature(host.open_task_store).parameters)
    if len(params) >= 2:
        return host.open_task_store(task_file, layout)
    return host.open_task_store(task_file)


def add_msg_parser(
    subparsers: argparse._SubParsersAction[Any],
    cli_help: Mapping[str, Any],
) -> None:
    """Register the `msg` subcommand on the host parser."""
    msg_help = str(cli_help["msg"]["command"])
    msg_note_help = str(cli_help["msg"]["note"])
    parser = subparsers.add_parser(
        "msg",
        help=msg_help,
        description=msg_help,
        epilog=msg_note_help,
    )
    parser.add_argument(
        "--to",
        required=True,
        metavar="ADDRESS",
        help="exactly manager or slice:KEY",
    )
    msg_mode = parser.add_mutually_exclusive_group()
    msg_mode.add_argument("--note", help="message body for send")
    msg_mode.add_argument(
        "--read",
        action="store_true",
        help="print, then acknowledge this mailbox",
    )
    parser.add_argument(
        "--owner",
        help="sender provenance; defaults to $PI_JOB_OWNER, a sole claim, or manager",
    )
    parser.set_defaults(fn=cmd_msg)


def cmd_msg(args: argparse.Namespace, *, host: MsgHost | None = None) -> None:
    """Send or acknowledge a mailbox message. Host supplies task/claim process policy."""
    host = host or _default_host()
    task_file = host.require_task(args.task, cmd="msg")
    store = _open_task_store(host, task_file, args.layout)
    layout = _require_plans_layout(store, die=host.die)
    task = store.read()
    bus = MessageService.from_layout(layout)

    if args.read:
        if args.note is not None:
            host.die("--note and --read are mutually exclusive")
        try:
            address = parse_address(args.to)
        except ValueError as exc:
            host.die(str(exc))
        slug = address_slug(address)
        snapshot = bus.list(slug=slug, unread_only=True)
        formatted = format_read_messages(snapshot)
        if formatted:
            print(formatted)
        print(f"Read: {len(snapshot)} message(s) from {args.to}", flush=True)
        bus.mark_read(slug=slug, ids=[message.id for message in snapshot])
        return

    host.require_initialized(task_file, task)
    try:
        address = parse_address(args.to)
    except ValueError as exc:
        host.die(str(exc))
    if address.kind == "slice" and address.key not in {
        task_slice.key for task_slice in host.task_slices(task)
    }:
        host.die(f"unknown slice address {args.to!r}; no message was sent")
    note = (args.note or "").strip()
    if not note:
        host.die("--note is required and must not be empty")
    claim = host.resolve_claim_for_command(task, args, cmd="msg", required=False)
    owner_id = args.owner or os.environ.get("PI_JOB_OWNER")
    if claim is not None:
        if address.kind == "slice" and address.key == claim.slice:
            host.die("cannot send a message to the caller's claimed slice")
        sender = f"slice:{claim.slice} ({claim.owner})"
    else:
        sender = owner_id or "manager"
    path = bus.send(to=address, note=note, sender=sender)
    print(f"sent message: {bus.paths.inbox_pointer(address_slug(address))}")
    print(f"path: {path}")
