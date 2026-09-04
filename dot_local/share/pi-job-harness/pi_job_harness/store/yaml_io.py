"""YAML load, validate, digest, and atomic write for task documents."""

from __future__ import annotations

import hashlib
import json
import os
import stat
import sys
from collections.abc import Mapping
from pathlib import Path
from typing import Any
from uuid import uuid4

import yaml
from pydantic import ValidationError

from pi_job_harness.errors import die
from pi_job_harness.layout import PiJobLayout
from pi_job_harness.task import TaskDocument


def _instruction_packets() -> dict[str, Any]:
    from pi_job_harness.profile import load_profile_contract

    return load_profile_contract()["instruction_packets"]


class UniqueKeyLoader(yaml.SafeLoader):
    """Safe YAML loader that rejects duplicate mapping keys instead of losing data."""


def _construct_unique_mapping(
    loader: UniqueKeyLoader,
    node: yaml.nodes.MappingNode,
    deep: bool = False,
) -> dict[Any, Any]:
    mapping: dict[Any, Any] = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in mapping:
            raise yaml.constructor.ConstructorError(
                "while constructing a mapping",
                node.start_mark,
                f"found duplicate key {key!r}",
                key_node.start_mark,
            )
        mapping[key] = loader.construct_object(value_node, deep=deep)
    return mapping


UniqueKeyLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
    _construct_unique_mapping,
)

YAML_HEADER = (
    "# Managed by pi-job. Prefer pi-job commands over manual edits.\n"
    "# Validate emergency edits with: pi-job --task <this-file> validate\n"
)


def load_yaml_mapping(path: Path, *, label: str) -> dict[str, Any]:
    """Load one YAML mapping with duplicate-key and syntax diagnostics."""

    if not path.exists():
        if label == "task file":
            die(missing_task_hint(path))
        die(f"{label} not found: {path}")
    try:
        value = yaml.load(path.read_text(encoding="utf-8"), Loader=UniqueKeyLoader)
    except (OSError, yaml.YAMLError) as exc:
        die(f"invalid {label} {path}:\n{exc}")
    if not isinstance(value, dict):
        die(f"invalid {label} {path}: expected a YAML mapping at the document root")
    return value


def validate_task_mapping(
    value: Mapping[str, Any],
    *,
    source: str,
) -> TaskDocument:
    """Validate backend data against the single documented task contract."""

    try:
        return TaskDocument.model_validate(dict(value), extra="forbid")
    except ValidationError as exc:
        die(f"task validation failed for {source}:\n{exc}")


def canonical_task_mapping(value: Mapping[str, Any], *, source: str) -> dict[str, Any]:
    """Return deterministic JSON-compatible task data after strict validation."""

    data = validate_task_mapping(value, source=source).model_dump(mode="json", exclude_none=True)
    # Omit empty layers so upgrades do not dirty content digests of tasks without layers.
    if not data.get("layers"):
        data.pop("layers", None)
    orchestration = data.get("orchestration")
    if isinstance(orchestration, dict) and not orchestration.get("maintain"):
        orchestration.pop("maintain", None)
    return data


def _task_mapping_for_content_digest(task: Mapping[str, Any]) -> dict[str, Any]:
    """Return a copy of task data with orchestration.content_digest stripped for hashing."""

    data = json.loads(json.dumps(task))
    orchestration = data.get("orchestration")
    if isinstance(orchestration, dict):
        orchestration.pop("content_digest", None)
    return data


def compute_content_digest(task: Mapping[str, Any]) -> str:
    """SHA-256 hex of canonical semantic task content (digest field excluded)."""

    canonical = canonical_task_mapping(
        _task_mapping_for_content_digest(task),
        source="content-digest",
    )
    payload = json.dumps(canonical, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def is_content_dirty(task: Mapping[str, Any]) -> bool:
    """True when a stored digest exists and no longer matches semantic content."""

    orchestration = task.get("orchestration")
    if not isinstance(orchestration, dict):
        return False
    stored = orchestration.get("content_digest")
    if not stored:
        return False
    return stored != compute_content_digest(task)


def warn_if_content_dirty(task: Mapping[str, Any], path: Path | str) -> None:
    """Print one stderr warning when the task file was edited outside pi-job.

    Warning body comes from profile instruction_packets.out_of_band_edit_warning.
    """

    if not is_content_dirty(task):
        return
    packets = _instruction_packets()
    text = packets["out_of_band_edit_warning"].format(task_file=str(path)).rstrip()
    print(text, file=sys.stderr)


def set_content_digest(task: dict[str, Any]) -> None:
    """Refresh orchestration.content_digest from the current semantic task content."""

    orchestration = task.get("orchestration")
    if not isinstance(orchestration, dict) or "cursors" not in orchestration:
        return
    orchestration["content_digest"] = compute_content_digest(task)


def semantic_task_mapping(value: Mapping[str, Any], *, source: str) -> dict[str, Any]:
    """Canonical task mapping with orchestration.content_digest excluded for equality checks."""

    data = canonical_task_mapping(value, source=source)
    copy = json.loads(json.dumps(data))
    orchestration = copy.get("orchestration")
    if isinstance(orchestration, dict):
        orchestration.pop("content_digest", None)
    return copy


def render_yaml_task(value: Mapping[str, Any], *, source: str) -> str:
    """Render canonical machine-owned YAML with stable field order."""

    data = canonical_task_mapping(value, source=source)
    return YAML_HEADER + yaml.safe_dump(
        data,
        allow_unicode=True,
        default_flow_style=False,
        sort_keys=False,
        width=100,
    )


def atomic_write_text(path: Path, content: str) -> None:
    """Atomically replace one file using a sibling temporary path."""

    path.parent.mkdir(parents=True, exist_ok=True)
    existing_mode = stat.S_IMODE(path.stat().st_mode) if path.exists() else None
    temporary = path.with_name(f".{path.name}.tmp-{os.getpid()}-{uuid4().hex}")
    try:
        temporary.write_text(content, encoding="utf-8")
        if existing_mode is not None:
            temporary.chmod(existing_mode)
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def yaml_task_lock_path(task_path: Path, layout: PiJobLayout) -> Path:
    """Return the advisory lock path for a YAML task file.

    Locks live under ``$XDG_CACHE_HOME/pi-job/locks`` (default ``~/.cache``),
    keyed by a hash of the resolved task path, so task directories stay free of
    sibling ``.*.yaml.lock`` sentinels. Resolve before hashing so two names for
    the same file share one lock.
    """

    return layout.yaml_task_lock_path(task_path)


def atomic_create_text(path: Path, content: str) -> None:
    """Publish a complete new file atomically without replacing an existing path."""

    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp-{os.getpid()}-{uuid4().hex}")
    try:
        temporary.write_text(content, encoding="utf-8")
        try:
            os.link(temporary, path)
        except FileExistsError:
            die(f"destination YAML task already exists: {path}")
    finally:
        temporary.unlink(missing_ok=True)


def missing_task_hint(task_file: Path) -> str:
    packets = _instruction_packets()
    return packets["missing_task_hint"].format(task_file=str(task_file))

