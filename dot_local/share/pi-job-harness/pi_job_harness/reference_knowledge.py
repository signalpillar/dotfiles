"""Soft OKF convention on bundle `references/` (not the task store).

Boundary: only this type scans concept notes, writes the index stub, and
formats status/validate warnings. Callers pass a `references/` directory
and print `warnings()`; they do not parse frontmatter beside this module.

OKF v0.2: path is identity, `type` is the only required concept field,
and `index.md` / `log.md` are reserved (no `type`).
"""

from __future__ import annotations

from collections.abc import Iterator
from pathlib import Path
from typing import ClassVar

import yaml

from pi_job_harness.profile import load_profile_contract
from pi_job_harness.store import BundleTaskLayout, atomic_write_text

RESERVED_REFERENCE_NAMES = frozenset({"index.md", "log.md"})
REFERENCE_INDEX_NAME = "index.md"
REFERENCE_WARN_TOP_N = 8


def concept_type(text: str) -> str | None:
    """Return the YAML `type` from a concept document, or None when absent."""
    if not text.startswith("---"):
        return None
    rest = text[3:]
    end = rest.find("\n---")
    if end == -1:
        return None
    try:
        data = yaml.safe_load(rest[:end])
    except yaml.YAMLError:
        return None
    if not isinstance(data, dict):
        return None
    value = data.get("type")
    if isinstance(value, str) and value.strip():
        return value.strip()
    return None


class ReferenceKnowledgeLint:
    """Soft OKF check and index stub for a bundle `references/` directory.

    Loose YAML tasks have no `references/` convention; callers skip them.
    """

    reserved_names: ClassVar[frozenset[str]] = RESERVED_REFERENCE_NAMES
    index_name: ClassVar[str] = REFERENCE_INDEX_NAME
    warn_top_n: ClassVar[int] = REFERENCE_WARN_TOP_N

    def __init__(self, references_dir: Path) -> None:
        self.references_dir = references_dir

    @classmethod
    def from_layout(cls, layout: object) -> ReferenceKnowledgeLint | None:
        """Return a lint for a bundle layout; None for loose YAML or other stores."""
        if isinstance(layout, BundleTaskLayout):
            return cls(layout.references_dir)
        return None

    @classmethod
    def from_task_arg(cls, task_arg: Path | None) -> ReferenceKnowledgeLint | None:
        """Return a lint when `--task` names a bundle directory or its `task.yaml`."""
        if task_arg is None:
            return None
        if task_arg.is_dir() and (task_arg / BundleTaskLayout.DOCUMENT_NAME).is_file():
            return cls(BundleTaskLayout(task_arg).references_dir)
        if task_arg.name == BundleTaskLayout.DOCUMENT_NAME:
            return cls(BundleTaskLayout(task_arg.parent).references_dir)
        return None

    def concept_paths(self) -> tuple[Path, ...]:
        """Markdown concept files under `references/`, reserved names excluded."""
        return tuple(self._iter_concept_paths())

    def _iter_concept_paths(self) -> Iterator[Path]:
        root = self.references_dir
        if not root.is_dir():
            return
        for path in sorted(root.rglob("*.md")):
            if path.name in self.reserved_names:
                continue
            if path.is_file():
                yield path

    def missing_type_relpaths(self) -> tuple[str, ...]:
        """Concept paths relative to `references/` that lack a `type` field."""
        missing: list[str] = []
        for path in self.concept_paths():
            try:
                text = path.read_text(encoding="utf-8")
            except OSError:
                missing.append(path.relative_to(self.references_dir).as_posix())
                continue
            if concept_type(text) is None:
                missing.append(path.relative_to(self.references_dir).as_posix())
        return tuple(missing)

    def warnings(self) -> list[str]:
        """Soft-limit copy for `status` / `validate`. Never fails the command."""
        issues: list[str] = []
        index = self.references_dir / self.index_name
        if not index.is_file():
            issues.append(
                "references/index.md missing; write the current-concept map "
                "so workers open it first"
            )
        missing = self.missing_type_relpaths()
        if missing:
            shown = missing[: self.warn_top_n]
            extra = f" (+{len(missing) - len(shown)} more)" if len(missing) > len(shown) else ""
            issues.append(
                "references/ concept files missing YAML `type`: "
                f"{', '.join(shown)}{extra}; "
                "add type/title/status (skip index.md/log.md)"
            )
        return issues

    def index_stub_text(self) -> str:
        """Pure: interpolate the profile index stub."""
        template = str(load_profile_contract()["instruction_packets"]["references_index_stub"])
        return template if template.endswith("\n") else template + "\n"

    def ensure_index(self) -> Path | None:
        """Write `references/index.md` once when missing. Never overwrite."""
        self.references_dir.mkdir(parents=True, exist_ok=True)
        path = self.references_dir / self.index_name
        if path.exists():
            return None
        atomic_write_text(path, self.index_stub_text())
        return path
