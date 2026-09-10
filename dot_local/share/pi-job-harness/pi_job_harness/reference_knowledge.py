"""Soft OKF convention on bundle `references/` (not the task store).

Boundary: only this type scans concept notes, writes the index stub, and
formats status/validate warnings. Callers pass a `references/` directory
and print `warnings()`; they do not parse frontmatter beside this module.

OKF v0.2: path is identity, `type` is the only required wiki field,
and `index.md` / `log.md` are reserved (no `type`).
Durable pages live in `wiki/`. Step notes live in `working/`.
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
WIKI_DIR_NAME = "wiki"
WORKING_DIR_NAME = "working"
ALLOWED_WIKI_TYPES = frozenset({"position", "gateway", "concept", "evidence"})
STEP_NOTE_SUFFIXES = (
    "-explore-context.md",
    "-investigate.md",
    "-clarify-scope.md",
)


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


def is_step_note_name(name: str) -> bool:
    """Return True when the filename is an explore/investigate/clarify dump."""
    return name.endswith(STEP_NOTE_SUFFIXES)


class ReferenceKnowledgeLint:
    """Soft OKF check and index stub for a bundle `references/` directory.

    Loose YAML tasks have no `references/` convention; callers skip them.
    """

    reserved_names: ClassVar[frozenset[str]] = RESERVED_REFERENCE_NAMES
    index_name: ClassVar[str] = REFERENCE_INDEX_NAME
    warn_top_n: ClassVar[int] = REFERENCE_WARN_TOP_N
    wiki_dir_name: ClassVar[str] = WIKI_DIR_NAME
    working_dir_name: ClassVar[str] = WORKING_DIR_NAME
    allowed_wiki_types: ClassVar[frozenset[str]] = ALLOWED_WIKI_TYPES

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

    def wiki_dir(self) -> Path:
        return self.references_dir / self.wiki_dir_name

    def working_dir(self) -> Path:
        return self.references_dir / self.working_dir_name

    def concept_paths(self) -> tuple[Path, ...]:
        """Markdown wiki pages under `references/wiki/`."""
        return tuple(self._iter_wiki_paths())

    def _iter_markdown(self, root: Path) -> Iterator[Path]:
        if not root.is_dir():
            return
        for path in sorted(root.rglob("*.md")):
            if path.name in self.reserved_names:
                continue
            if path.is_file():
                yield path

    def _iter_wiki_paths(self) -> Iterator[Path]:
        yield from self._iter_markdown(self.wiki_dir())

    def missing_type_relpaths(self) -> tuple[str, ...]:
        """Wiki paths relative to `references/` that lack a `type` field."""
        missing: list[str] = []
        for path in self._iter_wiki_paths():
            rel = path.relative_to(self.references_dir).as_posix()
            try:
                text = path.read_text(encoding="utf-8")
            except OSError:
                missing.append(rel)
                continue
            if concept_type(text) is None:
                missing.append(rel)
        return tuple(missing)

    def invalid_type_relpaths(self) -> tuple[str, ...]:
        """Wiki paths whose `type` is set and not in the closed OKF set."""
        invalid: list[str] = []
        for path in self._iter_wiki_paths():
            rel = path.relative_to(self.references_dir).as_posix()
            try:
                text = path.read_text(encoding="utf-8")
            except OSError:
                continue
            found = concept_type(text)
            if found is None:
                continue
            if found.lower() not in self.allowed_wiki_types:
                invalid.append(rel)
        return tuple(invalid)

    def stray_markdown_relpaths(self) -> tuple[str, ...]:
        """Markdown files that sit outside `wiki/` and `working/`."""
        stray: list[str] = []
        root = self.references_dir
        if not root.is_dir():
            return ()
        wiki = self.wiki_dir().resolve()
        working = self.working_dir().resolve()
        for path in sorted(root.rglob("*.md")):
            if not path.is_file() or path.name in self.reserved_names:
                continue
            resolved = path.resolve()
            if wiki in resolved.parents or working in resolved.parents:
                continue
            stray.append(path.relative_to(root).as_posix())
        return tuple(stray)

    def step_named_wiki_relpaths(self) -> tuple[str, ...]:
        """Wiki pages named like explore/investigate/clarify dumps."""
        return tuple(
            path.relative_to(self.references_dir).as_posix()
            for path in self._iter_wiki_paths()
            if is_step_note_name(path.name)
        )

    def unlisted_wiki_relpaths(self) -> tuple[str, ...]:
        """Wiki pages whose relative path is absent from `index.md`."""
        index = self.references_dir / self.index_name
        try:
            body = index.read_text(encoding="utf-8")
        except OSError:
            body = ""
        unlisted: list[str] = []
        for path in self._iter_wiki_paths():
            rel = path.relative_to(self.references_dir).as_posix()
            if rel not in body:
                unlisted.append(rel)
        return tuple(unlisted)

    def _format_list(self, items: tuple[str, ...]) -> str:
        shown = items[: self.warn_top_n]
        extra = f" (+{len(items) - len(shown)} more)" if len(items) > len(shown) else ""
        return f"{', '.join(shown)}{extra}"

    def warnings(self) -> list[str]:
        """Soft-limit copy for `status` / `validate`. Never fails the command."""
        issues: list[str] = []
        index = self.references_dir / self.index_name
        if not index.is_file():
            issues.append(
                "references/index.md missing; write the current-concept map "
                "so workers open it first"
            )
        stray = self.stray_markdown_relpaths()
        if stray:
            issues.append(
                "references/ markdown outside wiki/ or working/: "
                f"{self._format_list(stray)}; "
                "move durable pages to wiki/ and step notes to working/"
            )
        missing = self.missing_type_relpaths()
        if missing:
            issues.append(
                "references/ wiki files missing YAML `type`: "
                f"{self._format_list(missing)}; "
                "add type/title/status (skip index.md/log.md/working/)"
            )
        invalid = self.invalid_type_relpaths()
        if invalid:
            issues.append(
                "references/ wiki files have invalid YAML `type`: "
                f"{self._format_list(invalid)}; "
                "use position, gateway, concept, or evidence"
            )
        step_named = self.step_named_wiki_relpaths()
        if step_named:
            issues.append(
                "references/ wiki files use step-note names: "
                f"{self._format_list(step_named)}; "
                "move explore/investigate/clarify dumps to working/"
            )
        if index.is_file():
            unlisted = self.unlisted_wiki_relpaths()
            if unlisted:
                issues.append(
                    "references/ wiki files missing from index.md: "
                    f"{self._format_list(unlisted)}; "
                    "add a Current or deprecated row in synthesize"
                )
        return issues

    def index_stub_text(self) -> str:
        """Pure: interpolate the profile index stub."""
        template = str(load_profile_contract()["instruction_packets"]["references_index_stub"])
        return template if template.endswith("\n") else template + "\n"

    def ensure_layout(self) -> None:
        """Create `references/`, `wiki/`, and `working/` when missing."""
        self.references_dir.mkdir(parents=True, exist_ok=True)
        self.wiki_dir().mkdir(exist_ok=True)
        self.working_dir().mkdir(exist_ok=True)

    def ensure_index(self) -> Path | None:
        """Write `references/index.md` once when missing. Never overwrite."""
        self.ensure_layout()
        path = self.references_dir / self.index_name
        if path.exists():
            return None
        atomic_write_text(path, self.index_stub_text())
        return path
