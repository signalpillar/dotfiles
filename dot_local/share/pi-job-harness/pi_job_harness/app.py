"""Deterministic task harness prototype for pi-agent jobs.

The selected TaskStore is the source of truth. YAML task files and directory-backed
tasks are supported. The package-local YAML profile defines step and slice-kind behavior.
"""

from __future__ import annotations

import argparse
import difflib
import json
import os
import re
import shutil
import sys
from collections.abc import Iterator, Mapping, Sequence
from contextlib import nullcontext
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, ClassVar

import yaml
from pydantic import ValidationError

from pi_job_harness.errors import die
from pi_job_harness.layout import PiJobLayout
from pi_job_harness.messaging import (
    Address,  # noqa: F401 - tests getattr this on the app module
    Message,
    MessageService,
    address_slug,  # noqa: F401 - tests getattr this on the app module
    format_read_messages,  # noqa: F401 - tests getattr this on the app module
    parse_address,  # noqa: F401 - tests getattr this on the app module
    render_message_status_line,
)
from pi_job_harness.messaging.cli import add_msg_parser, cmd_msg  # noqa: F401
from pi_job_harness.profile import (
    PROFILE,
    ProfileDocument,  # noqa: F401 - tests getattr this on the app module
    load_profile_contract,
)
from pi_job_harness.project_validation import (
    validate_project_route_and_key,
    validate_real_goal,
)
from pi_job_harness.store import (
    BundleTaskLayout,
    FsTaskStore,  # noqa: F401 - tests getattr this on the app module
    TaskStore,
    YamlTaskLayout,
    YamlTaskStore,
    atomic_write_text,
    canonical_task_mapping,
    compute_content_digest,  # noqa: F401 - tests getattr this on the app module
    layout_for_document_path,
    load_yaml_mapping,
    open_task_store,
    project,
    render_yaml_task,
    semantic_task_mapping,
    unsupported_storage,
    validate_task_mapping,
    yaml_task_lock_path,  # noqa: F401 - tests getattr this on the app module
)
from pi_job_harness.store.text import merge_note, utc_now
from pi_job_harness.task import (
    ARTIFACT_STATUSES,
    LAYERED_SLICE_KINDS,
    STATUS_DONE,
    BootstrapDocument,
    Cursor,
    ExecutionRecord,
    OwnedCursor,
    TaskDocument,
    TaskSlice,
    TaskStatus,
    TaskStep,
    task_slices,
)

ROOT = Path.cwd()
NOTE_WARN_CHARS = 2000
TASK_FILE_WARN_BYTES = 100_000
NOTE_WARN_TOP_N = 5
BIGPICTURE_STUB_MARKER = "# pi-job-bigpicture-stub"
BIGPICTURE_DEFAULT_RELPATH = "references/bigpicture.txt"
GLYPH = {"done": "✓", "skipped": "⊘", "in_progress": "▸", "blocked": "✗", "planned": "○"}
# ANSI styles for human-facing show glyphs only (default off unless TTY / --color).
_RESET = "\033[0m"
_GLYPH_STYLE = {
    "done": "\033[32m",        # green tick
    "skipped": "\033[33m",     # yellow
    "in_progress": "\033[36m", # cyan
    "blocked": "\033[31m",     # red cross
    "planned": "\033[2m",      # dim
}
_CURRENT_STYLE = "\033[1;35m"  # bold magenta: readable on light and dark terminals


def color_enabled(mode: str) -> bool:
    """Resolve --color auto|always|never. Respects NO_COLOR / FORCE_COLOR."""
    if mode == "always":
        return True
    if mode == "never":
        return False
    if os.environ.get("NO_COLOR"):
        return False
    if os.environ.get("FORCE_COLOR"):
        return True
    return sys.stdout.isatty()


def paint_glyph(glyph: str, status: str, *, color: bool) -> str:
    if not color:
        return glyph
    style = _GLYPH_STYLE.get(status, "")
    if not style:
        return glyph
    return f"{style}{glyph}{_RESET}"





CREATE_MODES_HINT = (
    "Supported create modes:\n"
    "  pi-job --task TASK create --from intent.yaml\n"
    "  pi-job --task TASK create --empty-plan\n"
    "  pi-job --task TASK create --kind K --goal \"…\""
)


def _require_create_goal(args: argparse.Namespace) -> str:
    raw = getattr(args, "goal", None)
    if raw is None or not str(raw).strip():
        die("--goal is required when seeding slices\n" + CREATE_MODES_HINT)
    return validate_real_goal(str(raw), label="--goal")


def _validate_task_project_route(task: Mapping[str, Any], *, repo_root: Path | None = None) -> None:
    project = task.get("project") or {}
    validate_project_route_and_key(
        str(project.get("route") or ""),
        str(project.get("key") or ""),
        repo_root=repo_root or Path.cwd(),
    )


def example_task_mapping(*, title: str = "Example bounded change") -> dict[str, Any]:
    """Return the backend-neutral example used by YAML scaffolding."""

    return {
        "title": title,
        "status": "in_progress",
        "source": {
            "jira": "",
            "discovered": "",
            "context": "Short discovery note for why this task exists.",
        },
        "project": {
            "key": "example",
            "name": "Example Project",
            "route": "projects/example/workflow.md",
            "context": "Where this work lives in the repository.",
        },
        "context": (
            "Free-form background the agent should read before acting.\n"
            "Prefer pi-job commands when replacing this example with real work."
        ),
        "decisions": [],
        "plan": {
            "note": "Replace this scaffold plan with real slices using pi-job commands.",
            "slices": [{
                "key": "do-the-change",
                "kind": "implement",
                "title": "Do the change",
                "goal": "Ship the bounded edit with verification.",
                "status": "in_progress",
                "note": "",
                "steps": [
                    {"key": key, "title": step_title, "status": "planned", "note": ""}
                    for key, step_title in steps_from_kind_template("implement")
                ],
                "final_steps": [],
            }],
        },
    }








TASK_SLUG_RE = re.compile(r"^[a-z0-9]+(?:-[a-z0-9]+)*$")


def task_tasks_home(layout: PiJobLayout) -> Path:
    """Central pi-job task home; `PI_JOB_TASKS` overrides the default location."""
    return layout.tasks_home


def task_archive_home(layout: PiJobLayout) -> Path:
    """Archived task home; `PI_JOB_ARCHIVE` overrides, else sibling `archive/` of the tasks home."""
    return layout.archive_home


def bundle_slug_under_home(
    task_layout: BundleTaskLayout,
    host_layout: PiJobLayout,
) -> str | None:
    """Bundle slug when `layout.bundle_root` is an immediate child of `task_tasks_home()`.

    Mirrors the home-membership rule `list` uses to enumerate bundles: a bundle opened by
    path from anywhere else (or nested deeper under the home) is not slug-addressable and
    has no display slug, even though its directory name looks like one."""
    if task_layout.bundle_root.parent == task_tasks_home(host_layout):
        return task_layout.bundle_root.name
    return None


def task_display_ref(store: TaskStore, host_layout: PiJobLayout) -> str:
    """Display identity for `--task`, used as the `Task:` header in status output and
    instruction/plan/sync/wayfinder packets.

    Prefers the bundle slug when the store is a `BundleTaskLayout` living directly under
    the task home; otherwise falls back to the resolved document path (loose YAML, or a
    bundle opened from outside the home). Non-YAML-backed stores (e.g. the experimental
    directory store) have no layout document path, so they fall back to `store.describe()`.
    """
    layout = getattr(store, "layout", None)
    if isinstance(layout, BundleTaskLayout):
        slug = bundle_slug_under_home(layout, host_layout)
        if slug is not None:
            return slug
    if isinstance(layout, (YamlTaskLayout, BundleTaskLayout)):
        return str(layout.document_path)
    return store.describe()


def worktrees_home(layout: PiJobLayout) -> Path:
    """Central pi-job worktree home; `PI_JOB_WORKTREES` overrides the default location.

    Advisory only: `pi-job` never creates directories or git worktrees here, it only
    recommends and records absolute paths under this convention."""
    return layout.worktrees_home


def iter_home_bundle_docs(home: Path) -> list[tuple[str, Path]]:
    """`(slug, task.yaml path)` for every immediate child bundle dir of `home`, sorted by slug.

    Only directories containing a `task.yaml` file count; loose `*.yaml`/`*.yml` files
    directly under `home`, non-directory entries, and dirs without `task.yaml` are ignored
    (no recursive scan, no slug map)."""
    if not home.is_dir():
        return []
    found = []
    for child in home.iterdir():
        if not child.is_dir():
            continue
        doc = child / BundleTaskLayout.DOCUMENT_NAME
        if doc.is_file():
            found.append((child.name, doc))
    return sorted(found, key=lambda pair: pair[0])


def archive_home_bundle(
    *,
    bundle_root: Path,
    archive_home: Path,
    dest_slug: str,
    dry_run: bool = False,
) -> Path:
    """Move a task-home bundle directory into `archive_home` under `dest_slug`.

    Home-level store operation: `list` / slug resolve only see `$PI_JOB_TASKS`, so moving
    the bundle out of the tasks home is how archive works. Returns the destination path.
    Refuses when the destination already exists (no overwrite).
    """
    if not is_task_slug(dest_slug):
        die(
            f"invalid archive slug {dest_slug!r}; slugs must match {TASK_SLUG_RE.pattern}"
        )
    if not bundle_root.is_dir():
        die(f"archive source is not a bundle directory: {bundle_root}")
    doc = bundle_root / BundleTaskLayout.DOCUMENT_NAME
    if not doc.is_file():
        die(f"archive source is missing {BundleTaskLayout.DOCUMENT_NAME}: {bundle_root}")
    dest = archive_home / dest_slug
    if dest.exists():
        die(
            f"archive destination already exists: {dest}; "
            "pass --to <new-slug> to choose a free name"
        )
    if dry_run:
        return dest
    archive_home.mkdir(parents=True, exist_ok=True)
    shutil.move(str(bundle_root), str(dest))
    return dest


LIST_STATUS_ORDER = {
    "in_progress": 0,
    "blocked": 1,
    "planned": 2,
    "skipped": 3,
    "done": 4,
}


@dataclass(frozen=True)
class TaskListEntry:
    slug: str
    title: str
    status: TaskStatus
    updated: datetime
    cursor_labels: tuple[str, ...]


def task_list_updated(task: Mapping[str, Any], document_path: Path) -> datetime:
    """Latest valid cursor heartbeat, falling back to the task document mtime."""
    cursor_updates = [
        parsed
        for claim in owned_cursors(task)
        if (parsed := parse_utc_timestamp(claim.last_seen)) is not None
    ]
    if cursor_updates:
        return max(cursor_updates)
    return datetime.fromtimestamp(document_path.stat().st_mtime, UTC)


def task_list_sort_key(entry: TaskListEntry) -> tuple[int, float, str]:
    """Status group first, newest activity first, then stable slug order."""
    return (
        LIST_STATUS_ORDER[entry.status],
        -entry.updated.timestamp(),
        entry.slug,
    )


def format_task_list_entry(entry: TaskListEntry) -> str:
    """Render one readable `pi-job list` block."""
    lines = [
        f"{entry.slug} [{entry.status}]",
        f"  Title: {entry.title}",
        f"  Updated: {entry.updated.astimezone(UTC):%Y-%m-%d %H:%M UTC}",
    ]
    if entry.cursor_labels:
        lines.extend(f"  Cursor: {label}" for label in entry.cursor_labels)
    else:
        lines.append("  Cursor: -")
    return "\n".join(lines)


def is_task_slug(text: str) -> bool:
    return TASK_SLUG_RE.fullmatch(text) is not None


def resolve_task_arg(raw: str | Path, layout: PiJobLayout) -> Path:
    """Resolve a `--task` CLI value to a concrete path, before `open_task_store` runs.

    A bare slug (charset `^[a-z0-9]+(?:-[a-z0-9]+)*$`) resolves only inside
    `task_tasks_home()`, to that slug's bundle `task.yaml`; nothing else about a
    slug is special-cased (no directory scan, no loose-YAML slug map). Anything
    that is not a valid slug and contains no path separator is rejected outright
    rather than silently falling back to a `cwd`-relative path (use `./name` or an
    absolute/`~` path for a real file next to the slug charset). Everything else
    is a path and defers unchanged to `open_task_store`'s existing bundle/YAML/
    directory detection.
    """
    text = str(raw)
    if is_task_slug(text):
        doc = task_tasks_home(layout) / text / BundleTaskLayout.DOCUMENT_NAME
        if not doc.is_file():
            die(f"unknown task slug {text!r}; expected {doc}")
        return doc.resolve()
    if "/" not in text and "\\" not in text:
        die(
            f"invalid task slug {text!r}; slugs must match {TASK_SLUG_RE.pattern} "
            f"(use ./{text} or an absolute/~ path to open a file)"
        )
    return Path(raw).expanduser().resolve()




def resolve_create_task_arg(raw: str | Path, layout: PiJobLayout) -> Path:
    """Resolve a `--task` CLI value for `create`, before any bundle may exist.

    Slug resolution mirrors `resolve_task_arg` (same charset, same invalid-token
    die), except an unknown slug is the *scaffold target*, not an error: it
    always resolves to `task_tasks_home()/<slug>/task.yaml`, whether or not
    that bundle exists yet. A path argument resolves to a bundle root via
    `derive_bundle_root` and then to that root's `task.yaml`; `create` never
    targets a loose YAML file.
    """
    text = str(raw)
    if is_task_slug(text):
        return (task_tasks_home(layout) / text / BundleTaskLayout.DOCUMENT_NAME).resolve()
    if "/" not in text and "\\" not in text:
        die(
            f"invalid task slug {text!r}; slugs must match {TASK_SLUG_RE.pattern} "
            f"(use ./{text} or an absolute/~ path to open a file)"
        )
    resolved = Path(raw).expanduser().resolve()
    bundle_root = derive_bundle_root(resolved)
    return bundle_root / BundleTaskLayout.DOCUMENT_NAME


def derive_bundle_root(resolved: Path) -> Path:
    """Given an expanded `create` `--task` path, return the bundle root to scaffold.

    `…/task.yaml` resolves to its parent directory; an existing directory
    resolves to itself; a loose `*.yaml`/`*.yml` file (any other name) is
    rejected outright, since `create` never writes loose YAML task files or
    their sibling `<stem>.plans/` directories.
    """
    if resolved.name == BundleTaskLayout.DOCUMENT_NAME:
        return resolved.parent
    if resolved.is_dir():
        return resolved
    if resolved.suffix.lower() in {".yaml", ".yml"}:
        die(
            f"create requires a task bundle directory or its {BundleTaskLayout.DOCUMENT_NAME}, "
            f"not a loose YAML file: {resolved}"
        )
    return resolved


def scaffold_bundle_dirs(bundle_root: Path) -> None:
    """Create the bundle root, `plans/`, and `references/` (idempotent).

    Never removes or overwrites existing contents; `create --force` relies on
    this to leave `plans/` / `references/` intact while only `task.yaml` is
    overwritten.
    """
    bundle_root.mkdir(parents=True, exist_ok=True)
    (bundle_root / "plans").mkdir(exist_ok=True)
    (bundle_root / "references").mkdir(exist_ok=True)


def derive_task_slug_from_loose_yaml(doc: Path) -> str | None:
    """Best-effort task slug derived from a loose YAML document's filename, for hints only.

    Strips a trailing `.task.yaml`/`.task.yml` suffix if present, else `.yaml`/`.yml`;
    returns `None` when the remaining stem doesn't match the slug charset (nothing to hint).
    """
    name = doc.name
    for suffix in (".task.yaml", ".task.yml"):
        if name.endswith(suffix):
            stem = name[: -len(suffix)]
            break
    else:
        stem = doc.stem
    return stem if is_task_slug(stem) else None


def resolve_project_dest(raw: str | Path, layout: PiJobLayout) -> Path:
    """Resolve `project --to` to a bundle root directory.

    A bare slug (charset `^[a-z0-9]+(?:-[a-z0-9]+)*$`) resolves under `task_tasks_home()`,
    whether or not that bundle exists yet. Anything else is a path, resolved via
    `derive_bundle_root` (`…/task.yaml` -> parent, directory -> itself). A loose `*.yaml`/
    `*.yml` path is rejected here (with a derived-slug hint when available) rather than by
    `derive_bundle_root`'s generic `create`-flavored message, since `project` never targets
    a loose YAML file or a directory store.
    """
    text = str(raw)
    if is_task_slug(text):
        return task_tasks_home(layout) / text
    if "/" not in text and "\\" not in text:
        die(
            f"invalid task slug {text!r} for --to; slugs must match {TASK_SLUG_RE.pattern} "
            f"(use ./{text} or an absolute/~ path for a bundle directory)"
        )
    resolved = Path(raw).expanduser().resolve()
    if resolved.suffix.lower() in {".yaml", ".yml"} and resolved.name != BundleTaskLayout.DOCUMENT_NAME:
        hint = derive_task_slug_from_loose_yaml(resolved)
        hint_text = f"; maybe --to {hint}" if hint else ""
        die(
            "--to must be a task slug or a bundle directory/"
            f"{BundleTaskLayout.DOCUMENT_NAME}, not a loose YAML file: {resolved}{hint_text}"
        )
    return derive_bundle_root(resolved)


def loose_sibling_entries(doc: Path, plans_dir: Path) -> list[Path]:
    """Filesystem entries next to a loose YAML task document, excluding the document
    itself and its `<stem>.plans/` directory."""
    parent = doc.parent
    excluded = {doc.resolve(), plans_dir.resolve()}
    return sorted(
        (entry for entry in parent.iterdir() if entry.resolve() not in excluded),
        key=lambda entry: entry.name,
    )


def _copy_tree_merge(src: Path, dst: Path) -> None:
    """Copy `src`'s contents into `dst`, merging into any existing directory."""
    dst.mkdir(parents=True, exist_ok=True)
    for item in src.iterdir():
        target = dst / item.name
        if item.is_dir():
            _copy_tree_merge(item, target)
        else:
            shutil.copy2(item, target)


def copy_loose_artifacts(doc: Path, bundle_root: Path) -> None:
    """Copy a loose YAML task's sibling artifacts into a freshly scaffolded bundle.

    `<stem>.plans/` merges into `plans/`; other sibling directories are copied to the
    bundle root under their own name; sibling files land under `references/`. Nothing at
    the source is modified or removed here.

    Skips any sibling entry that is `bundle_root` itself or an ancestor of it, in case
    the destination was scaffolded as (or under) a sibling of the loose source - e.g. a
    freshly created `$PI_JOB_TASKS` living next to the source - which would otherwise
    recurse the (still being populated) destination into itself.
    """
    plans_dir = YamlTaskLayout(doc).plans_dir
    if plans_dir.is_dir():
        _copy_tree_merge(plans_dir, bundle_root / "plans")
    resolved_bundle_root = bundle_root.resolve()
    for entry in loose_sibling_entries(doc, plans_dir):
        resolved_entry = entry.resolve()
        if resolved_bundle_root == resolved_entry or resolved_bundle_root.is_relative_to(resolved_entry):
            continue
        if entry.is_dir():
            _copy_tree_merge(entry, bundle_root / entry.name)
        else:
            references_dir = bundle_root / "references"
            references_dir.mkdir(parents=True, exist_ok=True)
            shutil.copy2(entry, references_dir / entry.name)


def delete_loose_source(doc: Path, plans_dir: Path) -> list[Path]:
    """Delete only the loose YAML document and its `<stem>.plans/` directory after a
    successful `project`; other copied siblings remain at the source parent (manual
    cleanup if desired). Returns the paths actually removed."""
    removed: list[Path] = []
    if doc.is_file():
        doc.unlink()
        removed.append(doc)
    if plans_dir.is_dir():
        shutil.rmtree(plans_dir)
        removed.append(plans_dir)
    return removed




def task_slug_for_worktree(store: TaskStore, task_path: Path) -> str | None:
    """Bundle slug for a worktree recommendation, or `None` when not bundle-backed.

    A `BundleTaskLayout`-backed store (central-home or path-opened bundle) contributes
    its bundle root directory name as the slug segment; a loose `YamlTaskLayout` or a
    directory-backed `FsTaskStore` has no slug segment. `task_path` is accepted for
    symmetry with `recommend_worktree_path` but the slug is always derived from the
    open store's layout, not the raw CLI path."""
    layout = getattr(store, "layout", None)
    if isinstance(layout, BundleTaskLayout):
        return layout.bundle_root.name
    return None


def recommend_worktree_path(
    *,
    store: TaskStore,
    task_path: Path,
    slice_key: str,
    repo: str,
    layout: PiJobLayout,
) -> tuple[str, str | None]:
    """Recommended `$PI_JOB_WORKTREES/<slug>/<slice>/<repo>` path, plus an optional note.

    The slug segment is omitted for a non-bundle-backed task (loose YAML or directory
    store); the note then explains how to get a slug-addressable recommendation."""
    slug = task_slug_for_worktree(store, task_path)
    path = str(layout.worktree_path(slug=slug, slice_key=slice_key, repo=repo))
    note = (
        None
        if slug
        else "no bundle slug for this task; project it into the central task home for a slug-based path"
    )
    return path, note










def contract_step_kinds() -> dict[str, dict[str, Any]]:
    kinds: dict[str, dict[str, Any]] = {}
    for entry in load_profile_contract().get("step_kinds", {}).values():
        key = str(entry.get("key") or "")
        if key:
            kinds[key] = entry
    if not kinds:
        die(f"step/slice-kind contract has no usable step_kinds: {PROFILE}")
    return kinds


def contract_slice_kinds() -> dict[str, dict[str, Any]]:
    kinds: dict[str, dict[str, Any]] = {}
    for entry in load_profile_contract().get("slice_kinds", {}).values():
        key = str(entry.get("key") or "")
        if key:
            kinds[key] = entry
    if not kinds:
        die(f"step/slice-kind contract has no usable slice_kinds: {PROFILE}")
    return kinds


def valid_slice_kinds() -> set[str]:
    return set(contract_slice_kinds())


def get_step_kind(step_key: str) -> dict[str, Any]:
    kinds = contract_step_kinds()
    if step_key not in kinds:
        die(f"unknown step kind {step_key!r}; expected one of: {', '.join(sorted(kinds))}")
    return kinds[step_key]


def get_slice_kind(kind_key: str) -> dict[str, Any]:
    kinds = contract_slice_kinds()
    if kind_key not in kinds:
        die(f"unknown slice kind {kind_key!r}; expected one of: {', '.join(sorted(kinds))}")
    return kinds[kind_key]


def steps_from_kind_template(kind_key: str) -> list[tuple[str, str]]:
    template = get_slice_kind(kind_key).get("step_template") or []
    return [(str(step_key), str(get_step_kind(str(step_key)).get("title") or step_key)) for step_key in template]


def task_slice_kinds(task: dict[str, Any]) -> set[str]:
    return {str(sl.get("kind") or "") for sl in task.get("plan", {}).get("slices", []) if sl.get("kind")}


def slice_structure_issues(task: dict[str, Any], *, slice_key: str | None = None) -> list[str]:
    """Structural lint for plan.slices, independent of the step-ordering gate.

    Each slice must (1) carry a `kind` that exists in the contract's slice_kinds and
    (2) contain at least every step key its kind's step_template lists. Extra steps are
    fine; the template is the bare minimum. Placement across steps/final_steps does not
    matter - the union is checked - so a slice that keeps closing steps in final_steps
    still passes. Returns a list of human-readable issues (empty means conformant).

    When slice_key is set, only that slice is checked."""
    valid = valid_slice_kinds()
    valid_hint = ", ".join(sorted(valid))
    issues: list[str] = []
    slices = task.get("plan", {}).get("slices", []) or []
    for idx, sl in enumerate(slices):
        key = str(sl.get("key") or f"#{idx}")
        if slice_key is not None and key != slice_key:
            continue
        kind = sl.get("kind")
        if not kind:
            issues.append(f"slice {key!r}: missing 'kind' (expected one of: {valid_hint})")
            continue
        kind = str(kind)
        if kind not in valid:
            issues.append(f"slice {key!r}: unknown kind {kind!r} (expected one of: {valid_hint})")
            continue
        kind_contract = get_slice_kind(kind)
        template = [
            str(step_key)
            for step_key in (kind_contract.get("required_steps") or kind_contract.get("step_template") or [])
        ]
        present = {str(s.get("key") or "") for s in (sl.get("steps") or [])}
        present |= {str(s.get("key") or "") for s in (sl.get("final_steps") or [])}
        missing = [t for t in template if t not in present]
        if missing:
            issues.append(
                f"slice {key!r} (kind {kind!r}): missing required step(s) {', '.join(missing)}; "
                f"kind {kind!r} template requires: {', '.join(template)}"
            )
    return issues


def known_slice_keys(task: dict[str, Any]) -> str:
    slices = task.get("plan", {}).get("slices", []) or []
    return ", ".join(str(sl.get("key") or "") for sl in slices)


def slice_template_warnings(task: dict[str, Any]) -> list[str]:
    """Report persisted slices that predate additions to their new-slice template."""
    warnings: list[str] = []
    valid = valid_slice_kinds()
    for task_slice in task_slices(task):
        if task_slice.kind not in valid:
            continue
        template = [str(key) for key in (get_slice_kind(task_slice.kind).get("step_template") or [])]
        present = {step.key for step in task_slice.all_steps}
        missing = [key for key in template if key not in present]
        if missing:
            warnings.append(
                f"slice {task_slice.key!r} predates template step(s) {', '.join(missing)}; "
                "add them if this unfinished slice should adopt the latest workflow"
            )
    return warnings


def contract_toolbelt() -> dict[str, dict[str, Any]]:
    """Toolbelt catalog keyed by aid key."""
    toolbelt = load_profile_contract().get("toolbelt") or {}
    return {str(key): entry for key, entry in toolbelt.items()}


def toolbelt_for_kinds(kind_keys: set[str]) -> list[dict[str, Any]]:
    """Catalog aids whose `suits` intersects kind_keys, sorted by key."""
    aids = [
        entry
        for entry in contract_toolbelt().values()
        if kind_keys & set(entry.get("suits") or [])
    ]
    return sorted(aids, key=lambda e: str(e.get("key") or ""))


def toolbelt_for_kind(kind_key: str) -> list[dict[str, Any]]:
    return toolbelt_for_kinds({kind_key})


def task_artifacts(task: dict[str, Any]) -> dict[str, Any]:
    return task.get("orchestration", {}).get("artifacts", {}) or {}


def task_maintain(task: Mapping[str, Any]) -> list[dict[str, Any]]:
    """Registered keep-current surfaces, in persisted order."""
    raw = (task.get("orchestration") or {}).get("maintain") or []
    return [item for item in raw if isinstance(item, dict)]


def is_remote_uri(uri: str) -> bool:
    """True for http(s) URLs (PR/Jira). Local paths stay in `files` listings."""
    value = uri.strip().lower()
    return value.startswith(("http://", "https://"))


def maintain_block(task: Mapping[str, Any], *, include_empty: bool = True) -> list[str]:
    """Packet lines for orchestration.maintain. Copy lives in profile packets."""
    packets = load_profile_contract()["instruction_packets"]
    items = task_maintain(task)
    if not items:
        if not include_empty:
            return []
        return ["", *render_packet_lines(packets["maintain_empty"], defaults={})]
    lines = ["", *render_packet_lines(packets["maintain_header"], defaults={})]
    for item in items:
        lines.extend(
            render_packet_lines(
                packets["maintain_item"],
                defaults={
                    "uri": str(item.get("uri") or ""),
                    "note": str(item.get("note") or ""),
                },
            )
        )
    return lines


def status_value(item: TaskSlice | TaskStep | Mapping[str, Any]) -> str:
    if isinstance(item, (TaskSlice, TaskStep)):
        return item.status
    return str(item.get("status") or "").lower()


def owned_cursors(task: Mapping[str, Any]) -> list[OwnedCursor]:
    """All active owned claims, in persisted order."""
    raw = (task.get("orchestration") or {}).get("cursors") or []
    return [
        OwnedCursor(
            owner=str(c.get("owner") or ""),
            slice=str(c.get("slice") or ""),
            claimed_at=str(c.get("claimed_at") or ""),
            last_seen=str(c.get("last_seen") or ""),
        )
        for c in raw
    ]


def find_claim_by_owner(task: Mapping[str, Any], owner: str) -> OwnedCursor | None:
    return next((c for c in owned_cursors(task) if c.owner == owner), None)


def find_claim_by_slice(task: Mapping[str, Any], slice_key: str) -> OwnedCursor | None:
    return next((c for c in owned_cursors(task) if c.slice == slice_key), None)


def claim_ttl_seconds() -> float:
    hours = load_profile_contract().get("orchestration_defaults", {}).get("claim_stale_after_hours", 24.0)
    return float(hours) * 3600.0


def claim_is_stale(claim: OwnedCursor, *, now: datetime | None = None) -> bool:
    """A claim is stale when now - last_seen exceeds the profile TTL, or last_seen is
    unparseable (fail toward allowing displacement over wedging a slice forever)."""
    last_seen = parse_utc_timestamp(claim.last_seen)
    if last_seen is None:
        return True
    moment = now or datetime.now(UTC)
    return (moment - last_seen).total_seconds() > claim_ttl_seconds()


def claim_position(task: Mapping[str, Any], claim: OwnedCursor) -> Cursor:
    """Derived position for a claim: the claimed slice's first non-terminal step
    (steps then final_steps), or the bare slice when none remain. Never persisted."""
    return within_slice_cursor(task, claim.slice) or Cursor(slice=claim.slice)


def claim_label(task: Mapping[str, Any], claim: OwnedCursor) -> str:
    position = claim_position(task, claim)
    suffix = " (stale)" if claim_is_stale(claim) else ""
    return f"{claim.owner} \u2192 {position.label()}{suffix}"


def resolve_claim_for_command(
    task: Mapping[str, Any],
    args: argparse.Namespace,
    *,
    cmd: str,
    required: bool,
) -> OwnedCursor | None:
    """Resolve which claim a mutating/reading command acts on.

    Precedence: CLI --owner > env PI_JOB_OWNER > the sole active claim when
    unambiguous. Dies (when required) rather than guessing across >1 claims.
    """
    owner_id = getattr(args, "owner", None) or os.environ.get("PI_JOB_OWNER")
    claims = owned_cursors(task)
    if owner_id:
        owner_claims = [claim for claim in claims if claim.owner == owner_id]
        if len(owner_claims) > 1:
            die(
                f"multiple active claims for owner {owner_id!r}: {len(owner_claims)}; "
                "repair orchestration.cursors before retrying"
            )
        if not owner_claims and required:
            die(
                f"no active claim for owner {owner_id!r}; run "
                f"`pi-job claim --slice KEY --owner {owner_id}` first (see `pi-job show`)"
            )
        return owner_claims[0] if owner_claims else None
    if len(claims) == 1:
        return claims[0]
    if not required:
        return None
    if not claims:
        die(
            f"pi-job {cmd} needs an active claim; run `pi-job claim --slice KEY --owner ID` "
            "first (see `pi-job show`), or pass --owner / set PI_JOB_OWNER"
        )
    owners = ", ".join(c.owner for c in claims)
    die(
        f"ambiguous owner for pi-job {cmd}: {len(claims)} active claims ({owners}); "
        "pass --owner ID or set PI_JOB_OWNER"
    )


def require_initialized(task_file: Path, task: dict[str, Any]) -> None:
    if not task.get("orchestration"):
        die(
            "task is not initialized for pi-job: missing task.orchestration\n"
            f"run: pi-job --task {task_file} create [--kind setup|implement|... --goal \"…\"]"
        )


def slice_status_map(task: dict[str, Any]) -> dict[str, str]:
    """Return {slice_key: status_value} for all slices in the task."""
    return {task_slice.key: task_slice.status for task_slice in task_slices(task)}


def dependency_satisfied(task_slice: TaskSlice, status_map: Mapping[str, str]) -> bool:
    """Return True if all declared dependencies are done/skipped."""
    return all(status_map.get(key) in STATUS_DONE for key in task_slice.depends_on)


def is_actionable(task_slice: TaskSlice, status_map: Mapping[str, str]) -> bool:
    """Return True if slice is unfinished, not blocked, and has satisfied dependencies."""
    return (
        task_slice.status not in STATUS_DONE
        and task_slice.status != "blocked"
        and dependency_satisfied(task_slice, status_map)
    )


def unknown_dependency_keys(task: dict[str, Any], status_map: dict[str, str]) -> list[tuple[str, str]]:
    """Return [(slice_key, missing_dep_key)] for unknown dependency references."""
    missing = []
    for task_slice in task_slices(task):
        for dep_key in task_slice.depends_on:
            if dep_key not in status_map:
                missing.append((task_slice.key, dep_key))
    return missing


def ready_slices(task: dict[str, Any]) -> list[TaskSlice]:
    """Return dependency-satisfied unfinished slices (frontier). Array order is listing only."""
    status_map = slice_status_map(task)
    return [task_slice for task_slice in task_slices(task) if is_actionable(task_slice, status_map)]


def all_slices_done(task: dict[str, Any]) -> bool:
    """Return True if all slices are done/skipped."""
    return all(task_slice.status in STATUS_DONE for task_slice in task_slices(task))


def derived_task_status(task: dict[str, Any]) -> TaskStatus:
    """Overall task status from slice statuses. Stored task.status is ignored."""
    slices = task_slices(task)
    if not slices:
        return "planned"
    statuses = [task_slice.status for task_slice in slices]
    if any(status == "blocked" for status in statuses):
        return "blocked"
    if any(status == "in_progress" for status in statuses):
        return "in_progress"
    if all(status in STATUS_DONE for status in statuses):
        if all(status == "skipped" for status in statuses):
            return "skipped"
        return "done"
    if any(status in STATUS_DONE for status in statuses):
        return "in_progress"
    return "planned"



def slice_cursor(task_slice: TaskSlice) -> Cursor:
    """Return the first unfinished step cursor in a slice, or the slice itself if all steps are done."""
    for step in task_slice.steps:
        if step.status not in STATUS_DONE:
            return Cursor(slice=task_slice.key, step=step.key)

    for step in task_slice.final_steps:
        if step.status not in STATUS_DONE:
            return Cursor(slice=task_slice.key, step=step.key)

    return Cursor(slice=task_slice.key)


def within_slice_cursor(task: dict[str, Any], slice_key: str) -> Cursor | None:
    """First unfinished step in slice, or None if the slice is missing or has no unfinished steps.

    Empty-step actionable slices return a slice-only cursor so init/seed can land somewhere.
    """
    task_slice = find_slice(task, slice_key)
    if task_slice is None:
        return None
    if task_slice.status in STATUS_DONE or task_slice.status == "blocked":
        return None
    for step in task_slice.steps:
        if step.status not in STATUS_DONE:
            return Cursor(slice=task_slice.key, step=step.key)
    for step in task_slice.final_steps:
        if step.status not in STATUS_DONE:
            return Cursor(slice=task_slice.key, step=step.key)
    if not task_slice.steps and not task_slice.final_steps:
        return Cursor(slice=task_slice.key)
    return None


def seed_cursor(task: dict[str, Any]) -> Cursor | None:
    """Init/bootstrap seed only: first unfinished step of the first ready slice.

    Not a mid-run schedule - array order among Ready is a convenience seed, not execution order.
    """
    for task_slice in ready_slices(task):
        cursor = within_slice_cursor(task, task_slice.key)
        if cursor is not None:
            return cursor
    return None


def slice_has_unfinished_steps(task: dict[str, Any], slice_key: str) -> bool:
    """True when the slice exists and still has an unfinished step (or is empty-step actionable)."""
    return within_slice_cursor(task, slice_key) is not None


def slice_kind_policy(task_slice: TaskSlice) -> dict[str, Any]:
    if not task_slice.kind:
        return {}
    return get_slice_kind(task_slice.kind).get("policies") or {}


def merged_coding_policy(task: dict[str, Any], task_slice: TaskSlice | None) -> dict[str, Any]:
    contract_policy = (slice_kind_policy(task_slice) if task_slice else {}).get("coding_execution") or {}
    task_policy = task.get("orchestration", {}).get("policy", {}).get("coding_execution", {}) or {}
    merged = dict(contract_policy)
    merged.update(task_policy)
    return merged


def short_block_reason(note: str, *, limit: int = 72) -> str:
    """First line of a blocked-slice note, collapsed and truncated for status."""
    first = ""
    for line in (note or "").splitlines():
        collapsed = " ".join(line.split())
        if collapsed:
            first = collapsed
            break
    if len(first) > limit:
        return first[: limit - 3].rstrip() + "..."
    return first


def interrupt_park_steps() -> frozenset[str]:
    """Step keys from the profile that park the cursor for interrupt-friendly status."""
    return frozenset(str(s) for s in load_profile_contract()["interrupt_park_steps"])


def cursor_on_user_decision(task: Mapping[str, Any], cursor: Cursor | None) -> bool:
    """True when the saved cursor sits on grill/clarify (or requires_user_decision)."""
    if cursor is None or not cursor.step:
        return False
    if cursor.step in interrupt_park_steps():
        return True
    cur_slice = find_slice(task, cursor.slice)
    if cur_slice is None:
        return False
    step = find_current_step(cur_slice, cursor)
    if step is None:
        return False
    return bool(step_execution_policy(step).get("requires_user_decision"))


def print_status(
    ref: str,
    task: dict[str, Any],
    task_path: Path | None = None,
    *,
    unread: Sequence[Message] = (),
) -> None:
    print(f"Task: {task.get('title', '<untitled>')}")
    print(f"Task: {ref}")
    print(f"Status: {derived_task_status(task)}")
    print(f"Contract: {PROFILE}")
    if not task.get("orchestration"):
        print("Initialization: required")
        print(f"Next: initialize with pi-job --task {ref} create [--kind setup|implement|... --goal \"…\"]")
    else:
        print("Initialization: ok")
        structure_issues = slice_structure_issues(task)
        if structure_issues:
            count = len(structure_issues)
            issue_word = "issue" if count == 1 else "issues"
            print(
                f"Structure: invalid ({count} {issue_word}; "
                "try validate or validate --slice <key>)"
            )
        else:
            print("Structure: ok")
        claims = owned_cursors(task)
        if not claims:
            print("Cursors: <none>")
        else:
            print("Cursors:")
            for claim in sorted(claims, key=lambda c: c.owner):
                print(f"  {claim_label(task, claim)}")
        if unread:
            print(f"Inbox: {len(unread)} unread")
            for message in unread[:20]:
                print(render_message_status_line(message))
            if len(unread) > 20:
                print(f"  ... {len(unread) - 20} more unread")
            print(f"  read: pi-job --task {ref} msg --read --to manager")
        else:
            print("Inbox: <none>")
        for claim in claims:
            if cursor_on_user_decision(task, claim_position(task, claim)):
                hint = str(
                    load_profile_contract()["instruction_packets"]["status_interrupt_hint"]
                ).strip()
                if hint:
                    print(hint)
                break
        ready = ready_slices(task)
        if ready:
            ready_keys = [task_slice.key for task_slice in ready]
            print(f"Ready: {', '.join(ready_keys)}")
        else:
            print("Ready: none")
        blocked = [s for s in task_slices(task) if s.status == "blocked"]
        if blocked:
            parts: list[str] = []
            for task_slice in blocked:
                reason = short_block_reason(task_slice.note)
                parts.append(f"{task_slice.key} ({reason})" if reason else task_slice.key)
            print(f"Blocked: {', '.join(parts)}")

        if all_slices_done(task):
            print("Task: all slices done")
        elif not claims and not ready:
            print(
                "⚠ no active claim and Ready is empty "
                "(blocked on depends_on or all remaining slices blocked)"
            )
        elif not claims:
            print("Next: `pi-job claim --slice KEY --owner ID` (pick from Ready above)")
        else:
            status_map = slice_status_map(task)
            for claim in claims:
                cur_slice = find_slice(task, claim.slice)
                if cur_slice is None:
                    print(
                        f"⚠ {claim.owner}'s claim targets a missing slice {claim.slice!r}; "
                        f"run `pi-job release --owner {claim.owner}`"
                    )
                elif cur_slice.status == "blocked" or not is_actionable(cur_slice, status_map):
                    print(
                        f"⚠ {claim.owner}'s claim ({claim.slice}) is not Ready "
                        f"(status={cur_slice.status}); consider `pi-job release --owner {claim.owner}`"
                    )

        # Unknown dependency key warnings
        status_map = slice_status_map(task)
        unknown = unknown_dependency_keys(task, status_map)
        for slice_key, dep_key in unknown:
            print(f"⚠ slice '{slice_key}' depends_on unknown slice key '{dep_key}'")

    for issue in note_length_warnings(task, task_path):
        print(f"warning: {issue}")






































def split_csv(value: str | None) -> list[str]:
    return [v.strip() for v in value.split(",") if v.strip()] if value else []








































def toolbelt_add(store: TaskStore, task: dict[str, Any], args: argparse.Namespace) -> None:
    key = args.key
    if not key:
        die("toolbelt add requires an aid key, e.g. `toolbelt add sequence-diagram`")
    catalog = contract_toolbelt()
    if key not in catalog:
        die(f"unknown toolbelt aid {key!r}; expected one of: {', '.join(sorted(catalog))}")
    store.write_artifact(key, status=args.status, path=args.path, note=args.note)
    print(f"registered toolbelt aid: {key} [{args.status}]")


def toolbelt_block(task: dict[str, Any], kind_keys: set[str] | None = None) -> list[str]:
    kinds = kind_keys if kind_keys is not None else task_slice_kinds(task)
    aids = toolbelt_for_kinds(kinds) if kinds else []
    if not aids:
        return ["", "Toolbelt (planning aids): none suited to this task's slice kinds."]
    artifacts = task_artifacts(task)
    lines = ["", "Toolbelt (planning aids) — register with `pi-job toolbelt add <key>`:"]
    for aid in aids:
        key = str(aid.get("key") or "")
        registered = artifacts.get(key)
        status = str(registered.get("status")) if registered else "not registered"
        lines.append(f"- {key} [{status}] — {aid.get('purpose', '')}")
    return lines


def find_current_slice(task: Mapping[str, Any], cursor: Cursor) -> TaskSlice | None:
    for task_slice in task_slices(task):
        if task_slice.key == cursor.slice:
            return task_slice
    return None


def find_slice(task: Mapping[str, Any], slice_key: str) -> TaskSlice | None:
    for task_slice in task_slices(task):
        if task_slice.key == slice_key:
            return task_slice
    return None


def find_current_step(task_slice: TaskSlice | None, cursor: Cursor) -> TaskStep | None:
    if not task_slice or not cursor.step:
        return None
    return task_slice.find_step(cursor.step)


def find_step(task_slice: TaskSlice, step_key: str) -> TaskStep | None:
    return task_slice.find_step(step_key)




def parse_utc_timestamp(value: str) -> datetime | None:
    try:
        parsed = datetime.fromisoformat(value)
    except (TypeError, ValueError):
        return None
    if parsed.tzinfo is None or parsed.utcoffset() != UTC.utcoffset(parsed):
        return None
    return parsed


def is_fully_qualified_model(model: str) -> bool:
    return "/" in model and not model.startswith("/") and not model.endswith("/")


MODEL_ID_EXAMPLE = "openai/gpt-5.6-sol"
MODEL_ID_QUALIFICATION_HINT = (
    f"model ID must be fully qualified as provider/model (for example {MODEL_ID_EXAMPLE})"
)


def require_fully_qualified_model(model: str) -> str:
    normalized = model.strip()
    if not is_fully_qualified_model(normalized):
        die(MODEL_ID_QUALIFICATION_HINT)
    return normalized


def note_length_warnings(task: Mapping[str, Any], task_path: Path | None = None) -> list[str]:
    """Warn when slice/step notes or the on-disk task file exceed soft size limits."""
    offenders: list[tuple[str, int]] = []
    for task_slice in task_slices(task):
        if len(task_slice.note) > NOTE_WARN_CHARS:
            offenders.append((task_slice.key, len(task_slice.note)))
        for step in task_slice.all_steps:
            note_len = len(step.note)
            if note_len > NOTE_WARN_CHARS:
                offenders.append((f"{task_slice.key}/{step.key}", note_len))
    offenders.sort(key=lambda item: item[1], reverse=True)
    warnings: list[str] = []
    for label, length in offenders[:NOTE_WARN_TOP_N]:
        warnings.append(
            f"oversized note {label}: {length} chars (soft limit {NOTE_WARN_CHARS}); "
            "put long prose in the slice plan file, keep finish --note short"
        )
    if task_path is not None and task_path.is_file():
        file_size = task_path.stat().st_size
        if file_size > TASK_FILE_WARN_BYTES:
            warnings.append(
                f"task file size {file_size} bytes exceeds soft limit {TASK_FILE_WARN_BYTES}; "
                "prefer plan files over growing notes"
            )
    return warnings


def execution_issues(task: dict[str, Any]) -> list[str]:
    """Return warnings for legacy or malformed lifecycle metadata without making old tasks unreadable."""
    issues: list[str] = []
    for task_slice in task_slices(task):
        items: list[tuple[str, TaskSlice | TaskStep]] = [(task_slice.key, task_slice)]
        items += [(f"{task_slice.key}/{step.key}", step) for step in task_slice.all_steps]
        for label, item in items:
            execution = item.execution
            status = item.status
            if not execution:
                if status != "planned":
                    issues.append(f"{label}: {status} work has no execution metadata (legacy or externally modified)")
                continue
            model = execution.model.strip()
            started = parse_utc_timestamp(execution.started)
            ended_raw = execution.ended
            ended = parse_utc_timestamp(ended_raw) if ended_raw else None
            if not model:
                issues.append(f"{label}: execution.model is empty")
            elif not is_fully_qualified_model(model):
                issues.append(f"{label}: execution.model is not fully qualified as provider/model (for example {MODEL_ID_EXAMPLE})")
            if started is None:
                issues.append(f"{label}: execution.started is not a UTC ISO 8601 timestamp")
            if status in STATUS_DONE and ended is None:
                issues.append(f"{label}: completed execution has no valid ended timestamp")
            if status == "in_progress" and ended_raw:
                issues.append(f"{label}: in_progress execution unexpectedly has ended")
            if started is not None and ended is not None and ended < started:
                issues.append(f"{label}: execution.ended precedes execution.started")
    return issues


def policy_author_model(task_slice: TaskSlice, step: TaskStep) -> str | None:
    """Return the referenced author execution.model when step kind declares different_model_from_step."""
    source_step_key = str(step_execution_policy(step).get("different_model_from_step") or "")
    if not source_step_key:
        return None
    author_step = find_step(task_slice, source_step_key)
    if not author_step or not author_step.execution:
        return None
    author_model = author_step.execution.model.strip()
    return author_model or None


def enforce_lifecycle_mutate_guards(
    task_file: Path,
    task_slice: TaskSlice,
    item: TaskSlice | TaskStep,
    label: str,
    *,
    verb: str,
) -> None:
    """Blocked slice/target and terminal target guards shared by start and one-shot finish."""
    if task_slice.status == "blocked":
        die(
            f"slice is blocked: {task_slice.key}; "
            f"run pi-job --task {task_file} unblock-slice --slice {task_slice.key} first"
        )
    status = item.status
    if status == "blocked":
        die(f"cannot {verb} blocked work: {label} [blocked]")
    if status in STATUS_DONE:
        die(f"cannot {verb} completed work: {label} [{status}]")


def resolve_lifecycle_target(
    task: dict[str, Any], args: argparse.Namespace, *, claim: OwnedCursor | None
) -> tuple[TaskSlice, TaskSlice | TaskStep, str | None]:
    """Resolve the slice/step a start|finish call targets.

    An explicit --slice makes the claim irrelevant (no owner needed at all). Without
    --slice, this falls back to claim's derived position (caller already resolved -
    and, when required, died - via resolve_claim_for_command)."""
    if args.slice_only and args.step:
        die("--slice-only cannot be combined with --step")
    if args.slice:
        slice_key = args.slice
        position = None
    else:
        if claim is None:
            die(
                "no active claim; run `pi-job claim --slice KEY --owner ID` first "
                "(see `pi-job show`), or pass --slice explicitly"
            )
        position = claim_position(task, claim)
        slice_key = position.slice
    task_slice = find_slice(task, slice_key)
    if task_slice is None:
        die(f"slice not found: {slice_key!r}")
    if args.step:
        step_key = args.step
    elif args.slice_only or args.slice:
        step_key = None
    else:
        assert position is not None
        step_key = position.step
    if step_key is None:
        return task_slice, task_slice, None
    step = find_step(task_slice, step_key)
    if step is None:
        die(f"step not found: {slice_key}/{step_key}")
    return task_slice, step, step_key


def step_execution_policy(step: TaskStep | None) -> dict[str, Any]:
    if not step:
        return {}
    return try_get_step_kind(step.key) or {}


def has_lifecycle_policy(step_kind: dict[str, Any]) -> bool:
    return bool(step_kind.get("requires_user_decision") or step_kind.get("different_model_from_step"))


def step_policy_issue(
    task_slice: TaskSlice, step: TaskStep, *, model: str, status: str, note: str
) -> str | None:
    step_key = step.key or "<unknown>"
    step_kind = step_execution_policy(step)
    if status == "skipped" and step_kind.get("requires_user_decision"):
        lowered = note.lower()
        if "user" not in lowered or "declin" not in lowered:
            return f"{step_key} may be skipped only with an explicit user-declined reason"
        return None
    source_step_key = str(step_kind.get("different_model_from_step") or "")
    if not source_step_key:
        return None
    author_step = find_step(task_slice, source_step_key)
    author_model = author_step.execution.model.strip() if author_step and author_step.execution else ""
    if not author_model:
        return f"{source_step_key} has no recorded execution.model; record the code-author model before scanning"
    if not is_fully_qualified_model(author_model):
        return f"{source_step_key} execution.model is not fully qualified as provider/model (for example {MODEL_ID_EXAMPLE})"
    if status != "done":
        return None
    if model == author_model:
        return (
            f"{step_key} model must differ from {source_step_key} model "
            f"({model}); choose a different fully qualified model ID"
        )
    return None


def completed_step_policy_issue(task_slice: TaskSlice, step: TaskStep) -> str | None:
    step_kind = step_execution_policy(step)
    if not has_lifecycle_policy(step_kind):
        return None
    if step.status not in STATUS_DONE:
        return f"{step.key} is policy-governed and must be completed or explicitly skipped"
    execution = step.execution
    model = execution.model.strip() if execution else ""
    if not execution or not model or not execution.started or not execution.ended:
        return f"{step.key} requires model, started, and ended execution metadata"
    if not is_fully_qualified_model(model):
        return f"{step.key} execution.model must be fully qualified as provider/model (for example {MODEL_ID_EXAMPLE})"
    started = parse_utc_timestamp(execution.started)
    ended = parse_utc_timestamp(execution.ended)
    if started is None or ended is None:
        return f"{step.key} timestamps must be valid UTC ISO 8601 values"
    if ended < started:
        return f"{step.key} execution.ended precedes execution.started"
    return step_policy_issue(task_slice, step, model=model, status=step.status, note=step.note)


def lifecycle_lock(store: TaskStore):
    """Lock a YAML task across lifecycle read-check-write; other stores are unchanged."""

    return store.exclusive() if isinstance(store, YamlTaskStore) else nullcontext()


def resolve_owner_for_claim(args: argparse.Namespace) -> str:
    """`claim` always needs an explicit identity: CLI --owner, else $PI_JOB_OWNER.

    Unlike other commands, there is no sole-claim fallback here - one claim per owner
    (decision 10) means an owner can never usefully infer identity from an existing claim
    when creating a new one."""
    owner = args.owner or os.environ.get("PI_JOB_OWNER")
    if not owner:
        die("claim requires --owner ID or $PI_JOB_OWNER (no default identity is inferred)")
    return owner


def cmd_claim(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("claim requires a YAML task file (owned cursors are a YAML-only feature)")
    owner = resolve_owner_for_claim(args)
    slice_key = args.slice
    if not slice_key:
        die("--slice KEY is required")
    with store.exclusive():
        task = store.read()
        require_initialized(task_file, task)
        task_slice = find_slice(task, slice_key)
        if task_slice is None:
            die(f"slice not found: {slice_key!r}; known slice keys: {known_slice_keys(task)}")

        existing_by_owner = find_claim_by_owner(task, owner)
        if existing_by_owner is not None and existing_by_owner.slice != slice_key:
            die(
                f"{owner} already has a claim on {existing_by_owner.slice!r}; "
                f"run `pi-job release --owner {owner}` first (one claim per owner)"
            )

        existing_on_slice = find_claim_by_slice(task, slice_key)
        displaced = None
        if existing_on_slice is not None and existing_on_slice.owner != owner:
            if not claim_is_stale(existing_on_slice):
                die(
                    f"slice {slice_key!r} is already claimed by {existing_on_slice.owner} "
                    "(not stale); wait for release/auto-release or pick another Ready slice"
                )
            displaced = existing_on_slice.owner

        status_map = slice_status_map(task)
        if not is_actionable(task_slice, status_map) and existing_by_owner is None:
            die(
                f"slice {slice_key!r} is not Ready (status={task_slice.status}, "
                f"depends_on={list(task_slice.depends_on)}); pick from Ready via `pi-job show`"
            )

        now = utc_now()
        store.claim_slice(owner=owner, slice_key=slice_key, now=now)
        task = store.read()
        claim = find_claim_by_owner(task, owner)
        assert claim is not None
        if displaced:
            print(f"displaced stale claim: {displaced} → {slice_key}")
        print(f"claimed: {claim_label(task, claim)}")


def cmd_release(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("release requires a YAML task file (owned cursors are a YAML-only feature)")
    with store.exclusive():
        task = store.read()
        require_initialized(task_file, task)
        claim = resolve_claim_for_command(task, args, cmd="release", required=True)
        assert claim is not None
        label = claim_label(task, claim)
        store.release_claim(owner=claim.owner)
        print(f"released: {label}")


def cmd_start(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    with lifecycle_lock(store):
        task = store.read()
        require_initialized(task_file, task)
        claim = resolve_claim_for_command(task, args, cmd="start", required=not bool(args.slice))
        task_slice, item, step_key = resolve_lifecycle_target(task, args, claim=claim)
        label = f"{task_slice.key}/{step_key}" if step_key else task_slice.key
        enforce_lifecycle_mutate_guards(task_file, task_slice, item, label, verb="start")
        model = require_fully_qualified_model(args.model)
        existing = item.execution
        if existing:
            if existing.ended:
                die(f"cannot restart ended execution: {label}")
            if parse_utc_timestamp(existing.started) is None:
                die(f"existing execution.started is not a valid UTC ISO 8601 timestamp: {label}")
            if existing.model != model:
                die(
                    f"execution already belongs to model {existing.model!r}; "
                    "refusing to replace it without preserving provenance"
                )
            print(f"already started: {label} by {model} at {existing.started}")
            return
        if step_key is not None:
            if not isinstance(item, TaskStep):
                die(f"internal lifecycle target mismatch for step {step_key!r}")
            issue = step_policy_issue(task_slice, item, model=model, status="in_progress", note="")
            if issue:
                die(issue)
        execution = {"model": model, "started": utc_now()}
        store.set_execution(
            slice_key=task_slice.key, step_key=step_key, status="in_progress",
            note=item.note, execution=execution,
        )
        if claim is not None and isinstance(store, YamlTaskStore):
            store.touch_claim(owner=claim.owner, now=execution["started"])
        print(f"started: {label} by {model} at {execution['started']}")


def cmd_finish(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    with lifecycle_lock(store):
        task = store.read()
        require_initialized(task_file, task)
        claim = resolve_claim_for_command(task, args, cmd="finish", required=not bool(args.slice))
        task_slice, item, step_key = resolve_lifecycle_target(task, args, claim=claim)
        label = f"{task_slice.key}/{step_key}" if step_key else task_slice.key
        target_status = "skipped" if args.skip else "done"
        existing = item.execution
        if args.reconcile and args.skip:
            die("finish --reconcile cannot be combined with --skip")
        if args.replace and args.note is None:
            die("finish --replace requires --note")
        if args.replace and args.skip:
            die("finish --replace cannot be combined with --skip")
        if args.reconcile and item.status not in ("in_progress",):
            die(
                f"reconcile refused: {label} status is {item.status!r}; "
                "reconcile only applies to in_progress targets"
            )
        if item.status == target_status and existing and existing.ended:
            print(f"already finished: {label} [{target_status}] at {existing.ended}")
            return
        if args.skip and not args.reason:
            die("--skip requires --reason '<why>'")
        if args.reconcile and args.note is None:
            die("finish --reconcile requires --note '<evidence>'")
        one_shot = (
            not existing
            and not args.skip
            and not args.reconcile
            and args.slice
            and args.step
            and args.model
            and args.note is not None
            and str(args.note).strip()
        )
        if one_shot:
            enforce_lifecycle_mutate_guards(task_file, task_slice, item, label, verb="finish")
            model = require_fully_qualified_model(str(args.model))
            note = str(args.note).strip()
            if step_key is not None:
                if not isinstance(item, TaskStep):
                    die(f"internal lifecycle target mismatch for step {step_key!r}")
                issue = step_policy_issue(task_slice, item, model=str(model), status="done", note=note)
                if issue:
                    die(issue)
            now = utc_now()
            execution = {"model": str(model), "started": now, "ended": now}
            store.set_execution(
                slice_key=task_slice.key, step_key=step_key, status="done",
                note=note, execution=execution,
            )
            print(f"finished: {label} [done] at {now}")
            if claim is not None and isinstance(store, YamlTaskStore):
                store.touch_claim(owner=claim.owner, now=now)
            print("tip: prefer Markdown in --note; `pi-job markdown` renders notes formatted")
            return
        if (
            not existing
            and not args.skip
            and not args.reconcile
            and args.slice
            and args.step
            and args.model
            and args.note is not None
            and not str(args.note).strip()
        ):
            die("one-shot finish requires a non-empty --note")
        if not existing and not args.skip and not args.reconcile:
            die(
                f"work was not started: {label}; run pi-job start --model <id> first, "
                "or one-shot with --slice KEY --step KEY --model <id> --note '<evidence>'"
            )
        model = args.model or (existing.model if existing else None)
        if not model:
            die("finish requires an existing started execution or --model <fully-qualified-model-id>")
        model = require_fully_qualified_model(str(model))
        allow_model_change = False
        if existing and existing.model and args.model and existing.model != model:
            author_model = (
                policy_author_model(task_slice, item)
                if step_key is not None and isinstance(item, TaskStep)
                else None
            )
            # Decision-point start uses the author model; scanner finish resets started so
            # provenance reflects scan execution, not the wait before dispatch.
            allow_model_change = (
                target_status == "done"
                and author_model is not None
                and existing.model == author_model
                and model != author_model
            )
            if not allow_model_change:
                die(f"execution started by {existing.model!r}; refusing to finish it as {model!r}")
        now = utc_now()
        started = now if allow_model_change else (existing.started if existing else now)
        if parse_utc_timestamp(started) is None:
            die(f"execution.started is not a valid UTC ISO 8601 timestamp: {label}")
        note = item.note
        step_item: TaskStep | None = None
        if step_key is not None:
            if not isinstance(item, TaskStep):
                die(f"internal lifecycle target mismatch for step {step_key!r}")
            step_item = item
        if args.skip:
            step_kind = step_execution_policy(step_item)
            note = (
                f"User declined {step_key}: {args.reason}"
                if step_kind.get("requires_user_decision")
                else str(args.reason)
            )
        elif args.note is not None:
            note = merge_note(item.note, args.note, replace=args.replace)
        if step_key is not None:
            assert step_item is not None
            issue = step_policy_issue(task_slice, step_item, model=str(model), status=target_status, note=note)
            if issue:
                die(issue)
        else:
            for governed_step in task_slice.all_steps:
                issue = completed_step_policy_issue(task_slice, governed_step)
                if issue:
                    die(f"cannot finish slice: {issue}")
        if step_key is None and not args.skip:
            unfinished = [
                step.key for step in task_slice.all_steps if step.status not in STATUS_DONE
            ]
            if unfinished:
                die(f"cannot finish slice with unfinished steps: {', '.join(unfinished)}")
        execution = {"model": str(model), "started": started, "ended": now}
        store.set_execution(
            slice_key=task_slice.key, step_key=step_key, status=target_status,
            note=note, execution=execution,
        )
        print(f"finished: {label} [{target_status}] at {now}")
        if isinstance(store, YamlTaskStore):
            slice_terminal = step_key is None and target_status in STATUS_DONE
            if slice_terminal:
                released_owner = store.auto_release_slice(slice_key=task_slice.key)
                if released_owner:
                    print(f"auto-released: {released_owner}'s claim on {task_slice.key} [{target_status}]")
            elif claim is not None:
                store.touch_claim(owner=claim.owner, now=now)
        if args.note is not None:
            print("tip: prefer Markdown in --note; `pi-job markdown` renders notes formatted")


def try_get_step_kind(step_key: str) -> dict[str, Any] | None:
    return contract_step_kinds().get(step_key)


def step_owner(task: dict[str, Any], cursor: Cursor) -> str:
    task_slice = find_current_slice(task, cursor)
    step = find_current_step(task_slice, cursor)
    if step:
        step_kind = try_get_step_kind(step.key)
        if step_kind:
            return str(step_kind.get("owner") or "orchestrator")
    if cursor.step:
        step_kind = try_get_step_kind(cursor.step)
        if step_kind:
            return str(step_kind.get("owner") or "orchestrator")
    return "orchestrator"


def enforce_owner_policy(
    owner: str,
    *,
    policy: dict[str, Any],
    contract_policy: dict[str, Any],
) -> None:
    subagent_required = policy.get("subagent_required", contract_policy.get("subagent_required", True))
    exceptions = contract_policy.get("exceptions") or []
    if owner == "subagent" and not subagent_required and not exceptions:
        die(
            "step owner is subagent but coding_execution.subagent_required is false "
            "without a recorded exception"
        )
    if owner == "subagent" and subagent_required:
        return


def build_plan(
    store: TaskStore,
    task: dict[str, Any],
    layout: PiJobLayout,
) -> str:
    claims = owned_cursors(task)
    positions = {claim.owner: claim_position(task, claim) for claim in claims}
    ready = ready_slices(task)
    profile = load_profile_contract()
    packets = profile["instruction_packets"]

    ready_label = ", ".join(s.key for s in ready) if ready else "none"
    cursors_label = (
        ", ".join(claim_label(task, claim) for claim in claims) if claims else "<none>"
    )
    lines = [
        "PI-JOB TASK PLAN",
        "",
        f"Task: {task_display_ref(store, layout)}",
        f"Contract: {PROFILE}",
        f"Task cursors: {cursors_label}",
        f"Ready slices: {ready_label}",
        "",
        "Plan slices:",
    ]
    for index, task_slice in enumerate(task_slices(task), start=1):
        slice_key = task_slice.key
        kind = task_slice.kind or "<unset>"
        st = task_slice.status
        owners_here = [owner for owner, pos in positions.items() if pos.slice == slice_key]
        marker = ""
        if owners_here:
            marker = f"  <-- claimed by {', '.join(owners_here)}"
        elif any(s.key == slice_key for s in ready):
            marker = "  <-- ready"
        kind_entry = get_slice_kind(kind) if kind in valid_slice_kinds() else {}
        lines.append(f"{index:2}. {slice_key} [kind:{kind}/{st}] {task_slice.title}{marker}")
        if task_slice.layer:
            lines.append(f"    layer: {task_slice.layer}")
        if kind_entry.get("description"):
            lines.append(f"    kind: {kind_entry.get('title', kind)} — {kind_entry.get('description')}")
        for step in task_slice.all_steps:
            step_key = step.key
            step_st = step.status
            step_owners = [
                owner for owner, pos in positions.items()
                if pos.slice == slice_key and pos.step == step_key
            ]
            step_marker = f"  <-- current for {', '.join(step_owners)}" if step_owners else ""
            step_kinds = contract_step_kinds()
            owner = step_kinds.get(step_key, {}).get("owner", "?")
            lines.append(f"    - {step_key} [{owner}/{step_st}] {step.title}{step_marker}")
    lines += ["", "Orchestrator todo instruction:"]
    lines.extend(render_packet_lines(packets["plan_todo"], defaults={}))
    lines += [
        "",
        "Note: within-slice advance walks steps linearly; across slices the orchestrator picks from Ready (array order is not schedule).",
    ]
    lines += toolbelt_block(task)
    lines += maintain_block(task)
    return "\n".join(lines)


def slice_plan_file(task_file: Path, slice_key: str) -> Path:
    """Layout-aware Markdown plan path (pure): bundle `plans/` or loose `<stem>.plans/`."""
    return layout_for_document_path(task_file).slice_plan_file(slice_key)


def slice_plan_note_pointer(task_file: Path, slice_key: str) -> str:
    """create-plan step note value: layout-aware pointer relative to the task dir."""
    return layout_for_document_path(task_file).slice_plan_pointer(slice_key)


def slice_plan_markdown_label(layout: YamlTaskLayout | BundleTaskLayout, slice_key: str) -> str:
    """Backtick-friendly label for a `markdown --slice` plan body.

    Same layout-relative path as `slice_plan_pointer`, without the `Plan file: ` prefix:
    `plans/<slice>.md` for a bundle, `<stem>.plans/<slice>.md` for loose YAML."""
    pointer = layout.slice_plan_pointer(slice_key)
    prefix = "Plan file: "
    return pointer.removeprefix(prefix) if pointer.startswith(prefix) else pointer


def slice_kind_requires_plan_file(kind: str) -> bool:
    """True when the slice kind's step_template includes create-plan."""
    template = get_slice_kind(kind).get("step_template") or []
    return "create-plan" in template


def render_slice_plan_stub(
    *,
    key: str,
    goal: str,
    depends_on: list[str],
) -> str:
    """Pure: interpolate the profile plan-stub template."""
    template = str(load_profile_contract()["instruction_packets"]["slice_plan_stub"])
    body = template.format(
        key=key,
        goal=goal.strip() or "(fill)",
        depends_on=", ".join(depends_on) or "(none)",
    )
    return body if body.endswith("\n") else body + "\n"


def render_finding_entry(*, note: str, source: str, stamp: str) -> str:
    """Pure: one dated findings entry block."""
    header = f"## {stamp}"
    if source:
        header += f" ({source})"
    return f"{header}\n\n{note.rstrip()}\n\n"


def _truncate_seed_text(text: str, limit: int = 240) -> str:
    collapsed = " ".join(str(text).split())
    if len(collapsed) <= limit:
        return collapsed
    return collapsed[: limit - 3].rstrip() + "..."


def build_seed_slice_plans_block(
    layout: YamlTaskLayout | BundleTaskLayout, task: dict[str, Any], slices: list[dict[str, Any]]
) -> str:
    """Return a seed reminder block for qualifying slices, or empty string if none."""
    qualifying = [
        sl for sl in slices
        if slice_kind_requires_plan_file(str(sl.get("kind") or ""))
    ]
    if not qualifying:
        return ""

    profile = load_profile_contract()
    packets = profile["instruction_packets"]
    preamble = packets["seed_slice_plans"]

    lines = ["SEED SLICE PLAN FILES NOW", preamble.rstrip(), ""]
    for sl in qualifying:
        key = str(sl.get("key") or "")
        kind = str(sl.get("kind") or "")
        lines.append(f"- {key} [{kind}]")
        lines.append(f"  {layout.slice_plan_pointer(key)}")
        goal = str(sl.get("goal") or "").strip()
        if goal:
            lines.append(f"  Goal: {_truncate_seed_text(goal)}")
        depends_on = sl.get("depends_on") or []
        if depends_on:
            lines.append(f"  Depends on: {', '.join(str(dep) for dep in depends_on)}")
        repos = sl.get("repos") or []
        if repos:
            lines.append(f"  Repos: {', '.join(str(repo) for repo in repos)}")
        lines.append("")

    context = str(task.get("context") or "").strip()
    if context:
        lines.append("Shared context:")
        lines.append(f"  {_truncate_seed_text(context)}")

    decisions = task.get("decisions") or []
    if decisions:
        lines.append("Decisions:")
        for decision in decisions:
            date = str(decision.get("date") or "")
            note = _truncate_seed_text(str(decision.get("note") or ""))
            lines.append(f"  - {date}: {note}")

    return "\n".join(lines).rstrip() + "\n"


def render_packet_lines(text: str, *, defaults: dict[str, str]) -> list[str]:
    """Split packet text into non-empty stripped lines with placeholder interpolation."""

    lines: list[str] = []
    for line in text.strip().split("\n"):
        stripped = line.strip()
        if stripped:
            lines.append(stripped.format(**defaults))
    return lines


SLICE_GOAL_PREVIEW_MAX_CHARS = 500


def collapse_slice_goal(goal: str, *, slice_key: str) -> str:
    """Return a short slice-goal preview with a markdown pointer when the goal is long."""

    if len(goal) <= SLICE_GOAL_PREVIEW_MAX_CHARS:
        return goal
    preview = goal[:SLICE_GOAL_PREVIEW_MAX_CHARS].rstrip()
    if not preview.endswith("…"):
        preview += "…"
    return (
        f"{preview}\n"
        f"(full goal: pi-job --task TASK_FILE markdown --slice {slice_key} --with-decisions)"
    )


def render_record_results_lines(
    profile: dict[str, Any],
    step_kind: dict[str, Any] | None,
    *,
    defaults: dict[str, str],
) -> list[str]:
    """Step-scoped RECORD RESULTS blurbs from profile record_channels."""

    packets = profile["instruction_packets"]
    channels = profile["record_channels"]
    lines = ["RECORD RESULTS"]
    lines.extend(render_packet_lines(packets["record_results_intro"], defaults=defaults))
    if step_kind:
        for channel_id in step_kind.get("record_channels") or []:
            blurb = channels["blurbs"].get(channel_id)
            if blurb:
                lines.append(f"- {blurb}")
    return lines


class InstructionPacketBudget:
    """Byte-budget limits and measurement for instruction packets.

    Boundary: only this type splits execution body vs Subagent prompt and
    measures generic / step-specific / total body / prompt bytes. Tests and
    callers use ``measure()`` and the ClassVar limits; they do not re-parse
    packet section markers.
    """

    GENERIC_MAX_BYTES: ClassVar[int] = 1500
    TOTAL_MAX_BYTES: ClassVar[int] = 4500
    SUBAGENT_PROMPT_MAX_BYTES: ClassVar[int] = 2500

    _SUBAGENT_PROMPT_MARKER: ClassVar[str] = "\nSubagent prompt:"
    _STEP_MARKER: ClassVar[str] = "\nSTEP\n"
    _RECORD_MARKER: ClassVar[str] = "\nRECORD RESULTS\n"
    _TODO_MARKER: ClassVar[str] = "\nTodo tracking:\n"

    @classmethod
    def split_execution(cls, instruction: str) -> tuple[str, str]:
        """Split an execution packet into body (budgeted) and optional Subagent prompt tail."""

        marker = cls._SUBAGENT_PROMPT_MARKER
        if marker in instruction:
            body, prompt = instruction.split(marker, 1)
            return body, marker + prompt
        return instruction, ""

    @classmethod
    def measure(cls, instruction: str) -> dict[str, int]:
        """Measure generic, step-specific, total execution body, and subagent prompt byte counts."""

        body, prompt_tail = cls.split_execution(instruction)
        step_idx = body.find(cls._STEP_MARKER)
        record_idx = body.find(cls._RECORD_MARKER)
        todo_idx = body.find(cls._TODO_MARKER)
        if step_idx == -1 or record_idx == -1 or record_idx < step_idx:
            generic_bytes = len(body.encode("utf-8"))
            step_specific_bytes = 0
        else:
            record_block = body[record_idx : todo_idx if todo_idx != -1 else len(body)]
            record_lines = record_block.split("\n")
            intro_lines: list[str] = []
            blurb_lines: list[str] = []
            past_header = False
            for line in record_lines:
                if line == "RECORD RESULTS":
                    intro_lines.append(line)
                    past_header = True
                    continue
                if past_header and line.startswith("- "):
                    blurb_lines.append(line)
                elif past_header:
                    intro_lines.append(line)
            record_intro = "\n".join(intro_lines)
            blurbs = "\n".join(blurb_lines)
            after_todo = body[todo_idx:] if todo_idx != -1 else ""
            generic_bytes = len(
                (body[: step_idx + len(cls._STEP_MARKER)] + record_intro + after_todo).encode("utf-8")
            )
            step_specific_bytes = len(
                (body[step_idx + len(cls._STEP_MARKER) : record_idx] + blurbs).encode("utf-8")
            )
        total_body_bytes = len(body.encode("utf-8"))
        prompt_bytes = len(prompt_tail.encode("utf-8")) if prompt_tail else 0
        return {
            "generic_bytes": generic_bytes,
            "step_specific_bytes": step_specific_bytes,
            "total_body_bytes": total_body_bytes,
            "subagent_prompt_bytes": prompt_bytes,
        }


def build_pick_next_instruction(
    store: TaskStore,
    task_file: Path,
    task: dict[str, Any],
    claim: OwnedCursor,
    layout: PiJobLayout,
) -> str:
    """Packet when claim's slice has no unfinished steps - owner picks via show."""
    require_initialized(task_file, task)
    cursor = claim_position(task, claim)
    ready = ready_slices(task)
    profile = load_profile_contract()
    packets = profile["instruction_packets"]
    packet_defaults = {
        "cursor": cursor.label(),
        "owner": claim.owner,
        "task_file": str(task_file),
        "slice_key": cursor.slice,
    }
    lines = [
        "PI-JOB PICK NEXT SLICE",
        "",
        f"Task: {task_display_ref(store, layout)}",
        f"Repository root: {ROOT}",
        f"Contract: {PROFILE}",
        f"Claim: {claim.owner}",
        f"Exhausted claim: {cursor.label()}",
        (
            "Role: orchestrator (CLI-only store; pause on grill/clarify/user-decision)."
        ),
        "",
    ]
    if all_slices_done(task):
        lines += ["Status: all slices done", "", "Stop - the task has no remaining work."]
        return "\n".join(lines)

    if ready:
        lines.append("Ready slices (deps satisfied; choose one - array order is not schedule):")
        for task_slice in ready:
            step_cursor = within_slice_cursor(task, task_slice.key)
            step_label = step_cursor.label() if step_cursor else task_slice.key
            goal = f" - {task_slice.goal}" if task_slice.goal else ""
            lines.append(f"- {task_slice.key} [{task_slice.kind}] first step: {step_label}{goal}")
    else:
        lines.append("Ready slices: none")
        lines.append("Blocked or waiting on depends_on - fix deps or unblock a slice before picking.")
    lines.append("")
    lines.append("NEXT ACTION")
    lines.extend(render_packet_lines(packets["pick_next_slice"], defaults=packet_defaults))
    lines += maintain_block(task, include_empty=False)
    return "\n".join(lines)


def build_instruction(
    store: TaskStore,
    task_file: Path,
    task: dict[str, Any],
    cursor: Cursor,
    *,
    claim: OwnedCursor,
    layout: PiJobLayout,
) -> str:
    require_initialized(task_file, task)
    task_slice = find_current_slice(task, cursor)
    step = find_current_step(task_slice, cursor)
    contract_policy = merged_coding_policy(task, task_slice)
    owner = step_owner(task, cursor)
    enforce_owner_policy(
        owner,
        policy=task.get("orchestration", {}).get("policy", {}).get("coding_execution", {}),
        contract_policy=contract_policy,
    )
    step_kind = None
    if step:
        step_kind = try_get_step_kind(step.key)
    elif cursor.step:
        step_kind = try_get_step_kind(cursor.step)
    title = step.title if step else task_slice.title if task_slice else cursor.label()
    note = step.note if step else task_slice.note if task_slice else ""

    profile = load_profile_contract()
    packets = profile["instruction_packets"]
    packet_defaults = {
        "cursor": cursor.label(),
        "task_file": str(task_file),
        "slice_key": task_slice.key if task_slice else (cursor.slice or "<unknown>"),
        "owner": claim.owner,
    }

    lines = [
        "PI-JOB EXECUTION INSTRUCTION",
        "",
        f"Task: {task_display_ref(store, layout)}",
        f"Repository root: {ROOT}",
        f"Contract: {PROFILE}",
        f"Current cursor: {cursor.label()}",
        f"Owner: {claim.owner}",
        f"Claim: {claim.owner}",
        (
            "Role: orchestrator (CLI-only store; pause on grill/clarify/user-decision)."
            if owner == "orchestrator"
            else "Role: orchestrator dispatching subagent (CLI-only store)."
        ),
        "",
        "NEXT ACTION",
    ]
    lines.extend(render_packet_lines(packets["next_action"], defaults=packet_defaults))
    lines.append("")
    lines.append("STEP")
    if task_slice:
        lines.append(f"Slice: {task_slice.key} [{task_slice.kind}] — {task_slice.title or '<untitled>'}")
        if task_slice.goal:
            lines.append(f"Slice goal: {collapse_slice_goal(task_slice.goal, slice_key=task_slice.key)}")
        if task_slice.note:
            lines.append(f"Slice note: {task_slice.note}")
    if step:
        lines.append(f"Step: {step.key} — {step.title or '<untitled>'}")
    lines.append(f"Step title: {title or '<untitled>'}")
    if note:
        lines.append(f"Step note: {note}")
    execution = (step.execution if step else task_slice.execution if task_slice else None)
    if execution:
        lines.append(
            f"Execution: model={execution.model or '<unset>'} "
            f"started={execution.started or '<unset>'} ended={execution.ended or '<open>'}"
        )
    if task_slice and step and step.key in ("create-plan", "grill-plan"):
        plan_path = slice_plan_file(task_file, task_slice.key)
        lines.append(
            f"Slice plan file: {plan_path} "
            f"(create-plan writes this Markdown; grill-plan revises it; "
            f"create-plan note must be only `{slice_plan_note_pointer(task_file, task_slice.key)}`)"
        )
    if step_kind:
        lines += [
            f"Step kind: {step_kind.get('key')} — {step_kind.get('title', '')}",
        ]
        validators = step_kind.get("validators") or []
        if validators:
            lines.append(f"Validators: {', '.join(validators)}")
        skip_rule = step_kind.get("skip_rule")
        if skip_rule:
            lines.append(f"Skip rule: {skip_rule}")
        guidance = step_kind.get("guidance")
        if guidance:
            lines.append(f"Guidance: {guidance}")
        for gate in step_kind.get("artifact_gates") or []:
            req = "required" if gate.get("required") else "optional"
            lines.append(
                f"Artifact gate: {gate.get('key')} ({req}) — {gate.get('output')} when {gate.get('when')}"
            )
        if step and has_lifecycle_policy(step_kind):
            source_step_key = str(step_kind.get("different_model_from_step") or "")
            lines.append("Step lifecycle policy:")
            if step_kind.get("requires_user_decision"):
                lines += [
                    "- Ask the user whether to run this step before dispatching its executor.",
                    "- If declined, run `finish --skip --model cursor/grok-4.6 --reason '<user decision>'`.",
                ]
            if source_step_key:
                source_step = find_step(task_slice, source_step_key) if task_slice else None
                source_model = source_step.execution.model if source_step and source_step.execution else "<missing>"
                lines += [
                    f"- Model recorded on {source_step_key}: {source_model}",
                    (
                        "- When this step completes as done, choose a different fully qualified "
                        "model ID than that author model."
                    ),
                    (
                        "- When a higher-reasoning / higher-capability model exists than that "
                        "author model, run this review/scan on that higher model."
                    ),
                    (
                        "- Do not reuse a fast coding or low-cost edit model for review/scan."
                    ),
                ]
            lines.append("- Record the outcome and resolve findings or explicitly record accepted risk before advancing.")

    subagent_required = contract_policy.get("subagent_required", True)
    lower_power = contract_policy.get("lower_power_model_preferred", True)
    reviews = contract_policy.get("orchestrator_reviews_subagent", True)
    lines += [
        "Execution policy:",
        f"- subagent_required: {subagent_required}",
        f"- lower_power_model_preferred: {lower_power}",
        f"- orchestrator_reviews_subagent: {reviews}",
    ]
    if step and step.key in ("confirm-layers", "select-toolbelt", "plan-slices"):
        lines.append("")
        lines.extend(format_layer_list_lines(task))
    if step and step.key in ("select-toolbelt", "plan-slices"):
        lines += toolbelt_block(task, task_slice_kinds(task))
    if step and step.key == "plan-slices":
        plan_slices = (task.get("plan") or {}).get("slices") or []
        if any(str(sl.get("kind") or "") != "setup" for sl in plan_slices):
            banner = packets.get("plan_slices_seeded_banner")
            if banner:
                lines.append("")
                lines.extend(render_packet_lines(str(banner), defaults=packet_defaults))
    lines += maintain_block(task, include_empty=False)

    lines.append("")
    lines.extend(render_record_results_lines(profile, step_kind, defaults=packet_defaults))
    lines.append("")
    lines.append("Todo tracking:")
    lines.extend(render_packet_lines(packets["todo_tracking"], defaults=packet_defaults))
    lines.append("Future-work capture:")
    lines.extend(render_packet_lines(packets["future_work"], defaults=packet_defaults))

    if owner == "subagent":
        lines.append("")
        lines.append("Subagent prompt:")
        lines.extend(render_packet_lines(packets["subagent_prompt"], defaults=packet_defaults))
    return "\n".join(lines)


def cmd_status(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    unread = (
        MessageService.from_layout(store.layout).list(unread_only=True)
        if isinstance(store, YamlTaskStore)
        else []
    )
    print_status(
        task_display_ref(store, args.layout),
        task,
        task_path=task_file,
        unread=unread,
    )


def cmd_advance(args: argparse.Namespace) -> None:
    """Deprecated: position is now derived from an owned claim, not a stored cursor.

    Kept only so old muscle-memory invocations fail with actionable guidance instead
    of a Python traceback; there is no `orchestration.cursor` left to advance."""
    die(
        "`advance` is deprecated and no longer has a cursor to move: position is derived "
        "from your claim (see `pi-job claim`/`pi-job show`).\n"
        "- No active claim: `pi-job claim --slice KEY --owner ID`, then `pi-job instruction`\n"
        "- Slice exhausted: `pi-job finish --slice-only`, then claim a new Ready slice\n"
        "- Wrong step order within a slice: mark the step `finish --skip --reason '<why>'`"
    )


def cmd_instruction(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    with lifecycle_lock(store):
        task = store.read()
        require_initialized(task_file, task)
        claim = resolve_claim_for_command(task, args, cmd="instruction", required=True)
        assert claim is not None
        within = within_slice_cursor(task, claim.slice)
        if within is None:
            print(build_pick_next_instruction(store, task_file, task, claim, args.layout))
            return
        if isinstance(store, YamlTaskStore):
            store.touch_claim(owner=claim.owner, now=utc_now())
    print(build_instruction(store, task_file, task, within, claim=claim, layout=args.layout))


def cmd_plan(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    require_initialized(task_file, task)
    print(build_plan(store, task, args.layout))


def build_wayfinder_context(
    store: TaskStore,
    task: dict[str, Any],
    layout: PiJobLayout,
) -> str:
    """Reconstruct the Wayfinder map from the task file at the slice level (no step
    detail): the destination, recorded decisions, in-progress/done slice notes, and the
    planned work split into frontier (takeable now) vs fog (blocked by unfinished deps).
    This is the deterministic context the `wayfinder` step loads before charting."""
    status_map = slice_status_map(task)
    destination = (task.get("plan") or {}).get("note") or ""
    decisions = task.get("decisions") or []

    active: list[TaskSlice] = []
    frontier: list[TaskSlice] = []
    fog: list[TaskSlice] = []
    for task_slice in task_slices(task):
        if task_slice.status in STATUS_DONE or task_slice.status in ("in_progress", "blocked"):
            active.append(task_slice)
        elif dependency_satisfied(task_slice, status_map):
            frontier.append(task_slice)
        else:
            fog.append(task_slice)

    def slice_line(task_slice: TaskSlice, *, with_deps: bool = False) -> str:
        note = task_slice.note or ""
        suffix = f": {note}" if note else ""
        deps = ""
        if with_deps and task_slice.depends_on:
            deps = f" depends_on={list(task_slice.depends_on)}"
        return f"  - {task_slice.key} [{task_slice.kind or '?'}, {task_slice.status}]{deps}{suffix}"

    lines = [
        "PI-JOB WAYFINDER CONTEXT",
        "",
        f"Task: {task_display_ref(store, layout)}",
        "",
        "DESTINATION:",
        f"  {destination}" if destination else "  <unset>",
        "",
        "DECISIONS:",
    ]
    if decisions:
        for decision in decisions:
            lines.append(f"  - {decision.get('date') or '?'}: {decision.get('note') or ''}")
    else:
        lines.append("  (none recorded)")

    lines += ["", "IN PROGRESS / DONE:"]
    lines += [slice_line(s) for s in active] if active else ["  (none)"]

    lines += ["", "FRONTIER (takeable now):"]
    lines += [slice_line(s, with_deps=True) for s in frontier] if frontier else ["  (none)"]

    lines += ["", "FOG (blocked by unfinished decisions):"]
    if fog:
        for s in fog:
            unmet = [d for d in s.depends_on if status_map.get(d) not in STATUS_DONE]
            note = s.note or ""
            suffix = f": {note}" if note else ""
            lines.append(f"  - {s.key} [{s.kind or '?'}] blocked_by={unmet}{suffix}")
    else:
        lines.append("  (none)")

    return "\n".join(lines)


def cmd_wayfinder_context(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    require_initialized(task_file, task)
    print(build_wayfinder_context(store, task, args.layout))


def cmd_sync(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    require_initialized(task_file, task)
    status_filter = set(args.status.split(",")) if args.status else None
    print(build_sync_instruction(store, task, status_filter, args.layout))


def sync_candidate_slices(task: dict[str, Any], status_filter: set[str] | None) -> SyncCandidateSlices:
    """Slices worth re-verifying: with an explicit status_filter, exactly those statuses;
    otherwise any in_progress/blocked slice, or any slice carrying an open PR (its recorded
    state could have changed - e.g. merged - since last checked).

    Slices whose only open remainder is pi-job-feedback are listed separately as
    feedback leftovers and excluded from blocking ACTION REQUIRED work."""
    candidates: list[TaskSlice] = []
    for task_slice in task_slices(task):
        st = task_slice.status
        if status_filter is not None:
            if st in status_filter:
                candidates.append(task_slice)
            continue
        if st in ("in_progress", "blocked"):
            candidates.append(task_slice)
            continue
        repo_work = task_slice.repo_work or {}
        if any(pr.get("status") == "open" for work in repo_work.values() for pr in (work.get("prs") or [])):
            candidates.append(task_slice)
    blocking: list[TaskSlice] = []
    feedback_leftovers: list[TaskSlice] = []
    for task_slice in candidates:
        if slice_feedback_only_tail(task_slice):
            feedback_leftovers.append(task_slice)
        else:
            blocking.append(task_slice)
    return SyncCandidateSlices(
        blocking=tuple(blocking),
        feedback_leftovers=tuple(feedback_leftovers),
    )


def _slice_has_open_pr(task_slice: TaskSlice) -> bool:
    repo_work = task_slice.repo_work or {}
    return any(
        pr.get("status") == "open"
        for work in repo_work.values()
        for pr in (work.get("prs") or [])
    )


def _append_sync_slice_lines(lines: list[str], task_slice: TaskSlice, *, prefix: str) -> None:
    lines.append(f"{prefix}{task_slice.key} ({task_slice.status})")
    repo_work = task_slice.repo_work or {}
    for repo_name, work in repo_work.items():
        worktree = work.get("worktree")
        suffix = f"  worktree: {worktree}" if worktree else ""
        lines.append(f"   repo: {repo_name}{suffix}")
        for pr in work.get("prs") or []:
            lines.append(f"   pr: {pr.get('url')} [{pr.get('status')}]")
    current = slice_cursor(task_slice)
    if current.step:
        step = find_current_step(task_slice, current)
        step_status = status_value(step) if step else "?"
        lines.append(f"   current step: {current.step} ({step_status})")
    lines.append("")


def build_sync_instruction(
    store: TaskStore,
    task: dict[str, Any],
    status_filter: set[str] | None,
    layout: PiJobLayout,
) -> str:
    candidates = sync_candidate_slices(task, status_filter)
    pipeline_text = load_profile_contract()["sync_pipeline_instructions"]

    lines = [
        "PI-JOB SYNC PIPELINE",
        "",
        f"Task: {task_display_ref(store, layout)}",
        f"Repository root: {ROOT}",
        f"{len(candidates.blocking)} slice(s) to verify.",
        "",
    ]
    if not candidates.blocking and not candidates.feedback_leftovers:
        lines.append("Nothing matched the sync criteria (in_progress/blocked slices, or slices with an open PR).")
        return "\n".join(lines)

    if candidates.blocking:
        lines += [pipeline_text, ""]
        for i, task_slice in enumerate(candidates.blocking, start=1):
            _append_sync_slice_lines(lines, task_slice, prefix=f"{i}. ")

    if candidates.feedback_leftovers:
        if not candidates.blocking:
            lines.append(
                "No blocking slices require live verification. "
                "Feedback leftover slices below are informational only."
            )
            lines.append("")
        lines.append("Feedback leftover (non-blocking, no live verification required):")
        for task_slice in candidates.feedback_leftovers:
            _append_sync_slice_lines(lines, task_slice, prefix="- ")
    return "\n".join(lines).rstrip() + "\n"


def slice_counts(task_slice: TaskSlice) -> tuple[int, int]:
    done = sum(1 for step in task_slice.all_steps if step.status in STATUS_DONE)
    return done, len(task_slice.all_steps)


def step_line(
    step: TaskStep,
    *,
    terminal: bool,
    current_step: str | None,
    color: bool,
    include_model: bool = False,
) -> str:
    st = step.status
    raw = "·" if (terminal and st not in STATUS_DONE) else GLYPH.get(st, "○")
    paint_status = "planned" if raw == "·" else st
    glyph = paint_glyph(raw, paint_status, color=color)
    key = step.key
    if current_step is not None and key == current_step:
        marker = "← current"
        suffix = f"   {_CURRENT_STYLE}{marker}{_RESET}" if color else f"   {marker}"
    else:
        suffix = ""
    execution_suffix = (
        f"   [{step.execution.model}]"
        if include_model and step.execution and step.execution.model
        else ""
    )
    return f"    {glyph} {key}{execution_suffix}{suffix}"


def append_multiline_note(
    lines: list[str],
    label_prefix: str,
    note: str,
    continuation_indent: str,
) -> None:
    if not note:
        return
    parts = note.split("\n")
    lines.append(f"{label_prefix}{parts[0]}")
    for part in parts[1:]:
        lines.append(f"{continuation_indent}{part}")


def append_set_worktree_lines(lines: list[str], task_slice: TaskSlice) -> None:
    """Append repo_work lines only for repos with a recorded worktree path.

    Used by status-filtered show so agents can inventory set worktrees on done
    slices without --all (default tree keeps those slices header-only).
    """
    repo_work = task_slice.repo_work or {}
    for repo_name in sorted(repo_work):
        work = repo_work[repo_name] or {}
        worktree = work.get("worktree")
        if worktree:
            lines.append(f"    repo_work[{repo_name}]: worktree={worktree}")


def append_slice_work_lines(
    lines: list[str],
    task_slice: TaskSlice,
    status_map: Mapping[str, str],
) -> None:
    if task_slice.layer:
        lines.append(f"    layer: {task_slice.layer}")
    if task_slice.depends_on:
        # Done/skipped deps are satisfied noise; only print open or missing ones.
        dep_strs = []
        for dep_key in task_slice.depends_on:
            dep_status = status_map.get(dep_key, "missing")
            if dep_status in STATUS_DONE:
                continue
            dep_strs.append(f"{dep_key}:{dep_status}")
        if dep_strs:
            lines.append(f"    deps: [{', '.join(dep_strs)}]")

    repo_work = task_slice.repo_work or {}
    for repo_name in sorted(repo_work):
        work = repo_work[repo_name] or {}
        worktree = work.get("worktree") or "not set"
        lines.append(f"    repo_work[{repo_name}]: worktree={worktree}")
        for pr in work.get("prs", []) or []:
            lines.append(f"        pr {pr.get('status')} {pr.get('url')}")


def step_detail_line(
    step: TaskStep,
    *,
    terminal: bool,
    current_step: str | None,
    color: bool,
    include_model: bool = True,
) -> list[str]:
    st = step.status
    raw = "·" if (terminal and st not in STATUS_DONE) else GLYPH.get(st, "○")
    paint_status = "planned" if raw == "·" else st
    glyph = paint_glyph(raw, paint_status, color=color)
    key = step.key
    execution_suffix = (
        f" [{step.execution.model}]"
        if include_model and step.execution and step.execution.model
        else ""
    )
    if current_step is not None and key == current_step:
        marker = "← current"
        cursor_suffix = f"   {_CURRENT_STYLE}{marker}{_RESET}" if color else f"   {marker}"
    else:
        cursor_suffix = ""
    lines = [f"    {glyph} {key} [{st}]{execution_suffix}{cursor_suffix}"]
    append_multiline_note(lines, "        note: ", step.note, "            ")
    return lines


def render_slice_scoped(
    task: Mapping[str, Any],
    task_slice: TaskSlice,
    *,
    positions: Mapping[str, Cursor],
    status_map: Mapping[str, str],
    color: bool,
    include_model: bool = True,
) -> list[str]:
    lines = [f"{task.get('title', '<untitled>')}"]
    title = task_slice.title or "<untitled>"
    lines.append(
        f"slice: {task_slice.key} [{task_slice.kind}] — {title} [{task_slice.status}]"
    )
    if task_slice.goal:
        lines.append(f"goal: {task_slice.goal}")
    if task_slice.layer:
        lines.append(f"layer: {task_slice.layer}")
    append_multiline_note(lines, "note: ", task_slice.note, "    ")
    append_slice_work_lines(lines, task_slice, status_map)
    current = positions.get(task_slice.key)
    current_step = current.step if current else None
    for step in task_slice.steps:
        lines.extend(
            step_detail_line(
                step,
                terminal=False,
                current_step=current_step,
                color=color,
                include_model=include_model,
            )
        )
    for step in task_slice.final_steps:
        lines.extend(
            step_detail_line(
                step,
                terminal=True,
                current_step=current_step,
                color=color,
                include_model=include_model,
            )
        )
    return lines


def escape_md_inline(text: str) -> str:
    """Escape characters that would break Markdown headings or inline structure."""

    return (
        text.replace("\\", "\\\\")
        .replace("#", "\\#")
        .replace("[", "\\[")
        .replace("]", "\\]")
        .replace("`", "\\`")
    )


def append_blockquote(lines: list[str], text: str, *, indent: str = "") -> None:
    """Append Markdown blockquote lines so recorded Markdown stays formatted."""

    body = text.splitlines() or [""]
    for line in body:
        lines.append(f"{indent}> {line}" if line else f"{indent}>")


def append_markdown_prose(lines: list[str], text: str) -> None:
    """Append recorded Markdown as section body (not fenced)."""

    lines.extend(text.splitlines() or [""])


def append_markdown_field(lines: list[str], label: str, text: str) -> None:
    if not text:
        return
    lines.append(f"**{label}:**")
    lines.append("")
    append_blockquote(lines, text)


def execution_markdown_lines(execution: ExecutionRecord | None) -> list[str]:
    if not execution or not (execution.model or execution.started or execution.ended):
        return []
    parts = []
    if execution.model:
        parts.append(f"model={execution.model}")
    if execution.started:
        parts.append(f"started={execution.started}")
    if execution.ended:
        parts.append(f"ended={execution.ended}")
    return [f"**Execution:** {', '.join(parts)}"]


def collect_execution_timestamps(execution: ExecutionRecord | None) -> list[str]:
    if not execution:
        return []
    timestamps: list[str] = []
    if execution.started:
        timestamps.append(execution.started)
    if execution.ended:
        timestamps.append(execution.ended)
    return timestamps


def slice_earliest_change(task_slice: TaskSlice) -> str | None:
    timestamps: list[str] = []
    timestamps.extend(collect_execution_timestamps(task_slice.execution))
    for step in task_slice.all_steps:
        timestamps.extend(collect_execution_timestamps(step.execution))
    return min(timestamps) if timestamps else None


def slice_latest_change(task_slice: TaskSlice) -> str | None:
    timestamps: list[str] = []
    timestamps.extend(collect_execution_timestamps(task_slice.execution))
    for step in task_slice.all_steps:
        timestamps.extend(collect_execution_timestamps(step.execution))
    return max(timestamps) if timestamps else None


def work_first_open_rank(
    task_slice: TaskSlice,
    status_map: Mapping[str, str],
    claimed_slices: set[str],
) -> int:
    """Lower ranks surface first among unfinished slices."""
    if task_slice.key in claimed_slices:
        return 0
    if task_slice.status == "in_progress":
        return 1
    if is_actionable(task_slice, status_map):
        return 2
    if task_slice.status == "blocked":
        return 3
    return 4


def _newest_first_indexed(items: list[tuple[int, TaskSlice]]) -> list[tuple[int, TaskSlice]]:
    """Newest latest-change first; undated last; plan index ascending on ties."""
    dated: list[tuple[int, TaskSlice, str]] = []
    undated: list[tuple[int, TaskSlice]] = []
    for plan_index, task_slice in items:
        latest = slice_latest_change(task_slice)
        if latest is None:
            undated.append((plan_index, task_slice))
        else:
            dated.append((plan_index, task_slice, latest))
    # reverse on (time, -plan_index): newest first; equal time -> smaller plan_index first
    dated.sort(key=lambda item: (item[2], -item[0]), reverse=True)
    undated.sort(key=lambda item: item[0])
    return [(plan_index, task_slice) for plan_index, task_slice, _ in dated] + undated


def ordered_slices_for_show(
    task: Mapping[str, Any],
    *,
    claimed_slices: set[str],
    work_first: bool,
) -> list[TaskSlice]:
    """Plan order by default; --work-first puts open work on top (newest first), finished last."""
    indexed = list(enumerate(task_slices(task)))
    if not work_first:
        return [task_slice for _, task_slice in indexed]

    status_map = slice_status_map(task)
    open_items = [
        (plan_index, task_slice)
        for plan_index, task_slice in indexed
        if task_slice.status not in STATUS_DONE
    ]
    finished_items = [
        (plan_index, task_slice)
        for plan_index, task_slice in indexed
        if task_slice.status in STATUS_DONE
    ]

    ordered: list[TaskSlice] = []
    for rank in range(5):
        group = [
            item
            for item in open_items
            if work_first_open_rank(item[1], status_map, claimed_slices) == rank
        ]
        ordered.extend(task_slice for _, task_slice in _newest_first_indexed(group))
    ordered.extend(task_slice for _, task_slice in _newest_first_indexed(finished_items))
    return ordered


def ordered_slices_for_markdown(
    task: Mapping[str, Any],
    *,
    chronological: bool,
) -> tuple[TaskSlice, ...]:
    slices = list(task_slices(task))
    if not chronological:
        return tuple(slices)
    indexed = list(enumerate(slices))

    def sort_key(item: tuple[int, TaskSlice]) -> tuple[int, str, int]:
        plan_index, task_slice = item
        change_time = slice_earliest_change(task_slice)
        if change_time is None:
            return (1, "", plan_index)
        return (0, change_time, plan_index)

    indexed.sort(key=sort_key)
    return tuple(task_slice for _, task_slice in indexed)


def _prior_slices_all_setup(slices: Sequence[Mapping[str, Any]], prior_count: int) -> bool:
    """True when no prior slices exist or every prior slice is kind setup."""
    if prior_count <= 0:
        return True
    prior = list(slices)[:prior_count]
    return all(str(sl.get("kind") or "") == "setup" for sl in prior)


def render_decisions_cli(task: Mapping[str, Any]) -> str:
    """Compact stdout decisions footer (not markdown)."""
    decisions = task.get("decisions") or []
    lines = ["Decisions:"]
    for decision in decisions:
        date = str(decision.get("date") or "")
        source = str(decision.get("source") or "").strip()
        note = str(decision.get("note") or "").strip()
        header = f"- {date}"
        if source:
            header += f" ({source})"
        lines.append(header)
        if note:
            for note_line in note.split("\n"):
                lines.append(f"  {note_line}")
    return "\n".join(lines)


def print_decisions_after_slice_add(
    task: Mapping[str, Any],
    *,
    prior_slice_count: int,
    decisions_count: int,
) -> None:
    """Print decisions footer after add-slice or create seed paths.

    Uses visible task state at command start (prior slices and kinds). Each public
    command path calls this exactly once at the end of its stdout output.
    """
    if decisions_count <= 0:
        print("no decisions recorded")
        return

    slices = (task.get("plan") or {}).get("slices") or []
    show_full = _prior_slices_all_setup(slices, prior_slice_count)

    if show_full:
        print()
        print(render_decisions_cli(task))
        return

    print(f"{decisions_count} decisions unchanged")
    print("Full list: pi-job --task TASK_FILE markdown")


def slice_feedback_only_tail(task_slice: TaskSlice) -> bool:
    """True when every step except pi-job-feedback is terminal and feedback is still open."""
    if _slice_has_open_pr(task_slice):
        return False
    feedback = task_slice.find_step("pi-job-feedback")
    if feedback is None:
        return False
    if feedback.status not in ("planned", "in_progress"):
        return False
    for step in task_slice.all_steps:
        if step.key == "pi-job-feedback":
            continue
        if step.status not in STATUS_DONE:
            return False
    return True


@dataclass(frozen=True)
class SyncCandidateSlices:
    blocking: tuple[TaskSlice, ...]
    feedback_leftovers: tuple[TaskSlice, ...]


def render_decisions_markdown(task: Mapping[str, Any]) -> list[str]:
    lines = ["## Decisions", ""]
    decisions = task.get("decisions") or []
    if not decisions:
        lines.append("_none_")
        return lines
    for decision in decisions:
        date = escape_md_inline(str(decision.get("date") or ""))
        source = str(decision.get("source") or "").strip()
        note = str(decision.get("note") or "")
        header = f"- **{date}**"
        if source:
            header += f" ({escape_md_inline(source)})"
        lines.append(header)
        if note:
            lines.append("")
            append_blockquote(lines, note, indent="  ")
    return lines


def render_project_markdown(task: Mapping[str, Any]) -> list[str]:
    project = task.get("project") or {}
    fields = [
        ("key", str(project.get("key") or "")),
        ("name", str(project.get("name") or "")),
        ("route", str(project.get("route") or "")),
        ("context", str(project.get("context") or "")),
    ]
    if not any(value for _, value in fields):
        return []
    lines = ["## Project", ""]
    for name, value in fields:
        if not value:
            continue
        if name == "context":
            lines.append(f"- **{name}:**")
            lines.append("")
            append_blockquote(lines, value, indent="  ")
            continue
        lines.append(f"- **{name}:** {escape_md_inline(value)}")
    return lines


def render_source_markdown(task: Mapping[str, Any]) -> list[str]:
    source = task.get("source") or {}
    fields = [
        ("jira", str(source.get("jira") or "")),
        ("discovered", str(source.get("discovered") or "")),
        ("context", str(source.get("context") or "")),
    ]
    if not any(value for _, value in fields):
        return []
    lines = ["## Source", ""]
    for name, value in fields:
        if not value:
            continue
        if name == "context":
            lines.append(f"- **{name}:**")
            lines.append("")
            append_blockquote(lines, value, indent="  ")
            continue
        lines.append(f"- **{name}:** {escape_md_inline(value)}")
    return lines


def render_artifacts_markdown(task: Mapping[str, Any]) -> list[str]:
    artifacts = task_artifacts(task)
    if not artifacts:
        return []
    lines = ["## Artifacts", ""]
    for key in sorted(artifacts):
        artifact = artifacts[key] or {}
        status = str(artifact.get("status") or "")
        path = str(artifact.get("path") or "").strip()
        note = str(artifact.get("note") or "")
        header = f"- **{escape_md_inline(key)}** [{status}]"
        if path:
            header += f" `{path}`"
        lines.append(header)
        if note:
            lines.append("")
            append_blockquote(lines, note, indent="  ")
    return lines


def render_maintain_markdown(task: Mapping[str, Any]) -> list[str]:
    items = task_maintain(task)
    if not items:
        return []
    lines = ["## Keep current", ""]
    for item in items:
        uri = escape_md_inline(str(item.get("uri") or ""))
        note = str(item.get("note") or "")
        lines.append(f"- `{uri}`")
        if note:
            lines.append("")
            append_blockquote(lines, note, indent="  ")
    return lines


def render_layers_markdown(task: Mapping[str, Any]) -> list[str]:
    entries = task_layers(task)
    if not entries:
        return []
    lines = ["## Layers", ""]
    for index, entry in enumerate(entries, start=1):
        name = escape_md_inline(str(entry.get("name") or ""))
        desc = escape_md_inline(str(entry.get("description") or "").strip())
        if desc:
            lines.append(f"{index}. **{name}** - {desc}")
        else:
            lines.append(f"{index}. **{name}**")
        for ref in entry.get("references") or []:
            lines.append(f"   - ref: `{ref}`")
    return lines


def render_step_markdown(
    step: TaskStep,
    *,
    current_step: str | None,
    terminal: bool,
) -> list[str]:
    current_badge = " (current)" if current_step is not None and step.key == current_step else ""
    terminal_label = " (final)" if terminal else ""
    title = escape_md_inline(step.title or step.key or "<untitled>")
    lines = [f"- **{escape_md_inline(step.key)}**{current_badge}{terminal_label} [{step.status}] — {title}"]
    exec_lines = execution_markdown_lines(step.execution)
    if exec_lines:
        lines.extend(f"  {line}" for line in exec_lines)
    if step.note:
        lines.append("")
        append_blockquote(lines, step.note, indent="  ")
    return lines


def render_slices_toc(
    slice_items: tuple[TaskSlice, ...],
    *,
    cursors: Sequence[Cursor],
) -> list[str]:
    """Return a default table of contents for slices (key + title)."""

    if not slice_items:
        return []
    lines = ["## Contents", ""]
    for task_slice in slice_items:
        is_current = any(c.slice == task_slice.key for c in cursors)
        current_badge = " (current)" if is_current else ""
        key = escape_md_inline(task_slice.key)
        title = escape_md_inline(task_slice.title or task_slice.key or "<untitled>")
        status = escape_md_inline(task_slice.status)
        anchor = f"slice-{task_slice.key}"
        lines.append(
            f"- [{key}{current_badge} — {title}](#{anchor}) `[{status}]`"
        )
    return lines


def render_slice_markdown(
    task_slice: TaskSlice,
    *,
    cursors: Sequence[Cursor],
    summary: bool = False,
    plan_body: str | None = None,
    plan_label: str | None = None,
) -> list[str]:
    matching = [c for c in cursors if c.slice == task_slice.key]
    is_current_slice = bool(matching)
    current_step = matching[0].step if matching else None
    current_badge = " (current)" if is_current_slice else ""
    title = escape_md_inline(task_slice.title or task_slice.key or "<untitled>")
    lines = [
        f'<a id="slice-{task_slice.key}"></a>',
        (
            f"### {escape_md_inline(task_slice.key)}{current_badge} "
            f"[{task_slice.kind}] — {title} [{task_slice.status}]"
        ),
        "",
    ]
    if task_slice.goal:
        lines.append(f"**Goal:** {escape_md_inline(task_slice.goal)}")
        lines.append("")
    if task_slice.layer:
        lines.append(f"**Layer:** {escape_md_inline(task_slice.layer)}")
        lines.append("")
    if summary:
        while lines and lines[-1] == "":
            lines.pop()
        return lines
    if task_slice.repos:
        lines.append(f"**Repos:** {', '.join(escape_md_inline(repo) for repo in task_slice.repos)}")
        lines.append("")
    if task_slice.depends_on:
        lines.append(
            f"**Depends on:** {', '.join(escape_md_inline(dep) for dep in task_slice.depends_on)}"
        )
        lines.append("")
    exec_lines = execution_markdown_lines(task_slice.execution)
    if exec_lines:
        lines.extend(exec_lines)
        lines.append("")
    if task_slice.note:
        append_markdown_field(lines, "Note", task_slice.note)
        lines.append("")
    repo_work = task_slice.repo_work or {}
    for repo_name in sorted(repo_work):
        work = repo_work[repo_name] or {}
        lines.append(f"**Repo work ({escape_md_inline(repo_name)}):**")
        worktree = work.get("worktree")
        if worktree:
            lines.append(f"- **Worktree:** `{worktree}`")
        for pr in work.get("prs") or []:
            pr_status = str(pr.get("status") or "")
            pr_url = str(pr.get("url") or "")
            lines.append(f"- **PR:** {pr_status} {pr_url}")
            pr_note = str(pr.get("note") or "")
            if pr_note:
                lines.append("")
                append_blockquote(lines, pr_note, indent="  ")
        lines.append("")
    if task_slice.steps:
        lines.append("#### Steps")
        lines.append("")
        for step in task_slice.steps:
            lines.extend(
                render_step_markdown(step, current_step=current_step, terminal=False)
            )
            lines.append("")
    if task_slice.final_steps:
        lines.append("#### Final steps")
        lines.append("")
        for step in task_slice.final_steps:
            lines.extend(
                render_step_markdown(step, current_step=current_step, terminal=True)
            )
            lines.append("")
    if plan_body is not None:
        lines.append("#### Plan file")
        lines.append("")
        if plan_label:
            lines.append(f"`{plan_label}`")
            lines.append("")
        stripped = plan_body.rstrip()
        if stripped:
            lines.append(stripped)
            lines.append("")
    while lines and lines[-1] == "":
        lines.pop()
    return lines


def render_task_markdown(
    task: Mapping[str, Any],
    *,
    chronological: bool = False,
    summary: bool = False,
    slice_key: str | None = None,
    with_decisions: bool = False,
    with_preamble: bool = False,
    plan_bodies: Mapping[str, str] | None = None,
    plan_labels: Mapping[str, str] | None = None,
) -> str:
    cursors = [claim_position(dict(task), claim) for claim in owned_cursors(dict(task))]
    title = escape_md_inline(str(task.get("title") or "<untitled>"))
    status = derived_task_status(dict(task))
    lines = [f"# {title}", "", f"**Status:** {status}", ""]

    # Lean --slice: omit decisions/preamble unless opted in. Full/summary keep them.
    lean_slice = bool(slice_key) and not summary
    include_decisions = (not lean_slice) or with_decisions
    include_preamble = (not lean_slice) or with_preamble
    bodies = plan_bodies or {}
    labels = plan_labels or {}

    if include_preamble:
        project_lines = render_project_markdown(task)
        if project_lines:
            lines.extend(project_lines)
            lines.append("")

    if include_decisions:
        lines.extend(render_decisions_markdown(task))
        lines.append("")

    if include_preamble and not summary:
        context = str(task.get("context") or "")
        if context:
            lines.append("## Context")
            lines.append("")
            append_markdown_prose(lines, context)
            lines.append("")

        source_lines = render_source_markdown(task)
        if source_lines:
            lines.extend(source_lines)
            lines.append("")

        plan_note = str((task.get("plan") or {}).get("note") or "")
        if plan_note:
            lines.append("## Plan note")
            lines.append("")
            append_markdown_prose(lines, plan_note)
            lines.append("")

        artifact_lines = render_artifacts_markdown(task)
        if artifact_lines:
            lines.extend(artifact_lines)
            lines.append("")

        maintain_lines = render_maintain_markdown(task)
        if maintain_lines:
            lines.extend(maintain_lines)
            lines.append("")

        layers_lines = render_layers_markdown(task)
        if layers_lines:
            lines.extend(layers_lines)
            lines.append("")

    slice_items = ordered_slices_for_markdown(task, chronological=chronological)
    if slice_key:
        matched = tuple(s for s in slice_items if s.key == slice_key)
        if not matched:
            known = ", ".join(s.key for s in task_slices(task)) or "(none)"
            die(f"slice not found: {slice_key!r}; known slice keys: {known}")
        slice_items = matched

    if slice_items:
        if not lean_slice:
            toc_lines = render_slices_toc(slice_items, cursors=cursors)
            lines.extend(toc_lines)
            lines.append("")
        lines.append("## Slices")
        lines.append("")
        for index, task_slice in enumerate(slice_items):
            body = bodies.get(task_slice.key) if lean_slice else None
            label = labels.get(task_slice.key) if body is not None else None
            lines.extend(
                render_slice_markdown(
                    task_slice,
                    cursors=cursors,
                    summary=summary,
                    plan_body=body,
                    plan_label=label,
                )
            )
            if index + 1 < len(slice_items):
                lines.append("")

    while lines and lines[-1] == "":
        lines.pop()
    return "\n".join(lines) + "\n"


def cmd_markdown(args: argparse.Namespace) -> None:
    if args.summary and args.slice:
        die("markdown: --summary and --slice are mutually exclusive")
    if (args.with_decisions or args.with_preamble) and not args.slice:
        die("markdown: --with-decisions / --with-preamble require --slice")
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    plan_bodies: dict[str, str] = {}
    plan_labels: dict[str, str] = {}
    # Disk read at the edge: render stays pure over task + optional plan bodies.
    if args.slice and isinstance(store, YamlTaskStore):
        layout = store.layout
        plan_path = layout.slice_plan_file(args.slice)
        if plan_path.is_file():
            plan_bodies[args.slice] = plan_path.read_text(encoding="utf-8")
            plan_labels[args.slice] = slice_plan_markdown_label(layout, args.slice)
    print(
        render_task_markdown(
            task,
            chronological=bool(args.chronological),
            summary=bool(args.summary),
            slice_key=args.slice,
            with_decisions=bool(args.with_decisions),
            with_preamble=bool(args.with_preamble),
            plan_bodies=plan_bodies,
            plan_labels=plan_labels,
        ),
        end="",
    )



def task_layers(task: Mapping[str, Any]) -> tuple[dict[str, Any], ...]:
    """Return task.layers entries as plain dict copies in registry order."""
    raw = task.get("layers") or []
    return tuple(dict(entry) for entry in raw if isinstance(entry, Mapping))


def layer_names(task: Mapping[str, Any]) -> tuple[str, ...]:
    return tuple(str(entry.get("name") or "") for entry in task_layers(task) if entry.get("name"))


def slice_requires_layer(kind: str) -> bool:
    return kind in LAYERED_SLICE_KINDS


def validate_slice_layer(task: Mapping[str, Any], kind: str, layer: str | None) -> str | None:
    """Return the layer to persist, or die when the binding is invalid."""
    names = layer_names(task)
    if not names:
        if layer:
            die(
                f"--layer {layer!r} not allowed when task.layers is empty; "
                "register layers first with `pi-job layers add`"
            )
        return None
    if kind in LAYERED_SLICE_KINDS:
        if not layer:
            die(
                f"--layer is required for kind {kind!r} when task.layers is set; "
                f"known layers: {', '.join(names)}"
            )
        if layer not in names:
            die(
                f"layer {layer!r} is not in task.layers; "
                f"known: {', '.join(names)}"
            )
        return layer
    if layer:
        die(
            f"--layer is not allowed for kind {kind!r}; "
            f"only {', '.join(sorted(LAYERED_SLICE_KINDS))} use layers"
        )
    return None


def task_bundle_root(store: YamlTaskStore) -> Path:
    layout = store.layout
    return layout.bundle_root if hasattr(layout, "bundle_root") else layout.task_path.parent


def resolve_task_relative_path(bundle_root: Path, path_text: str) -> Path:
    """Resolve a task-relative or absolute registered artifact path."""
    raw = path_text.strip()
    if not raw:
        die("artifact path must be non-empty")
    path = Path(raw)
    if path.is_absolute():
        return path.resolve()
    return (bundle_root / path).resolve()


def _iter_tree_files(root: Path) -> Iterator[Path]:
    if not root.is_dir():
        return
    for path in sorted(root.rglob("*")):
        if path.is_file():
            yield path.resolve()


@dataclass(frozen=True)
class TaskFileListing:
    """Resolved task artifact paths for grep-friendly CLI listing."""

    bundle_root: Path
    paths: tuple[Path, ...]

    @classmethod
    def from_store(cls, store: YamlTaskStore, task: Mapping[str, Any]) -> TaskFileListing:
        bundle_root = task_bundle_root(store).resolve()
        layout = store.layout
        seen: set[Path] = set()
        collected: list[Path] = []

        def add(path: Path) -> None:
            resolved = path.resolve()
            if resolved in seen:
                return
            seen.add(resolved)
            collected.append(resolved)

        if isinstance(layout, BundleTaskLayout):
            for path in _iter_tree_files(layout.references_dir):
                add(path)
            for path in _iter_tree_files(layout.plans_dir):
                add(path)
        else:
            for path in _iter_tree_files(layout.plans_dir):
                add(path)

        for artifact in task_artifacts(task).values():
            path_text = str(artifact.get("path") or "").strip()
            if path_text:
                add(resolve_task_relative_path(bundle_root, path_text))

        for item in task_maintain(task):
            uri = str(item.get("uri") or "").strip()
            if not uri or is_remote_uri(uri):
                continue
            path_text = uri.split("#", 1)[0]
            if path_text:
                add(resolve_task_relative_path(bundle_root, path_text))

        return cls(bundle_root=bundle_root, paths=tuple(sorted(collected)))

    def formatted_lines(self, *, relative: bool) -> tuple[str, ...]:
        lines: list[str] = []
        for path in self.paths:
            if relative:
                try:
                    lines.append(str(path.relative_to(self.bundle_root)))
                except ValueError:
                    lines.append(str(path))
            else:
                lines.append(str(path))
        return tuple(lines)


@dataclass(frozen=True)
class LayerSliceGroups:
    """Pure partition of slices against the layer registry (order preserved)."""

    by_layer: tuple[tuple[str, tuple[TaskSlice, ...]], ...]
    unlayered: tuple[TaskSlice, ...]
    unknown: tuple[TaskSlice, ...]


def group_slices_by_layer(
    slices: Sequence[TaskSlice],
    names: Sequence[str],
) -> LayerSliceGroups:
    """Partition slices into registry bands, unlayered kinds, and unknown bindings."""
    name_set = set(names)
    buckets: dict[str, list[TaskSlice]] = {name: [] for name in names}
    unlayered: list[TaskSlice] = []
    unknown: list[TaskSlice] = []
    for task_slice in slices:
        if not slice_requires_layer(task_slice.kind):
            unlayered.append(task_slice)
            continue
        if not task_slice.layer or task_slice.layer not in name_set:
            unknown.append(task_slice)
            continue
        buckets[task_slice.layer].append(task_slice)
    return LayerSliceGroups(
        by_layer=tuple((name, tuple(buckets[name])) for name in names),
        unlayered=tuple(unlayered),
        unknown=tuple(unknown),
    )


def format_layer_survival_report(task: Mapping[str, Any]) -> tuple[str, ...]:
    """How planned/done slices map to the current layer registry (pure)."""
    names = layer_names(task)
    if not names:
        unbound = tuple(
            s for s in task_slices(task)
            if slice_requires_layer(s.kind) and s.layer
        )
        lines = ["Layer survival report:", "  (no layers registered)"]
        if unbound:
            lines.append("  slices with layer but empty registry:")
            lines.extend(
                f"    - {s.key} [{s.status}] layer={s.layer}" for s in unbound
            )
        return tuple(lines)

    groups = group_slices_by_layer(task_slices(task), names)
    lines = ["Layer survival report:"]
    for name, members in groups.by_layer:
        lines.append(f"  {name}: {len(members)} slice(s)")
        lines.extend(f"    - {s.key} [{s.status}]" for s in members)
    if groups.unlayered:
        lines.append(f"  (unlayered kinds): {len(groups.unlayered)} slice(s)")
        lines.extend(
            f"    - {s.key} [{s.kind}/{s.status}]" for s in groups.unlayered
        )
    if groups.unknown:
        lines.append("  MISSING or unknown layer binding:")
        lines.extend(
            f"    - {s.key} [{s.kind}/{s.status}] layer={s.layer!r}"
            for s in groups.unknown
        )
    lines.append(
        "Amend references/bigpicture.txt to match the registry "
        "(cross-layer call stacktrace); harness does not rewrite an enriched file."
    )
    return tuple(lines)


def format_layer_list_lines(task: Mapping[str, Any]) -> tuple[str, ...]:
    """Human list of registered layers (pure)."""
    entries = task_layers(task)
    if not entries:
        return ("Layers: (none - N/A or not confirmed)",)
    lines = ["Layers (order = diagram bands):"]
    for index, entry in enumerate(entries, start=1):
        name = str(entry.get("name") or "")
        desc = str(entry.get("description") or "").strip()
        suffix = f" - {desc}" if desc else ""
        lines.append(f"  {index}. {name}{suffix}")
        lines.extend(f"      ref: {ref}" for ref in (entry.get("references") or []))
    return tuple(lines)


def format_bigpicture_layer_bands(task: Mapping[str, Any]) -> str:
    """Per-layer TODO blocks for the bigpicture stub (pure)."""
    bands: list[str] = []
    for entry in task_layers(task):
        name = str(entry.get("name") or "")
        desc = str(entry.get("description") or "").strip()
        bands.append(f"LAYER: {name}")
        if desc:
            bands.append(f"  # {desc}")
        bands.append("  # TODO: hops that enter or run inside this band")
        bands.append("  #   N. METHOD /path")
        bands.append("  #      (Caller / Service)")
        bands.append("  #   resulting state lines…")
        bands.append("")
    return "\n".join(bands)


def bigpicture_stub_text(task: Mapping[str, Any]) -> str:
    """Stub body for references/bigpicture.txt from profile + layer registry (pure)."""
    layer_entries = task_layers(task)
    names = [str(entry.get("name") or "") for entry in layer_entries if entry.get("name")]
    template = str(load_profile_contract()["instruction_packets"]["bigpicture_stub"])
    body = template.format(
        stub_marker=BIGPICTURE_STUB_MARKER,
        top_layer=names[0] if names else "<layer>",
        bottom_layer=names[-1] if names else "<downstream-layer>",
        layer_names=", ".join(names) or "<layer-a>, <layer-b>",
        layer_bands=format_bigpicture_layer_bands(task).rstrip(),
        bigpicture_path=BIGPICTURE_DEFAULT_RELPATH,
    )
    return body if body.endswith("\n") else body + "\n"


def ensure_bigpicture_stub(bundle_root: Path, task: Mapping[str, Any]) -> Path | None:
    """Write bigpicture stub once when layers exist and the file is missing (I/O edge)."""
    if not layer_names(task):
        return None
    path = bundle_root / BIGPICTURE_DEFAULT_RELPATH
    if path.exists():
        return None
    path.parent.mkdir(parents=True, exist_ok=True)
    atomic_write_text(path, bigpicture_stub_text(task))
    return path


def print_layer_survival_report(task: Mapping[str, Any]) -> None:
    print()
    for line in format_layer_survival_report(task):
        print(line)


def make_layer_entry(
    *,
    name: str,
    description: str,
    references: Sequence[str] | None = None,
) -> dict[str, Any]:
    return {
        "name": name,
        "description": description,
        "references": list(references or ()),
    }


def layers_with_added(
    layers: Sequence[Mapping[str, Any]],
    entry: Mapping[str, Any],
    *,
    after: str | None = None,
) -> list[dict[str, Any]]:
    """Return a new layers list with entry appended or inserted after `after`."""
    name = str(entry.get("name") or "")
    current = [dict(item) for item in layers]
    if any(str(item.get("name") or "") == name for item in current):
        die(f"layer already exists: {name!r}")
    if after is None:
        return [*current, dict(entry)]
    for index, item in enumerate(current):
        if str(item.get("name") or "") == after:
            return [*current[: index + 1], dict(entry), *current[index + 1 :]]
    die(f"--after layer not found: {after!r}")


def layers_with_updated(
    layers: Sequence[Mapping[str, Any]],
    *,
    name: str,
    description: str | None = None,
    references: Sequence[str] | None = None,
) -> list[dict[str, Any]]:
    """Return a new layers list with one entry's fields updated."""
    found = False
    updated: list[dict[str, Any]] = []
    for item in layers:
        copy = dict(item)
        if str(copy.get("name") or "") == name:
            found = True
            if description is not None:
                copy["description"] = description
            if references is not None:
                copy["references"] = list(references)
        updated.append(copy)
    if not found:
        die(f"layer not found: {name!r}")
    return updated


def slice_keys_bound_to_layer(task: Mapping[str, Any], name: str) -> tuple[str, ...]:
    return tuple(
        str(sl.get("key") or "")
        for sl in (task.get("plan") or {}).get("slices") or []
        if str(sl.get("layer") or "") == name
    )


def layers_without(
    layers: Sequence[Mapping[str, Any]],
    *,
    name: str,
) -> list[dict[str, Any]]:
    """Return layers with `name` removed; die when absent."""
    kept = [dict(item) for item in layers if str(item.get("name") or "") != name]
    if len(kept) == len(tuple(layers)):
        die(f"layer not found: {name!r}")
    return kept


def layers_with_renamed(
    layers: Sequence[Mapping[str, Any]],
    *,
    old: str,
    new: str,
) -> list[dict[str, Any]]:
    """Return layers with one name rewritten; die on collision or missing old."""
    current = tuple(dict(item) for item in layers)
    if any(str(item.get("name") or "") == new for item in current):
        die(f"layer already exists: {new!r}")
    if not any(str(item.get("name") or "") == old for item in current):
        die(f"layer not found: {old!r}")
    return [
        {**item, "name": new} if str(item.get("name") or "") == old else dict(item)
        for item in current
    ]


def slices_with_layer_renamed(
    slices: Sequence[Mapping[str, Any]],
    *,
    old: str,
    new: str,
) -> list[dict[str, Any]]:
    """Return slice dicts with layer bindings rewritten from old to new."""
    return [
        {**dict(sl), "layer": new} if str(sl.get("layer") or "") == old else dict(sl)
        for sl in slices
    ]


def layers_reordered(
    layers: Sequence[Mapping[str, Any]],
    names: Sequence[str],
) -> list[dict[str, Any]]:
    """Return layers in `names` order; die unless names are a permutation."""
    by_name = {str(item.get("name") or ""): dict(item) for item in layers}
    if set(names) != set(by_name):
        die(
            "reorder must list each layer name exactly once; "
            f"have={sorted(by_name)} got={list(names)}"
        )
    return [by_name[name] for name in names]


class SliceDependencyMermaid:
    """Mermaid flowchart export of slice `depends_on` for termaid/stdin viewers.

    Boundary: only this type formats Mermaid. Callers (e.g. `show --graph`) pass
    task state in and print `render()`; they do not assemble classDef/nodes/edges.
    Edges run dependency → dependent (delivery order, top-down). Unknown
    depends_on targets appear as orange `missing` nodes so broken refs stay visible.
    """

    # Green = delivered, blue = in progress (including the saved cursor slice).
    CLASS_DEFS: ClassVar[tuple[str, ...]] = (
        "classDef done fill:#22c55e,color:#ffffff,stroke:#16a34a",
        "classDef in_progress fill:#3b82f6,color:#ffffff,stroke:#2563eb",
        "classDef planned fill:#6b7280,color:#ffffff,stroke:#4b5563",
        "classDef blocked fill:#ef4444,color:#ffffff,stroke:#dc2626",
        "classDef skipped fill:#eab308,color:#000000,stroke:#ca8a04",
        "classDef missing fill:#f97316,color:#000000,stroke:#ea580c",
    )

    def __init__(
        self,
        *,
        claimed_slices: set[str] | None = None,
        status_filter: set[str] | None = None,
        by_layer: bool = False,
    ) -> None:
        self._claimed_slices = claimed_slices or set()
        self._status_filter = status_filter
        self._by_layer = by_layer
        self._node_ids: dict[str, str] = {}
        self._used_ids: set[str] = set()

    @staticmethod
    def node_id(key: str) -> str:
        """Stable Mermaid node id from a slice key (hyphens/punctuation → underscores)."""
        cleaned = re.sub(r"[^A-Za-z0-9_]", "_", key)
        if not cleaned or not cleaned[0].isalpha():
            cleaned = f"n_{cleaned}"
        return cleaned

    @staticmethod
    def node_label(text: str) -> str:
        """Escape a slice key for use inside a Mermaid quoted node label."""
        return text.replace("\\", "\\\\").replace('"', "#quot;")

    def style_status(self, task_slice: TaskSlice) -> str:
        """Status class for graph coloring; a claimed slice paints as in_progress like show glyphs."""
        if task_slice.key in self._claimed_slices and task_slice.status not in STATUS_DONE:
            return "in_progress"
        if task_slice.status in {"done", "skipped", "in_progress", "blocked", "planned"}:
            return task_slice.status
        return "planned"

    def _allocate_id(self, key: str) -> str:
        if key in self._node_ids:
            return self._node_ids[key]
        base = self.node_id(key)
        candidate = base
        n = 2
        while candidate in self._used_ids:
            candidate = f"{base}_{n}"
            n += 1
        self._used_ids.add(candidate)
        self._node_ids[key] = candidate
        return candidate

    def render(self, task: Mapping[str, Any]) -> str:
        """Return a Mermaid flowchart document (trailing newline) for this task."""
        slices = list(task_slices(task))
        if self._status_filter is not None:
            slices = [s for s in slices if s.status in self._status_filter]
        known_keys = {s.key for s in slices}
        # Prefer edges among visible nodes only; unknown keys (never in plan) still
        # get a missing node. Deps filtered out of this view (still in plan) skip the edge.
        all_plan_keys = {s.key for s in task_slices(task)}

        lines: list[str] = ["flowchart TD", *self.CLASS_DEFS]
        names = layer_names(task)
        if self._by_layer and names:
            groups = group_slices_by_layer(slices, names)
            # Unlayered kinds and unbound work slices float above bands.
            for task_slice in (*groups.unlayered, *groups.unknown):
                lines.append(self._node_line(task_slice, indent="  "))
            for layer_name, members in groups.by_layer:
                if not members:
                    continue
                sg = self._allocate_id(f"layer_{layer_name}")
                sg_label = self.node_label(layer_name)
                lines.append(f'  subgraph {sg}["{sg_label}"]')
                lines.extend(self._node_line(s, indent="    ") for s in members)
                lines.append("  end")
        else:
            lines.extend(self._node_line(s, indent="  ") for s in slices)

        emitted_missing: set[str] = set()
        for task_slice in slices:
            dependent_id = self._allocate_id(task_slice.key)
            for dep_key in task_slice.depends_on:
                if dep_key in known_keys:
                    lines.append(f"  {self._allocate_id(dep_key)} --> {dependent_id}")
                    continue
                if dep_key in all_plan_keys:
                    continue
                if dep_key not in emitted_missing:
                    mid = self._allocate_id(dep_key)
                    label = self.node_label(dep_key)
                    lines.append(f'  {mid}["{label}"]:::missing')
                    emitted_missing.add(dep_key)
                lines.append(f"  {self._allocate_id(dep_key)} --> {dependent_id}")

        return "\n".join(lines) + "\n"

    def _node_line(self, task_slice: TaskSlice, *, indent: str) -> str:
        nid = self._allocate_id(task_slice.key)
        label = self.node_label(task_slice.key)
        style = self.style_status(task_slice)
        return f'{indent}{nid}["{label}"]:::{style}'


def cmd_show(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    require_initialized(task_file, task)
    claims = owned_cursors(task)
    positions = {claim.slice: claim_position(task, claim) for claim in claims}
    claimed_slices = set(positions)
    color = color_enabled(args.color)

    if args.graph and args.slice:
        die("show --graph is whole-task; omit --slice (use --status to filter nodes)")

    if args.graph:
        status_filter = set(args.status.split(",")) if args.status else None
        print(
            SliceDependencyMermaid(
                claimed_slices=claimed_slices,
                status_filter=status_filter,
                by_layer=bool(getattr(args, "by_layer", False)),
            ).render(task),
            end="",
        )
        return

    if args.slice:
        task_slice = find_slice(task, args.slice)
        if task_slice is None:
            die(f"slice not found: {args.slice!r}; known slice keys: {known_slice_keys(task)}")
        status_map = slice_status_map(task)
        lines = render_slice_scoped(
            task,
            task_slice,
            positions=positions,
            status_map=status_map,
            color=color,
            include_model=True,
        )
        print("\n".join(lines))
        return

    status_filter = set(args.status.split(",")) if args.status else None
    include_model = bool(args.full)

    lines = [f"{task.get('title', '<untitled>')}"]
    if claims:
        for claim in claims:
            lines.append(f"claim → {claim_label(task, claim)}")
    else:
        lines.append("claim → <none>")
    lines.append("")

    slices = ordered_slices_for_show(task, claimed_slices=claimed_slices, work_first=bool(args.work_first))
    status_map = slice_status_map(task)
    visible_slices = [
        s for s in slices
        if not status_filter or s.status in status_filter
    ]
    key_width = max((len(task_slice.key) for task_slice in visible_slices), default=0)
    short = bool(args.short) and not args.all
    done_slices = done_steps = total_steps = 0
    for task_slice in slices:
        st = task_slice.status
        d, t = slice_counts(task_slice)
        done_steps += d
        total_steps += t
        if st in STATUS_DONE:
            done_slices += 1

    # With --short, collapse consecutive status==done slices onto one "✓ a, b, c" line.
    # Skipped and non-done slices still render one-per-line. --all disables collapsing.
    i = 0
    while i < len(slices):
        task_slice = slices[i]
        st = task_slice.status
        if status_filter and st not in status_filter:
            i += 1
            continue

        if short and st == "done" and task_slice.key not in claimed_slices:
            run_keys: list[str] = []
            while (
                i < len(slices)
                and slices[i].status == "done"
                and slices[i].key not in claimed_slices
            ):
                if not status_filter or slices[i].status in status_filter:
                    run_keys.append(slices[i].key)
                i += 1
            if run_keys:
                glyph = paint_glyph(GLYPH["done"], "done", color=color)
                lines.append(f"{glyph} {', '.join(run_keys)}")
            continue

        d, t = slice_counts(task_slice)
        is_current = task_slice.key in claimed_slices
        glyph_status = "in_progress" if is_current else st
        glyph = paint_glyph(GLYPH.get(glyph_status, "○"), glyph_status, color=color)
        repos = ", ".join(task_slice.repos)
        # Done/skipped: omit [kind/n/m] - progress is already in the footer.
        kind_counts = "" if st in STATUS_DONE else f" [{task_slice.kind}/{d}/{t}]"
        execution_suffix = (
            f" [{task_slice.execution.model}]"
            if include_model and task_slice.execution and task_slice.execution.model
            else ""
        )
        header = f"{glyph} {task_slice.key:<{key_width}}{kind_counts}"
        if repos:
            header += f" {repos}"
        header += execution_suffix
        lines.append(header.rstrip())

        # Done/skipped slices stay header-only unless --all (no deps, steps, or unset
        # repo_work). With --status, still print recorded worktrees so agents can list
        # them without expanding the whole task via --all.
        if st in STATUS_DONE and not args.all:
            if status_filter is not None:
                append_set_worktree_lines(lines, task_slice)
            i += 1
            continue

        append_slice_work_lines(lines, task_slice, status_map)

        # Expand steps for the cursor slice, --all, or --started (active work only).
        expand_steps = (
            args.all
            or is_current
            or (args.started and st not in STATUS_DONE and st != "planned")
        )
        if expand_steps:
            current_step = positions[task_slice.key].step if is_current else None
            for step in task_slice.steps:
                lines.append(
                    step_line(
                        step,
                        terminal=False,
                        current_step=current_step,
                        color=color,
                        include_model=include_model,
                    )
                )
            for step in task_slice.final_steps:
                lines.append(
                    step_line(
                        step,
                        terminal=True,
                        current_step=current_step,
                        color=color,
                        include_model=include_model,
                    )
                )
        i += 1

    lines += ["", f"{done_slices}/{len(slices)} slices · {done_steps}/{total_steps} steps", "— toolbelt —"]
    artifacts = task_artifacts(task)
    if not artifacts:
        lines.append("no aids registered")
    else:
        for key, art in artifacts.items():
            art_st = str(art.get("status") or "")
            g = paint_glyph(GLYPH.get(art_st, "○"), art_st, color=color)
            lines.append(f"{g} {key}   {art.get('path', '')}".rstrip())
    items = task_maintain(task)
    lines += ["", "— keep current —"]
    if not items:
        lines.append("none registered")
    else:
        for item in items:
            uri = str(item.get("uri") or "")
            note = str(item.get("note") or "").strip()
            suffix = f"  {note}" if note else ""
            lines.append(f"○ {uri}{suffix}")
    print("\n".join(lines))


def validated_layer_binds(
    task: Mapping[str, Any],
    *,
    new_layer: str,
    raw_binds: list[str],
) -> list[tuple[str, str]]:
    """Parse and validate atomic bindings for a layer registry addition."""
    binds: list[tuple[str, str]] = []
    seen: dict[str, str] = {}
    for raw in raw_binds:
        if raw.count("=") != 1:
            die("--bind must use exactly one SLICE=LAYER pair")
        slice_key, layer = raw.split("=")
        if not slice_key or not layer:
            die("--bind requires non-empty SLICE and LAYER values")
        if slice_key in seen:
            if seen[slice_key] == layer:
                die(f"duplicate --bind for slice {slice_key!r}")
            die(
                f"conflicting --bind values for slice {slice_key!r}: "
                f"{seen[slice_key]!r} and {layer!r}"
            )
        seen[slice_key] = layer
        binds.append((slice_key, layer))

    slices = task.get("plan", {}).get("slices", [])
    by_key = {
        str(task_slice.get("key")): task_slice
        for task_slice in slices
        if isinstance(task_slice, Mapping)
    }
    known_layers = {*layer_names(task), new_layer}
    for slice_key, layer in binds:
        task_slice = by_key.get(slice_key)
        if task_slice is None:
            die(f"--bind slice not found: {slice_key!r}")
        kind = str(task_slice.get("kind") or "")
        if kind not in LAYERED_SLICE_KINDS:
            die(
                f"--bind is not allowed for kind {kind!r}; "
                f"only {', '.join(sorted(LAYERED_SLICE_KINDS))} use layers"
            )
        existing_layer = task_slice.get("layer")
        if existing_layer:
            die(
                f"slice {slice_key!r} already has layer {existing_layer!r}; "
                "rebind with `set-slice --layer`"
            )
        if layer not in known_layers:
            die(
                f"--bind layer {layer!r} is not registered or being added; "
                f"known: {', '.join(sorted(known_layers))}"
            )

    bound_keys = set(seen)
    unbound = [
        str(task_slice.get("key"))
        for task_slice in slices
        if isinstance(task_slice, Mapping)
        and str(task_slice.get("kind") or "") in LAYERED_SLICE_KINDS
        and not task_slice.get("layer")
        and str(task_slice.get("key")) not in bound_keys
    ]
    if unbound:
        die(
            "layers add requires bindings for all unlayered implement/spike/research "
            f"slices: {', '.join(unbound)}"
        )
    return binds


def cmd_layers(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="layers")
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("layers requires a YAML task file")
    task = store.read()
    require_initialized(task_file, task)
    action = args.layers_action

    if action == "show":
        updated = store.read()
        for line in format_layer_list_lines(updated):
            print(line)
        print_layer_survival_report(updated)
        return

    if action == "add":
        refs = split_csv(getattr(args, "references", None))
        binds = validated_layer_binds(
            task,
            new_layer=args.name,
            raw_binds=list(getattr(args, "bind", []) or []),
        )
        store.add_layer(
            name=args.name,
            description=args.description,
            references=refs or None,
            after=getattr(args, "after", None),
            binds=binds,
        )
        print(f"added layer: {args.name}")
    elif action == "set":
        if args.description is None and args.references is None:
            die("layers set requires at least one of --description or --references")
        refs = split_csv(args.references) if args.references is not None else None
        store.set_layer(
            name=args.name,
            description=args.description,
            references=refs,
        )
        print(f"updated layer: {args.name}")
    elif action == "remove":
        store.remove_layer(name=args.name)
        print(f"removed layer: {args.name}")
    elif action == "rename":
        store.rename_layer(old=args.old, new=args.new)
        print(f"renamed layer: {args.old} -> {args.new}")
    elif action == "reorder":
        names = split_csv(args.order)
        if not names:
            die("layers reorder requires --order with comma-separated layer names")
        store.reorder_layers(names=names)
        print(f"reordered layers: {', '.join(names)}")
    else:
        die(f"unknown layers action: {action!r}")

    updated = store.read()
    if action == "add":
        stub_path = ensure_bigpicture_stub(task_bundle_root(store), updated)
        if stub_path is not None:
            root = task_bundle_root(store)
            try:
                rel = stub_path.relative_to(root)
                print(f"created bigpicture stub: {rel}")
            except ValueError:
                print(f"created bigpicture stub: {stub_path}")
    print_layer_survival_report(updated)


def cmd_files(args: argparse.Namespace) -> None:
    store = open_task_store(args.task, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("files requires a YAML task file or bundle store")
    task = store.read()
    listing = TaskFileListing.from_store(store, task)
    for line in listing.formatted_lines(relative=args.relative):
        print(line)


def cmd_toolbelt(args: argparse.Namespace) -> None:
    store = open_task_store(args.task, args.layout)
    task = store.read()
    task_file = args.task
    require_initialized(task_file, task)
    if args.action == "add":
        toolbelt_add(store, task, args)
        return
    if args.kind:
        get_slice_kind(args.kind)
        kinds = {args.kind}
    else:
        kinds = task_slice_kinds(task)
    aids = toolbelt_for_kinds(kinds)
    kind_desc = args.kind if args.kind else ", ".join(sorted(kinds)) or "(none)"
    print(f"Toolbelt aids suited to slice kind(s) {kind_desc}:")
    if not aids:
        print("  (none — these slice kinds suggest no planning aids)")
        return
    artifacts = task_artifacts(task)
    for aid in aids:
        key = str(aid.get("key") or "")
        registered = artifacts.get(key)
        status = str(registered.get("status")) if registered else "not registered"
        print(f"  {key} [{status}] — {aid.get('title')}: {aid.get('purpose')}")


def cmd_maintain(args: argparse.Namespace) -> None:
    store = open_task_store(args.task, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("maintain requires a YAML task file or bundle store")
    task = store.read()
    require_initialized(args.task, task)
    action = args.action
    if action == "add":
        result = store.upsert_maintain(uri=args.uri or "", note=args.note or "")
        print(f"{result} maintain item: {args.uri.strip()}")
        return
    if action == "remove":
        store.remove_maintain(uri=args.uri or "")
        print(f"removed maintain item: {args.uri.strip()}")
        return
    for line in maintain_block(task):
        if line:
            print(line)


def _default_coding_policy() -> dict[str, Any]:
    return {
        "subagent_required": True,
        "lower_power_model_preferred": True,
        "orchestrator_reviews_subagent": True,
        "exceptions": [],
    }


def _fresh_orchestration() -> dict[str, Any]:
    """No silent claim on create: cursors starts empty; an owner must `claim` explicitly."""
    return {
        "cursors": [],
        "policy": {"coding_execution": _default_coding_policy()},
        "artifacts": {},
    }


def _scaffold_task_mapping(
    *,
    title: str,
    kind: str | None,
    empty_plan: bool,
    goal: str,
) -> dict[str, Any]:
    if empty_plan and kind:
        die("--empty-plan and --kind are mutually exclusive")
    if empty_plan:
        return {
            "title": title,
            "status": "planned",
            "source": {"jira": "", "discovered": "", "context": "Short discovery note for why this task exists."},
            "project": {"key": "", "name": "", "route": "", "context": ""},
            "context": "",
            "decisions": [],
            "plan": {"note": "Add slices with pi-job add-slice, then run create to initialize.", "slices": []},
        }
    if kind:
        get_slice_kind(kind)
        slice_key = f"{kind}-slice"
        kind_title = get_slice_kind(kind).get("title", kind)
        return {
            "title": title,
            "status": "planned",
            "source": {"jira": "", "discovered": "", "context": "Short discovery note for why this task exists."},
            "project": {"key": "", "name": "", "route": "", "context": ""},
            "context": "",
            "decisions": [],
            "plan": {
                "note": "Replace this plan with real slices using pi-job commands.",
                "slices": [{
                    "key": slice_key,
                    "kind": kind,
                    "title": kind_title,
                    "goal": goal,
                    "status": "planned",
                    "note": "",
                    "steps": [
                        {"key": key, "title": step_title, "status": "planned", "note": ""}
                        for key, step_title in steps_from_kind_template(kind)
                    ],
                    "final_steps": [],
                }],
            },
        }
    mapping = example_task_mapping(title=title)
    mapping["project"] = {"key": "", "name": "", "route": "", "context": ""}
    mapping["plan"]["slices"][0]["goal"] = goal
    mapping["status"] = "planned"
    mapping["plan"]["slices"][0]["status"] = "planned"
    return mapping


def _seed_kind_if_empty(store: TaskStore, task: dict[str, Any], kind: str | None, goal: str) -> dict[str, Any]:
    if not kind:
        return task
    get_slice_kind(kind)
    if task.get("plan", {}).get("slices"):
        return task
    store.add_slice(
        key=f"{kind}-slice",
        kind=kind,
        title=get_slice_kind(kind).get("title", kind),
        goal=goal,
        extra_fields={},
        steps=steps_from_kind_template(kind),
        final_steps=[],
        after=None,
    )
    return store.read()


def _suggest_first_claim(task: dict[str, Any]) -> str:
    """Human-readable next step after create; no claim is seeded automatically."""
    cursor = seed_cursor(task)
    if cursor is not None:
        return f"pi-job claim --slice {cursor.slice} --owner ID"
    if all_slices_done(task):
        return "(task has no actionable slice/step; add slices with pi-job add-slice --kind ...)"
    return (
        "(no slice is dependency-satisfied yet; check depends_on for a forward reference "
        "or cycle among the first slices)"
    )


def _create_from_intent(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="create")
    if task_file.suffix.lower() not in {".yaml", ".yml"}:
        unsupported_storage(task_file)
    input_path = args.from_path
    if input_path.suffix.lower() == ".cue":
        unsupported_storage(input_path)
    if not input_path.exists():
        die(f"create intent not found: {input_path}")
    if args.kind or args.empty_plan or args.title is not None:
        die("create --from cannot be combined with --kind, --empty-plan, or --title")
    raw = load_yaml_mapping(input_path, label="create intent")
    try:
        bootstrap = BootstrapDocument.model_validate(raw)
    except ValidationError as exc:
        die(f"create intent validation failed:\n{exc}")
    kind = bootstrap.initial_slice_kind
    if kind:
        get_slice_kind(kind)
        initial_goal = validate_real_goal(str(bootstrap.goal or ""), label="goal")
    else:
        initial_goal = None
    assembled_slices: list[dict[str, Any]] = []
    if kind:
        seed_key = bootstrap.initial_slice_key or f"task-{kind}"
        kind_title = get_slice_kind(kind).get("title", kind)
        assembled_slices.append({
            "key": seed_key,
            "kind": kind,
            "title": kind_title,
            "goal": initial_goal,
            "status": "planned",
            "note": "",
            "repos": [],
            "depends_on": [],
            "steps": [
                {"key": key, "title": step_title, "status": "planned", "note": ""}
                for key, step_title in steps_from_kind_template(kind)
            ],
            "final_steps": [],
        })
    for sl in bootstrap.slices:
        get_slice_kind(sl.kind)
        assembled_slices.append({
            "key": sl.key,
            "kind": sl.kind,
            "title": sl.title,
            "goal": sl.goal,
            "status": "planned",
            "note": "",
            "repos": list(sl.repos),
            "depends_on": list(sl.depends_on),
            "steps": [
                {"key": key, "title": step_title, "status": "planned", "note": ""}
                for key, step_title in steps_from_kind_template(sl.kind)
            ],
            "final_steps": [],
        })
    prospective = {
        "title": bootstrap.title,
        "status": bootstrap.status,
        "source": {
            "jira": bootstrap.source.jira or "",
            "discovered": bootstrap.source.discovered or "",
            "context": bootstrap.source.context or "",
        },
        "project": {
            "key": bootstrap.project.key or "",
            "name": bootstrap.project.name or "",
            "route": bootstrap.project.route or "",
            "context": bootstrap.project.context or "",
        },
        "context": bootstrap.context or "",
        "decisions": [
            {"date": d.date, "note": d.note, "source": d.source}
            for d in bootstrap.decisions
        ],
        "plan": {
            "note": bootstrap.plan_note or "",
            "slices": assembled_slices,
        },
    }
    slice_keys = {sl["key"] for sl in assembled_slices}
    missing_deps: list[str] = []
    for sl in assembled_slices:
        for dep in sl.get("depends_on", []):
            if dep not in slice_keys:
                missing_deps.append(f"  {sl['key']!r} depends on unknown slice key {dep!r}")
    if missing_deps:
        die("create: unresolved dependency references:\n" + "\n".join(missing_deps))
    _validate_task_project_route(prospective)
    validate_task_mapping(prospective, source=str(input_path))
    prospective["orchestration"] = _fresh_orchestration()
    full_task = canonical_task_mapping(prospective, source=str(input_path))
    new_yaml = render_yaml_task(full_task, source=str(input_path))
    if args.dry_run:
        existing = task_file.read_text() if task_file.exists() else ""
        diff = difflib.unified_diff(
            existing.splitlines(keepends=True),
            new_yaml.splitlines(keepends=True),
            fromfile=str(task_file) if task_file.exists() else "/dev/null",
            tofile=str(task_file),
        )
        sys.stdout.writelines(diff)
        return
    if task_file.exists() and not args.force:
        die(f"destination YAML task already exists: {task_file}\npass --force to overwrite, or choose another --task path")
    scaffold_bundle_dirs(task_file.parent)
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("create --from requires a YAML task file")
    with store.exclusive():
        store.replace(full_task)
    canonical_repr = store.read()
    if canonical_repr != full_task:
        die("create: written task failed semantic equality verification; possible concurrent modification")
    load_profile_contract()
    print(f"created: {task_file}")
    print(f"profile: {PROFILE}")
    print(f"schema: Pydantic {TaskDocument.__name__}")
    print("claim → <none>")
    print(f"next: {_suggest_first_claim(canonical_repr)}")
    print()
    print(build_plan(store, canonical_repr, args.layout))
    seed_block = build_seed_slice_plans_block(store.layout, canonical_repr, assembled_slices)
    if seed_block:
        print()
        print(seed_block, end="")
    decisions_count = len(canonical_repr.get("decisions") or [])
    print_decisions_after_slice_add(
        canonical_repr,
        prior_slice_count=0,
        decisions_count=decisions_count,
    )


def _init_existing_task(args: argparse.Namespace, task_file: Path) -> None:
    """Initialize orchestration on an existing task that has no orchestration yet."""
    store = open_task_store(task_file, args.layout)
    task = store.read()
    if task.get("orchestration"):
        die(
            f"task already exists and is initialized: {task_file}\n"
            "pass --force to recreate, or choose another --task path"
        )
    if args.dry_run:
        task = dict(task)
        # preview seed without writing
        kind = args.kind
        if kind and not task.get("plan", {}).get("slices"):
            seed_goal = _require_create_goal(args)
            get_slice_kind(kind)
            task.setdefault("plan", {})["slices"] = [{
                "key": f"{kind}-slice",
                "kind": kind,
                "title": get_slice_kind(kind).get("title", kind),
                "goal": seed_goal,
                "status": "planned",
                "note": "",
                "steps": [
                    {"key": key, "title": step_title, "status": "planned", "note": ""}
                    for key, step_title in steps_from_kind_template(kind)
                ],
                "final_steps": [],
            }]
        preview = {"orchestration": _fresh_orchestration()}
        print(yaml.safe_dump(preview, allow_unicode=True, sort_keys=False), end="")
        return
    scaffold_bundle_dirs(task_file.parent)
    seed_goal = _require_create_goal(args) if args.kind else ""
    prior_slice_count = len(task.get("plan", {}).get("slices") or [])
    task = _seed_kind_if_empty(store, task, args.kind, seed_goal)
    store.init_orchestration()
    task = store.read()
    print(f"initialized: {task_file}")
    print("claim → <none>")
    print(f"next: {_suggest_first_claim(task)}")
    print()
    print(build_plan(store, task, args.layout))
    if args.kind and prior_slice_count == 0:
        print_decisions_after_slice_add(
            task,
            prior_slice_count=0,
            decisions_count=len(task.get("decisions") or []),
        )


def cmd_create(args: argparse.Namespace) -> None:
    """Create and initialize a YAML task file."""
    if args.from_path is not None:
        _create_from_intent(args)
        return

    task_file = require_task(args.task, cmd="create")
    suffix = task_file.suffix.lower()

    if task_file.exists() and not args.force:
        if args.kind:
            _require_create_goal(args)
        _init_existing_task(args, task_file)
        return

    if suffix not in {".yaml", ".yml"}:
        unsupported_storage(task_file)

    seeding_slices = bool(args.kind) or not args.empty_plan
    seed_goal = _require_create_goal(args) if seeding_slices else ""
    mapping = _scaffold_task_mapping(
        title=args.title or "Example bounded change",
        kind=args.kind,
        empty_plan=bool(args.empty_plan),
        goal=seed_goal,
    )
    if not args.empty_plan:
        _validate_task_project_route(mapping)
    if args.empty_plan:
        content = render_yaml_task(mapping, source=str(task_file))
        if args.dry_run:
            print(content, end="" if content.endswith("\n") else "\n")
            return
        scaffold_bundle_dirs(task_file.parent)
        atomic_write_text(task_file, content)
        print(f"created empty plan: {task_file}")
        print("next: add slices with pi-job add-slice, then run:")
        print(f"  pi-job --task {task_file} create")
        return

    mapping["orchestration"] = _fresh_orchestration()
    full_task = canonical_task_mapping(mapping, source=str(task_file))
    content = render_yaml_task(full_task, source=str(task_file))
    if args.dry_run:
        print(content, end="" if content.endswith("\n") else "\n")
        return

    scaffold_bundle_dirs(task_file.parent)
    atomic_write_text(task_file, content)
    store = open_task_store(task_file, args.layout)
    task = store.read()
    print(f"created: {task_file}")
    print("claim → <none>")
    print(f"next: {_suggest_first_claim(task)}")
    print()
    print(build_plan(store, task, args.layout))
    seed_block = build_seed_slice_plans_block(
        store.layout, task, [dict(s) for s in (task.get("plan") or {}).get("slices") or []]
    )
    if seed_block:
        print()
        print(seed_block, end="")
    decisions_count = len(task.get("decisions") or [])
    print_decisions_after_slice_add(
        task,
        prior_slice_count=0,
        decisions_count=decisions_count,
    )


BASELINE_SLICE_FIELDS = {"key", "kind", "title", "goal", "status", "note", "steps", "final_steps"}
CLI_FILLABLE_SLICE_FIELDS = {"repos": lambda args: split_csv(args.repos), "depends_on": lambda args: split_csv(args.depends_on)}


def cmd_add_slice(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()

    if not args.kind:
        die(f"--kind is required; expected one of: {', '.join(sorted(valid_slice_kinds()))}")
    get_slice_kind(args.kind)

    status_map = slice_status_map(task)
    if args.key in status_map:
        die(f"slice key already exists: {args.key!r}")
    if args.after and args.after not in status_map:
        die(f"--after slice not found: {args.after!r}; known: {', '.join(status_map)}")

    layer_value = validate_slice_layer(task, args.kind, getattr(args, "layer", None))

    extra_fields: dict[str, list[str]] = {}
    for field_name, getter in CLI_FILLABLE_SLICE_FIELDS.items():
        values = getter(args)
        if values:
            extra_fields[field_name] = values
    final_steps: list[tuple[str, str]] = []
    final_step_keys = {key for key, _ in final_steps}
    steps = [
        (key, title)
        for key, title in steps_from_kind_template(args.kind)
        if key not in final_step_keys
    ]

    if args.dry_run:
        preview = {
            "key": args.key, "kind": args.kind, "title": args.title, "goal": args.goal,
            "status": "planned", "note": "", **extra_fields,
            "steps": [{"key": key, "title": title, "status": "planned", "note": ""} for key, title in steps],
            "final_steps": [],
        }
        if layer_value is not None:
            preview["layer"] = layer_value
        print(yaml.safe_dump(preview, allow_unicode=True, sort_keys=False), end="")
        return

    prior_slice_count = len(task.get("plan", {}).get("slices") or [])
    store.add_slice(
        key=args.key, kind=args.kind, title=args.title, goal=args.goal,
        extra_fields=extra_fields, steps=steps, final_steps=final_steps, after=args.after,
        layer=layer_value,
    )
    print(f"added slice: {args.key}")
    stub: Path | None = None
    if isinstance(store, YamlTaskStore):
        stub = store.ensure_slice_plan_stub(
            key=args.key,
            kind=args.kind,
            goal=args.goal,
            depends_on=list(extra_fields.get("depends_on") or []),
        )
    if stub is not None:
        print(f"created plan stub: {task_file.stem}.plans/{args.key}.md")
    updated = store.read()
    new_slice = next(
        (sl for sl in updated.get("plan", {}).get("slices", []) if sl.get("key") == args.key),
        None,
    )
    if new_slice is not None:
        layout = store.layout if isinstance(store, YamlTaskStore) else layout_for_document_path(task_file)
        seed_block = build_seed_slice_plans_block(layout, updated, [new_slice])
        if seed_block:
            print()
            print(seed_block, end="")
    decisions_count = len(updated.get("decisions") or [])
    print_decisions_after_slice_add(
        task,
        prior_slice_count=prior_slice_count,
        decisions_count=decisions_count,
    )


BASELINE_STEP_FIELDS = {"key", "title", "status", "note"}


def cmd_add_step(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()

    slices = task.get("plan", {}).get("slices", [])
    task_slice = next((s for s in slices if s.get("key") == args.slice), None)
    if task_slice is None:
        known = ", ".join(str(s.get("key") or "") for s in slices)
        die(f"slice not found: {args.slice!r}; known slice keys: {known}")

    existing = {str(s.get("key") or "") for s in task_slice.get("steps", [])} | {
        str(s.get("key") or "") for s in task_slice.get("final_steps", [])
    }
    if args.key in existing:
        die(f"step key already exists in slice {args.slice!r}: {args.key!r}")

    group = "final_steps" if args.final else "steps"
    group_keys = [str(s.get("key") or "") for s in task_slice.get(group, [])]
    if args.after and args.after not in group_keys:
        die(f"--after step not found in {args.slice!r}.{group}: {args.after!r}; known: {', '.join(group_keys) or '(none)'}")

    if args.dry_run:
        print(yaml.safe_dump({"key": args.key, "title": args.title, "status": "planned", "note": args.note or ""}, sort_keys=False), end="")
        return

    store.add_step(slice_key=args.slice, key=args.key, title=args.title, note=args.note or "", terminal=args.final, after=args.after)
    print(f"added step: {args.slice}/{args.key} ({group})")


def cmd_list(args: argparse.Namespace) -> None:
    """List `$PI_JOB_TASKS` bundles (slug, title, status, active claims).

    Scans only immediate child bundle directories of the task home; loose `*.yaml` files
    there are never listed (out of scope; use `project` to bundle them first). An
    individual bundle that fails to load (bad YAML, failed validation) is skipped with a
    stderr warning rather than aborting the whole listing.
    Ready frontier stays on `pi-job status` / `show`, not on this overview."""
    home = task_tasks_home(args.layout)
    entries: list[TaskListEntry] = []
    for slug, doc in iter_home_bundle_docs(home):
        try:
            task = open_task_store(doc, args.layout).read()
            entries.append(
                TaskListEntry(
                    slug=slug,
                    title=str(task.get("title") or ""),
                    status=derived_task_status(task),
                    updated=task_list_updated(task, doc),
                    cursor_labels=tuple(
                        claim_label(task, claim) for claim in owned_cursors(task)
                    ),
                )
            )
        except SystemExit:
            print(f"warning: skipping unreadable bundle {slug!r} ({doc})", file=sys.stderr)
            continue
        except (OSError, yaml.YAMLError, ValueError) as exc:
            print(f"warning: skipping unreadable bundle {slug!r} ({doc}): {exc}", file=sys.stderr)
            continue
    if entries:
        ordered = sorted(entries, key=task_list_sort_key)
        print("\n\n".join(format_task_list_entry(entry) for entry in ordered))


def cmd_archive(args: argparse.Namespace) -> None:
    """Move a home task bundle into the archive home so `list` / slug create stay clear.

    Only immediate children of `$PI_JOB_TASKS` archive. Loose YAML and path-opened
    bundles outside the home are refused. Destination defaults to the same slug under
    `$PI_JOB_ARCHIVE` (or `<tasks-home-parent>/archive`); `--to` renames on move.
    """
    task_file = require_task(args.task, cmd="archive")
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore) or not isinstance(store.layout, BundleTaskLayout):
        die("archive requires a task bundle under $PI_JOB_TASKS, not a loose YAML file")
    slug = bundle_slug_under_home(store.layout, args.layout)
    if slug is None:
        die(
            "archive requires a bundle that is an immediate child of the task home "
            f"({task_tasks_home(args.layout)}); got {store.describe()}"
        )
    dest_slug = args.to or slug
    archive_home = task_archive_home(args.layout)
    if args.dry_run:
        dest = archive_home_bundle(
            bundle_root=store.layout.bundle_root,
            archive_home=archive_home,
            dest_slug=dest_slug,
            dry_run=True,
        )
        print(f"would archive {slug} -> {dest}")
        return
    with store.exclusive():
        dest = archive_home_bundle(
            bundle_root=store.layout.bundle_root,
            archive_home=archive_home,
            dest_slug=dest_slug,
            dry_run=False,
        )
    print(f"archived {slug} -> {dest}")


def cmd_set_worktree(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    slices = task.get("plan", {}).get("slices", [])
    slice_dict = next((s for s in slices if s.get("key") == args.slice), None)
    if slice_dict is None:
        known = ", ".join(str(s.get("key") or "") for s in slices)
        die(f"slice not found: {args.slice!r}; known slice keys: {known}")

    if args.clear:
        repo_work = slice_dict.get("repo_work") or {}
        if args.repo not in repo_work:
            die(f"repo work not found: {args.slice}/{args.repo}")
        if args.dry_run:
            print(f"would clear worktree: {args.slice}/{args.repo}")
            return
        store.clear_worktree(slice_key=args.slice, repo=args.repo)
        print(f"cleared worktree: {args.slice}/{args.repo}")
        return

    if args.path is None:
        path, note = recommend_worktree_path(
            store=store,
            task_path=task_file,
            slice_key=args.slice,
            repo=args.repo,
            layout=args.layout,
        )
        print(f"recommended worktree path: {path}")
        if note:
            print(f"note: {note}")
        die("set-worktree requires --path to record (or --clear to remove); pass --path to persist the recommendation above")

    if args.dry_run:
        print(yaml.safe_dump({"worktree": args.path}), end="")
        return

    store.set_worktree(slice_key=args.slice, repo=args.repo, path=args.path)
    print(f"set worktree: {args.slice}/{args.repo} -> {args.path}")


def cmd_add_pr(args: argparse.Namespace) -> None:
    task_file = args.task
    store = open_task_store(task_file, args.layout)
    task = store.read()
    slices = task.get("plan", {}).get("slices", [])
    if not any(s.get("key") == args.slice for s in slices):
        known = ", ".join(str(s.get("key") or "") for s in slices)
        die(f"slice not found: {args.slice!r}; known slice keys: {known}")

    note = args.note or ""
    if args.dry_run:
        print(yaml.safe_dump({"url": args.url, "status": args.status, "note": note}, sort_keys=False), end="")
        return

    action = store.add_pr(slice_key=args.slice, repo=args.repo, url=args.url, status=args.status, note=note)
    print(f"{action} PR: {args.slice}/{args.repo} {args.url} [{args.status}]")


def cmd_project(args: argparse.Namespace) -> None:
    """Convert a loose YAML task (+ sibling `<stem>.plans/` and other artifacts) into a
    new `BundleTaskLayout` under `$PI_JOB_TASKS/<slug>/` or an explicit bundle path.

    Source must be a loose `YamlTaskLayout`; bundles and directory stores are refused
    (already central/bundle-shaped, or unsupported as a migrate source). The destination
    is always a fresh `BundleTaskLayout` - never a loose YAML file, never an `FsTaskStore`
    directory - and `task.yaml` must not already exist there (no `--force`). On success,
    only the source yaml and its `<stem>.plans/` are deleted; other copied sibling
    dirs/files remain at the old location. On any failure, the freshly scaffolded
    destination is rolled back and the source is left untouched.
    """
    src_store = open_task_store(args.task, args.layout)
    if not isinstance(src_store, YamlTaskStore) or not isinstance(src_store.layout, YamlTaskLayout):
        die(
            "project requires a loose YAML task source, not a bundle or directory store: "
            f"{src_store.describe()}"
        )
    loose_layout = src_store.layout
    plans_dir = loose_layout.plans_dir

    bundle_root = resolve_project_dest(args.to, args.layout)
    dest_doc = bundle_root / BundleTaskLayout.DOCUMENT_NAME
    if dest_doc.exists():
        die(f"destination {dest_doc} already exists; project never overwrites (no --force)")

    scaffold_bundle_dirs(bundle_root)
    dst_store = YamlTaskStore(
        BundleTaskLayout(bundle_root),
        args.layout,
        create_only=True,
    )

    try:
        project(src_store, dst_store)
        copy_loose_artifacts(loose_layout.document_path, bundle_root)
        source_data = semantic_task_mapping(src_store.read(), source=src_store.describe())
        destination_data = semantic_task_mapping(dst_store.read(), source=dst_store.describe())
        if source_data != destination_data:
            die("projected YAML task failed semantic equality verification")
    except BaseException:
        shutil.rmtree(bundle_root, ignore_errors=True)
        raise

    removed = delete_loose_source(loose_layout.document_path, plans_dir)
    print(f"projected {src_store.describe()} -> {dst_store.describe()}")
    home = task_tasks_home(args.layout)
    if bundle_root.parent == home:
        print(f"slug: {bundle_root.name}")
    for path in removed:
        print(f"removed {path}")


def cmd_validate(args: argparse.Namespace) -> None:
    """Validate storage syntax, the Pydantic task contract, and profile structure."""
    task_arg = args.task
    store = open_task_store(task_arg, args.layout)
    task = store.read()
    slice_key = getattr(args, "slice", None)
    if slice_key is not None:
        slices = task.get("plan", {}).get("slices", []) or []
        if not any(str(sl.get("key") or "") == slice_key for sl in slices):
            die(f"slice not found: {slice_key!r}; known slice keys: {known_slice_keys(task)}")
        issues = slice_structure_issues(task, slice_key=slice_key)
        if issues:
            die(
                "slice structure invalid (every slice needs a contract kind and its "
                "kind's template steps):\n  - " + "\n  - ".join(issues)
            )
        full_issues = slice_structure_issues(task)
        if full_issues:
            count = len(full_issues)
            print(
                f"full-task: {count} legacy structure issue(s); use validate without --slice"
            )
    else:
        issues = slice_structure_issues(task)
        if issues:
            die(
                "slice structure invalid (every slice needs a contract kind and its "
                "kind's template steps):\n  - " + "\n  - ".join(issues)
            )
    title = task.get("title") or "<untitled>"
    print(f"ok: {store.describe()}")
    print(f"title: {title}")
    for issue in slice_template_warnings(task):
        print(f"warning: {issue}")
    for issue in execution_issues(task):
        print(f"warning: {issue}")
    for issue in note_length_warnings(task, task_path=task_arg if task_arg.is_file() else None):
        print(f"warning: {issue}")
    if isinstance(store, YamlTaskStore):
        print(f"schema: Pydantic {TaskDocument.__name__}")
        print("note: YAML task files are machine-owned; prefer pi-job commands over manual edits.")
    else:
        print("note: directory store checked via FsTaskStore.read().")










TASK_OPTIONAL_COMMANDS = frozenset({"advance", "channels", "kinds", "list", "loop", "profile", "schema"})
"""Subcommands that read no task document, so `--task` stays optional for them.

Every other subcommand needs a task; `main()` gates on this set before dispatch, so a
missing `--task` fails with an actionable error instead of a `None` path traceback deep
in `open_task_store`. A new task-reading subcommand needs no entry here."""


def missing_task_message(cmd: str) -> str:
    """Actionable `--task` error, shared by the `main()` gate and `require_task`."""
    return (
        f"--task is required for `pi-job {cmd}`\n"
        f"run: pi-job --task <slug|path> {cmd}\n"
        "known task slugs: pi-job list"
    )


def require_task(task_arg: Path | None, *, cmd: str) -> Path:
    """`task_arg` is already resolved by `resolve_task_arg` in `main()`; just gate on presence."""
    if task_arg is None:
        die(missing_task_message(cmd))
    return task_arg


def cmd_set_slice(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-slice")
    if args.layer and args.clear_layer:
        die("--layer and --clear-layer are mutually exclusive")
    depends_on = list(args.depends_on or [])
    if depends_on and args.clear_depends_on:
        die("--depends-on and --clear-depends-on are mutually exclusive")
    if any(dep == "" for dep in depends_on):
        die("--depends-on requires a non-empty slice key")
    if (
        args.title is None
        and args.goal is None
        and args.layer is None
        and not args.clear_layer
        and not depends_on
        and not args.clear_depends_on
    ):
        die(
            "at least one of --title, --goal, --layer, --clear-layer, "
            "--depends-on, or --clear-depends-on is required"
        )
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("set-slice requires a YAML task file")
    task = store.read()
    key = args.slice
    status_map = slice_status_map(task)
    if key not in status_map:
        die(f"slice not found: {key!r}")
    if status_map[key] in STATUS_DONE:
        die(f"cannot update completed slice: {key} [{status_map[key]}]")
    for dep in depends_on:
        if dep not in status_map:
            die(f"--depends-on slice not found: {dep!r}")
        if dep == key:
            die("cannot make a slice depend on itself")
    slices = task.get("plan", {}).get("slices", [])
    slice_dict = next((s for s in slices if s.get("key") == key), None)
    kind = str(slice_dict.get("kind") or "") if slice_dict else ""
    layer_value: str | None = None
    if args.clear_layer:
        if layer_names(task) and kind in LAYERED_SLICE_KINDS:
            die(
                f"cannot --clear-layer on kind {kind!r} when task.layers is set; "
                "rebind with --layer instead"
            )
    elif args.layer is not None:
        layer_value = validate_slice_layer(task, kind, args.layer)
    store.set_slice_fields(
        slice_key=key,
        title=args.title,
        goal=args.goal,
        layer=layer_value,
        clear_layer=bool(args.clear_layer),
        depends_on=depends_on,
        clear_depends_on=bool(args.clear_depends_on),
    )
    parts = [f"key={key}"]
    if args.title is not None:
        parts.append(f"title={args.title}")
    if args.goal is not None:
        parts.append(f"goal={args.goal}")
    if args.clear_layer:
        parts.append("layer=<cleared>")
    elif args.layer is not None:
        parts.append(f"layer={args.layer}")
    if args.clear_depends_on:
        parts.append("depends_on=<cleared>")
    print(f"updated slice: {', '.join(parts)}")
    existing_deps = list(slice_dict.get("depends_on") or []) if slice_dict else []
    for dep in depends_on:
        if dep in existing_deps:
            print(f"depends_on already includes {dep}")
        else:
            print(f"depends_on += {dep}")
            existing_deps.append(dep)


def cmd_block_slice(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="block-slice")
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("block-slice requires a YAML task file")
    task = store.read()
    key = args.slice
    status_map = slice_status_map(task)
    if key not in status_map:
        die(f"slice not found: {key!r}")
    if status_map[key] in STATUS_DONE:
        die(f"cannot block completed slice: {key} [{status_map[key]}]")
    gate = (getattr(args, "gate", None) or "").strip() or None
    if gate:
        if gate not in status_map:
            die(f"--gate slice not found: {gate!r}")
        if gate == key:
            die("cannot --gate a slice on itself")
    gate_added = store.block_slice(slice_key=key, reason=args.reason, gate=gate)
    print(f"blocked slice: {key}")
    if gate:
        if gate_added:
            print(f"depends_on += {gate}")
        else:
            print(f"depends_on already includes {gate}")


def cmd_unblock_slice(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="unblock-slice")
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("unblock-slice requires a YAML task file")
    task = store.read()
    key = args.slice
    status_map = slice_status_map(task)
    if key not in status_map:
        die(f"slice not found: {key!r}")
    if status_map[key] != "blocked":
        die(f"slice is not blocked: {key} [{status_map[key]}]")
    store.set_slice_status(slice_key=key, status="planned")
    print(f"unblocked slice: {key} [planned]")


def cmd_set_step_note(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-step-note")
    if args.replace and args.note is None:
        die("set-step-note --replace requires --note")
    if args.note is None:
        die("set-step-note requires --note")
    store = open_task_store(task_file, args.layout)
    task = store.read()
    task_slice = find_slice(task, args.slice)
    if task_slice is None:
        die(f"slice not found: {args.slice!r}")
    step = find_step(task_slice, args.step)
    if step is None:
        die(f"step not found: {args.slice}/{args.step}")
    store.set_step_note(
        slice_key=args.slice,
        step_key=args.step,
        note=args.note,
        replace=args.replace,
    )
    print(f"updated step note: {args.slice}/{args.step}")


def cmd_set_slice_note(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-slice-note")
    if args.replace and args.note is None:
        die("set-slice-note --replace requires --note")
    if args.note is None:
        die("set-slice-note requires --note")
    store = open_task_store(task_file, args.layout)
    task = store.read()
    task_slice = find_slice(task, args.slice)
    if task_slice is None:
        die(f"slice not found: {args.slice!r}")
    store.set_slice_note(slice_key=args.slice, note=args.note, replace=args.replace)
    print(f"updated slice note: {args.slice}")


def cmd_set_source(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-source")
    fields: dict[str, str] = {}
    for attr in ("jira", "discovered", "context"):
        value = getattr(args, attr, None)
        if value is not None:
            fields[attr] = value
    if not fields:
        die("at least one field is required (--jira, --discovered, --context)")
    store = open_task_store(task_file, args.layout)
    store.set_source(fields)
    print(f"updated source: {', '.join(f'{k}={v}' for k, v in fields.items())}")


def cmd_set_project(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-project")
    fields: dict[str, str] = {}
    for attr in ("key", "name", "route", "context"):
        value = getattr(args, attr, None)
        if value is not None:
            fields[attr] = value
    title = getattr(args, "title", None)
    if title is not None and not str(title).strip():
        die("title must be non-empty")
    if not fields and title is None:
        die("at least one field is required (--title, --key, --name, --route, --context)")
    store = open_task_store(task_file, args.layout)
    route_supplied = getattr(args, "route", None) is not None
    key_supplied = getattr(args, "key", None) is not None
    if route_supplied or key_supplied:
        task = store.read()
        merged_project = dict(task.get("project") or {})
        for attr in ("key", "name", "route", "context"):
            value = getattr(args, attr, None)
            if value is not None:
                merged_project[attr] = value
        validate_project_route_and_key(
            str(merged_project.get("route") or ""),
            str(merged_project.get("key") or ""),
            repo_root=Path.cwd(),
        )
    updated: list[str] = []
    if title is not None:
        store.set_title(str(title).strip())
        updated.append(f"title={str(title).strip()}")
    if fields:
        store.set_project(fields)
        updated.extend(f"{k}={v}" for k, v in fields.items())
    print(f"updated: {', '.join(updated)}")


def cmd_set_context(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-context")
    text = args.context
    if args.file_path:
        if text:
            die("--context and --file are mutually exclusive")
        if not args.file_path.exists():
            die(f"context file not found: {args.file_path}")
        text = args.file_path.read_text(encoding="utf-8")
    elif text is None:
        die("--context <text> or --file <path> is required")
    store = open_task_store(task_file, args.layout)
    store.set_context(text)
    print("updated context")


def cmd_add_decision_cli(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="add-decision")
    note = args.note
    if not note:
        die("--note is required")
    date = args.date or utc_now()[:10]
    source = args.source or "pi-job add-decision"
    plan_file_arg = getattr(args, "plan_file", None)
    soft_limit_hit = len(note) > NOTE_WARN_CHARS or (
        task_file.is_file() and task_file.stat().st_size > TASK_FILE_WARN_BYTES
    )
    spill = plan_file_arg is not None or soft_limit_hit
    spill_path: Path | None = None
    if plan_file_arg is not None:
        spill_path = Path(plan_file_arg)
        if not spill_path.is_absolute():
            spill_path = (task_file.parent / spill_path).resolve()
    store = open_task_store(task_file, args.layout)
    written: Path | None = None
    if spill:
        if not isinstance(store, YamlTaskStore):
            die("add-decision spill (--plan-file / soft limit) requires a YAML task file")
        written = store.add_decision(
            date=date,
            note=note,
            source=source,
            spill_body=note,
            spill_path=spill_path,
        )
        # Re-read the pointer note the store wrote for the CLI summary.
        decisions = store.read().get("decisions") or []
        yaml_note = str((decisions[-1] or {}).get("note") or "")
    else:
        store.add_decision(date=date, note=note, source=source)
        yaml_note = note
    print(
        f"added decision ({date}): {yaml_note[:60]}"
        + ("..." if len(yaml_note) > 60 else "")
    )
    if written is not None:
        print(f"spilled decision body: {written}")
    else:
        print(
            "tip: prefer Markdown in --note; "
            "`pi-job markdown` renders decisions as blockquotes"
        )


def cmd_add_finding(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="add-finding")
    note = args.note
    if not note:
        die("--note is required")
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("add-finding requires a YAML task file")
    source = args.source or "pi-job add-finding"
    path = store.add_finding(note=note, source=source)
    print(f"appended finding: {store.layout.findings_pointer()}")
    print(f"path: {path}")


def render_loop_packet(type_name: str) -> str:
    """Pure: load one named loop packet collapsed to one physical line."""
    packets = load_profile_contract()["loop_packets"]
    if type_name not in packets:
        valid = ", ".join(sorted(packets))
        die(f"unknown loop type {type_name!r}; valid types: {valid}")
    return " ".join(str(packets[type_name]).split())


def render_orchestrator_heartbeat() -> str:
    """Pure: render the manager packet for compatibility."""
    return render_loop_packet("manager")


def render_slice_worker_boot() -> str:
    """Pure: render the worker packet for compatibility."""
    return render_loop_packet("worker")


def cmd_loop(args: argparse.Namespace) -> None:
    type_name = (
        "worker"
        if getattr(args, "worker", False)
        else ("manager" if args.type_name is None else args.type_name)
    )
    print(render_loop_packet(type_name))


def render_investigate_interrupt(
    *,
    task_file: Path,
    cursor_label: str,
    topic: str,
    pointer: str,
    source: str,
    finding_status: str,
) -> str:
    """Pure: interpolate the profile investigate packet."""
    template = str(load_profile_contract()["instruction_packets"]["investigate_interrupt"])
    body = template.format(
        task_file=str(task_file),
        cursor_label=cursor_label,
        topic=topic,
        pointer=pointer,
        source=source,
        finding_status=finding_status,
    )
    return body if body.endswith("\n") else body + "\n"


def cmd_investigate(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="investigate")
    topic = (args.topic or "").strip()
    if not topic:
        die("--topic is required")
    note = (args.note or "").strip()
    source = args.source or f"investigate:{topic}"
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("investigate requires a YAML task file")
    task = store.read()
    claim = resolve_claim_for_command(task, args, cmd="investigate", required=False)
    cursor_label = claim_label(task, claim) if claim else "<none>"
    finding_path: Path | None = None
    if note:
        finding_path = store.add_finding(note=note, source=source)
    pointer = store.layout.findings_pointer()
    if finding_path is not None:
        finding_status = f"Recorded finding: {finding_path}"
    else:
        finding_status = "No --note given; add-finding when you have evidence."
    print(
        render_investigate_interrupt(
            task_file=task_file,
            cursor_label=cursor_label,
            topic=topic,
            pointer=pointer,
            source=source,
            finding_status=finding_status,
        ),
        end="",
    )


def cmd_acknowledge_edit(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="acknowledge-edit")
    reason = args.reason
    if not reason:
        die("--reason is required")
    store = open_task_store(task_file, args.layout)
    if not isinstance(store, YamlTaskStore):
        die("acknowledge-edit requires a YAML task file")
    task = store.read()
    if args.slice:
        slice_key = args.slice
        if find_slice(task, slice_key) is None:
            die(f"slice not found: {slice_key!r}")
    else:
        claim = resolve_claim_for_command(task, args, cmd="acknowledge-edit", required=True)
        assert claim is not None
        slice_key = claim.slice
    store.acknowledge_edit(reason=reason, slice_key=slice_key)
    print(
        f"acknowledged out-of-band edit on slice {slice_key}: "
        + reason[:60]
        + ("..." if len(reason) > 60 else "")
    )


def cmd_remove_slice(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="remove-slice")
    key = args.key
    store = open_task_store(task_file, args.layout)
    task = store.read()
    status_map = slice_status_map(task)
    if key not in status_map:
        die(f"slice not found: {key!r}")
    slices_map = task_slices_map(task)
    dependents = [sk for sk in status_map if sk != key and key in slices_map[sk].depends_on]
    if dependents:
        die(f"cannot remove slice {key!r}: other slices depend on it: {', '.join(dependents)}")
    claim = find_claim_by_slice(task, key)
    if claim is not None:
        die(f"cannot remove slice {key!r}: {claim.owner} has an active claim on it; release it first")
    store.remove_slice(key=key)
    print(f"removed slice: {key}")


def task_slices_map(task: dict[str, Any]) -> dict[str, TaskSlice]:
    return {ts.key: ts for ts in task_slices(task)}


def cmd_set_plan_note_cli(args: argparse.Namespace) -> None:
    task_file = require_task(args.task, cmd="set-plan-note")
    note = args.note if args.note is not None else ""
    store = open_task_store(task_file, args.layout)
    store.set_plan_note(note)
    print("updated plan note")


def _task_label(task_file: Path) -> str:
    if task_file.name == "task.yaml":
        return task_file.parent.name
    return task_file.stem


def cmd_stats(args: argparse.Namespace) -> None:
    from pi_job_harness.emit import emit_output
    from pi_job_harness.stats import (
        DEFAULT_WAIT_KEYS,
        build_stats,
        render_json,
        render_markdown,
    )

    wait_keys: set[str] = set() if args.no_default_wait_keys else set(DEFAULT_WAIT_KEYS)
    wait_keys.update(args.wait_key or [])
    store = open_task_store(args.task, args.layout)
    task = store.read()
    payload = build_stats(task, _task_label(Path(args.task)), frozenset(wait_keys))
    body = render_json(payload) if args.json_output else render_markdown(payload)
    emit_output(body, args.out)


def cmd_report(args: argparse.Namespace) -> None:
    from pi_job_harness.emit import emit_output
    from pi_job_harness.report import (
        build_report,
        parse_since,
        render_json,
        render_markdown,
    )

    try:
        since = parse_since(args.since)
    except ValueError as exc:
        die(str(exc))
    store = open_task_store(args.task, args.layout)
    task = store.read()
    rows = build_report(task, since)
    label = _task_label(Path(args.task))
    body = render_json(rows) if args.json_output else render_markdown(label, since, rows)
    emit_output(body, args.out)


def cmd_profile(args: argparse.Namespace) -> None:
    profile = load_profile_contract()
    overlay = args.layout.profile_overlay_to_load()
    overlay_value = str(overlay) if overlay is not None else None
    if args.json_output:
        payload = {"profile": str(PROFILE), "overlay": overlay_value, **profile}
        print(json.dumps(payload, indent=2))
        return
    kinds = profile.get("slice_kinds", {})
    steps = profile.get("step_kinds", {})
    aids = profile.get("toolbelt", {})
    print(f"Profile: {PROFILE}")
    print(f"Overlay: {overlay_value if overlay_value is not None else '(none)'}")
    print(f"Slice kinds ({len(kinds)}): {', '.join(sorted(kinds))}")
    print(f"Step kinds ({len(steps)}): {', '.join(sorted(steps))}")
    print(f"Toolbelt aids ({len(aids)}): {', '.join(sorted(aids))}")
    print("Pass --json for the full validated profile.")


def cmd_channels(args: argparse.Namespace) -> None:
    profile = load_profile_contract()
    channels = profile["record_channels"]
    if args.step:
        step_kind = get_step_kind(args.step)
        print(f"Step {args.step} record channels:")
        for channel_id in step_kind.get("record_channels") or []:
            blurb = channels["blurbs"].get(channel_id, "")
            print(f"- {channel_id}: {blurb}")
        return
    print(channels["catalog"].rstrip())


def cmd_schema(args: argparse.Namespace) -> None:
    if args.json_output:
        schemas = {
            "task": TaskDocument.model_json_schema(),
            "create": BootstrapDocument.model_json_schema(),
        }
        print(json.dumps(schemas, indent=2))
        return
    print(f"Task document: Pydantic {TaskDocument.__name__} ({len(TaskDocument.model_fields)} fields)")
    print(f"Create intent: Pydantic {BootstrapDocument.__name__} ({len(BootstrapDocument.model_fields)} fields)")
    print(f"Profile: {PROFILE}")
    print("Pass --json for a complete JSON Schema dump.")


def cmd_kinds(args: argparse.Namespace) -> None:
    kinds_action = args.kinds_action
    kinds = contract_slice_kinds()
    if kinds_action == "show":
        kind_key = args.kind_key
        if kind_key not in kinds:
            die(f"unknown slice kind {kind_key!r}; expected one of: {', '.join(sorted(kinds))}")
        entry = kinds[kind_key]
        template_steps = entry.get("step_template") or []
        step_kinds = contract_step_kinds()
        if args.json_output:
            resolved = dict(entry)
            resolved["steps"] = [
                {"key": key, "title": step_kinds.get(key, {}).get("title", key), "owner": step_kinds.get(key, {}).get("owner", "orchestrator")}
                for key in template_steps
            ]
            resolved["required_steps"] = entry.get("required_steps", template_steps)
            print(json.dumps(resolved, indent=2))
            return
        print(f"Slice kind: {kind_key}")
        print(f"  Title: {entry.get('title', kind_key)}")
        print(f"  Description: {entry.get('description', '')}")
        required = entry.get("required_steps") or template_steps
        print(f"  Required steps: {', '.join(required)}")
        print(f"  Step template ({len(template_steps)}):")
        for key in template_steps:
            sk = step_kinds.get(key, {})
            print(f"    {key} [{sk.get('owner', 'orchestrator')}] {sk.get('title', key)}")
        policies = entry.get("policies") or {}
        if policies:
            print(f"  Policies: {json.dumps(policies)}")
        return
    if args.json_output:
        print(json.dumps({k: v for k, v in sorted(kinds.items())}, indent=2))
        return
    step_kinds = contract_step_kinds()
    for key in sorted(kinds):
        entry = kinds[key]
        template = entry.get("step_template") or []
        step_desc = ", ".join(template)
        print(f"  {key} — {entry.get('title', key)}: {step_desc}")



def main(layout: PiJobLayout | None = None) -> None:
    layout = layout or PiJobLayout.from_environ()
    slice_kinds = sorted(valid_slice_kinds())
    cli_help = load_profile_contract()["cli_help"]
    add_decision_help = str(cli_help["add_decision"]["command"])
    add_decision_note_help = str(cli_help["add_decision"]["note"])
    set_step_note_help = str(cli_help["set_step_note"]["command"])
    set_step_note_note_help = str(cli_help["set_step_note"]["note"])
    set_slice_note_help = str(cli_help["set_slice_note"]["command"])
    set_slice_note_note_help = str(cli_help["set_slice_note"]["note"])
    set_source_help = str(cli_help["set_source"]["command"])
    set_source_note_help = str(cli_help["set_source"]["note"])
    finish_note_help = str(cli_help["finish"]["note"])
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--task",
        type=str,
        default=None,
        help=(
            "task slug (resolves under $PI_JOB_TASKS, default ~/.local/share/pi-job/tasks) "
            "or a YAML task file / existing directory store to orchestrate"
        ),
    )
    sub = ap.add_subparsers(dest="cmd", required=True)

    list_cmd = sub.add_parser(
        "list",
        help=(
            "show task bundles under $PI_JOB_TASKS as readable activity-sorted blocks; "
            "does not take --task, and ignores loose YAML in the task home"
        ),
    )
    list_cmd.set_defaults(fn=cmd_list)

    archive_cmd = sub.add_parser(
        "archive",
        help=(
            "move a $PI_JOB_TASKS home bundle into the archive home "
            "($PI_JOB_ARCHIVE or <tasks-parent>/archive); frees the slug for create"
        ),
    )
    archive_cmd.add_argument(
        "--to",
        default=None,
        metavar="SLUG",
        help="archive under this slug (default: keep the current slug)",
    )
    archive_cmd.add_argument(
        "--dry-run",
        action="store_true",
        help="print the destination path without moving the bundle",
    )
    archive_cmd.set_defaults(fn=cmd_archive)

    status = sub.add_parser("status", help="show task status, saved cursor, and Ready frontier")
    status.set_defaults(fn=cmd_status)

    add_msg_parser(sub, cli_help)

    validate = sub.add_parser(
        "validate",
        help="validate task syntax, documented Pydantic fields, profile references, and slice structure",
    )
    validate.add_argument(
        "--slice",
        help="validate only this slice's profile structure (full-task legacy debt is reported non-fatally)",
    )
    validate.set_defaults(fn=cmd_validate)

    create = sub.add_parser(
        "create",
        help=(
            "create and initialize a task file "
            "(--from intent YAML, or --kind/--empty-plan skeleton; also inits an existing uninitialized file)"
        ),
    )
    create.add_argument("--from", dest="from_path", type=Path, help="create-intent YAML document")
    create.add_argument("--title", default=None, help="task.title when not using --from (default: Example bounded change)")
    create.add_argument(
        "--kind",
        choices=slice_kinds,
        help="seed a single initial slice of this kind (and initialize); mutually exclusive with --empty-plan/--from",
    )
    create.add_argument(
        "--empty-plan",
        action="store_true",
        help="write a task with no slices and no orchestration yet; add slices then re-run create",
    )
    create.add_argument(
        "--goal",
        help="real slice outcome when seeding slices (--kind or default implement scaffold); required unless --empty-plan or --from",
    )
    create.add_argument("--force", action="store_true", help="overwrite an existing task file")
    create.add_argument("--dry-run", action="store_true", help="print the would-be task (or intent diff) without writing")
    create.set_defaults(fn=cmd_create)

    claim = sub.add_parser(
        "claim",
        help="claim a Ready slice for an owner; required before start (one claim per owner, one owner per slice)",
    )
    claim.add_argument("--slice", required=True, metavar="KEY", help="Ready slice to claim")
    claim.add_argument("--owner", help="claim owner identity; defaults to $PI_JOB_OWNER")
    claim.set_defaults(fn=cmd_claim)

    release = sub.add_parser(
        "release",
        help="drop an owner's claim (any claim, not self-only); slice status is unchanged",
    )
    release.add_argument(
        "--owner",
        help="claim owner to release; defaults to $PI_JOB_OWNER, then the sole active claim",
    )
    release.set_defaults(fn=cmd_release)

    adv = sub.add_parser(
        "advance",
        help="deprecated: use `claim`/`instruction`; kept only to fail with actionable guidance",
    )
    adv.add_argument("--slice", help="explicit slice key (requires --step)")
    adv.add_argument("--step", help="explicit step key")
    adv.add_argument("--dry-run", action="store_true")
    adv_force_resync = adv.add_mutually_exclusive_group()
    adv_force_resync.add_argument("--force", action="store_true", help="advance past an unfinished current step")
    adv_force_resync.add_argument(
        "--resync",
        action="store_true",
        help="realign cursor without changing step status (requires --reason)",
    )
    adv.add_argument("--reason", help="required with --force or --resync; audit reason")
    adv.set_defaults(fn=cmd_advance)

    start = sub.add_parser("start", help="record model/start time and mark the current step or selected slice in progress")
    start.add_argument("--model", required=True, help="fully qualified model ID, for example openai/gpt-5.6-sol")
    start.add_argument("--slice", help="explicit slice key; without --step this targets the slice itself")
    start.add_argument("--step", help="explicit step key (uses the current slice unless --slice is supplied)")
    start.add_argument("--slice-only", action="store_true", help="target the current slice instead of its current step")
    start.add_argument(
        "--owner",
        help="claim owner whose position to act on; defaults to $PI_JOB_OWNER, then the sole active claim",
    )
    start.set_defaults(fn=cmd_start)

    finish = sub.add_parser(
        "finish",
        help="record end time and mark the current step or selected slice done/skipped",
        description="record end time and mark the current step or selected slice done/skipped",
    )
    finish.add_argument(
        "--model",
        help=(
            "fully qualified model ID; required for atomic skip/finish on a never-started step "
            "(explicit --slice --step one-shot finish also requires --model and --note)"
        ),
    )
    finish.add_argument("--slice", help="explicit slice key; without --step this targets the slice itself")
    finish.add_argument("--step", help="explicit step key (uses the current slice unless --slice is supplied)")
    finish.add_argument("--slice-only", action="store_true", help="target the current slice instead of its current step")
    finish.add_argument(
        "--owner",
        help="claim owner whose position to act on; defaults to $PI_JOB_OWNER, then the sole active claim",
    )
    finish.add_argument(
        "--note",
        help=finish_note_help,
    )
    finish.add_argument(
        "--replace",
        action="store_true",
        help="overwrite the existing note instead of appending; requires --note",
    )
    finish.add_argument(
        "--reconcile",
        action="store_true",
        help="finish an in_progress target without a prior start (requires --model and --note)",
    )
    finish.add_argument("--skip", action="store_true", help="mark skipped instead of done")
    finish.add_argument("--reason", help="required with --skip")
    finish.set_defaults(fn=cmd_finish)

    plan = sub.add_parser("plan", help="print slice/step plan from the task file and step/slice-kind contract")
    plan.set_defaults(fn=cmd_plan)

    wayfinder_context = sub.add_parser(
        "wayfinder-context",
        help="print the Wayfinder map from the task file (destination, decisions, and slices split into frontier vs fog) for the wayfinder step to load before charting",
    )
    wayfinder_context.set_defaults(fn=cmd_wayfinder_context)

    sync = sub.add_parser(
        "sync",
        help=(
            "print last-recorded slices to re-verify (in_progress/blocked, or open PR); "
            "never calls gh/Jira - orchestrator must run live checks before reporting status"
        ),
    )
    sync.add_argument("--status", help="comma-separated slice statuses to check instead of the default in_progress/blocked-or-open-PR selection")
    sync.set_defaults(fn=cmd_sync)

    layers_cmd = sub.add_parser(
        "layers",
        help=str(cli_help["layers"]["command"]),
        description=str(cli_help["layers"]["command"]),
        epilog=str(cli_help["layers"]["note"]),
    )
    layers_sub = layers_cmd.add_subparsers(dest="layers_action", required=True)
    layers_show = layers_sub.add_parser("show", help="list registered layers and slice survival report")
    layers_show.set_defaults(fn=cmd_layers)
    layers_add = layers_sub.add_parser("add", help="register a layer band")
    layers_add.add_argument("--name", required=True, help="stable layer slug")
    layers_add.add_argument("--description", required=True, help="one-line band description")
    layers_add.add_argument("--references", help="comma-separated paths or URLs")
    layers_add.add_argument("--after", help="insert after this existing layer name")
    layers_add.add_argument(
        "--bind",
        action="append",
        default=[],
        metavar="SLICE=LAYER",
        help="bind an existing unlayered implement/spike/research slice in this write; repeatable",
    )
    layers_add.set_defaults(fn=cmd_layers)
    layers_set = layers_sub.add_parser("set", help="update a layer description and/or references")
    layers_set.add_argument("--name", required=True, help="existing layer name")
    layers_set.add_argument("--description", help="new one-line description")
    layers_set.add_argument("--references", help="comma-separated paths or URLs (replaces the list)")
    layers_set.set_defaults(fn=cmd_layers)
    layers_remove = layers_sub.add_parser("remove", help="remove a layer (refuses when slices still bind it)")
    layers_remove.add_argument("--name", required=True, help="layer name to remove")
    layers_remove.set_defaults(fn=cmd_layers)
    layers_rename = layers_sub.add_parser("rename", help="rename a layer and rebind bound slices")
    layers_rename.add_argument("--old", required=True, help="current layer name")
    layers_rename.add_argument("--new", required=True, help="new layer name")
    layers_rename.set_defaults(fn=cmd_layers)
    layers_reorder = layers_sub.add_parser("reorder", help="set registry order (Mermaid band order)")
    layers_reorder.add_argument(
        "--order",
        required=True,
        help="comma-separated layer names listing each registry entry exactly once",
    )
    layers_reorder.set_defaults(fn=cmd_layers)

    files_cmd = sub.add_parser(
        "files",
        help="list task artifact paths (references/, plans/, registered artifacts) one per line",
    )
    files_cmd.add_argument(
        "--relative",
        action="store_true",
        help="print bundle-relative paths; paths outside the bundle stay absolute",
    )
    files_cmd.set_defaults(fn=cmd_files)

    tb = sub.add_parser("toolbelt", help="list toolbelt aids for slice kind(s), or add one")
    tb.add_argument("action", nargs="?", choices=["add"], help="'add' to register an aid")
    tb.add_argument("key", nargs="?", help="aid key when using add")
    tb.add_argument("--kind", choices=slice_kinds, help="filter aids to those suiting this slice kind")
    tb.add_argument("--path", help="artifact path to record")
    tb.add_argument(
        "--status",
        default="planned",
        choices=ARTIFACT_STATUSES,
        help="artifact status (default planned)",
    )
    tb.add_argument("--note", default="", help="artifact note")
    tb.set_defaults(fn=cmd_toolbelt)

    maintain_cmd = sub.add_parser(
        "maintain",
        help="list or register surfaces the orchestrator must keep current",
    )
    maintain_cmd.add_argument(
        "action",
        nargs="?",
        choices=["add", "remove"],
        help="'add' to register/update a uri, 'remove' to drop one; omit to list",
    )
    maintain_cmd.add_argument("--uri", help="path, PR URL, or ticket URL")
    maintain_cmd.add_argument(
        "--note",
        default="",
        help="what current means and when to update (required for add)",
    )
    maintain_cmd.set_defaults(fn=cmd_maintain)

    show = sub.add_parser("show", help="render the task as a cursor-focused slice/step tree")
    show.add_argument("--all", action="store_true", help="expand every slice (including done/skipped detail)")
    show.add_argument(
        "--started", action="store_true",
        help="also expand in_progress/blocked slices (not the cursor's slice); done/skipped stay collapsed unless --all",
    )
    show.add_argument(
        "--full",
        action="store_true",
        help="include executor model IDs on slice headers and expanded steps (tree view)",
    )
    show.add_argument(
        "--short",
        action="store_true",
        help="collapse consecutive done slices onto one line (names only); ignored with --all or --slice",
    )
    show.add_argument(
        "--work-first",
        action="store_true",
        help=(
            "reorder tree: unfinished slices first (cursor/in_progress, then Ready, then blocked), "
            "newest-touched first within each group; done/skipped last, newest-completed first"
        ),
    )
    show.add_argument(
        "--graph",
        action="store_true",
        help=(
            "emit a Mermaid flowchart of slice depends_on (stdout only; pipe to termaid). "
            "classDef colors: green done, blue in_progress/cursor, gray planned, red blocked, yellow skipped. "
            "Ignores tree flags; mutually exclusive with --slice"
        ),
    )
    show.add_argument(
        "--by-layer",
        action="store_true",
        help=(
            "with --graph: group slices into Mermaid subgraphs by task.layers order; "
            "unlayered kinds sit outside subgraphs. Ignored without --graph or when layers empty."
        ),
    )
    show.add_argument(
        "--status",
        help=(
            "comma-separated slice statuses to include; "
            "for done/skipped also prints repo_work.worktree when set "
            "(agents: list recorded worktrees via show --status done, "
            "or show --all / show --slice KEY)"
        ),
    )
    show.add_argument(
        "--color",
        choices=("auto", "always", "never"),
        default="auto",
        help="color status glyphs (✓ green, ✗ red, …); auto = TTY only, respects NO_COLOR",
    )
    show.add_argument(
        "--slice",
        metavar="KEY",
        help="render one slice in full (goal, notes, steps, repo_work, models); ignores tree expansion flags",
    )
    show.set_defaults(fn=cmd_show)

    markdown = sub.add_parser(
        "markdown",
        help="render the task as a read-only Markdown preview on stdout",
    )
    markdown.add_argument(
        "--chronological",
        action="store_true",
        help="sort slices oldest-changed-first using execution timestamps (plan order tie-break)",
    )
    markdown.add_argument(
        "--summary",
        action="store_true",
        help="compact preview: Decisions + Contents + slice headers/goals only (no steps/notes)",
    )
    markdown.add_argument(
        "--slice",
        metavar="KEY",
        help=(
            "slice-scoped preview (header + steps + plan file); "
            "omit full Decisions/preamble unless --with-decisions/--with-preamble; "
            "mutually exclusive with --summary"
        ),
    )
    markdown.add_argument(
        "--with-decisions",
        action="store_true",
        help="with --slice: include the full ## Decisions block",
    )
    markdown.add_argument(
        "--with-preamble",
        action="store_true",
        help="with --slice: include project/context/source/plan note/artifacts",
    )
    markdown.set_defaults(fn=cmd_markdown)

    stats_cmd = sub.add_parser(
        "stats",
        help="task execution stats (models, wait-filtered durations, weekly velocity)",
    )
    stats_cmd.add_argument("--json", dest="json_output", action="store_true", help="emit JSON instead of markdown")
    stats_cmd.add_argument("-o", "--out", help="write the chosen render to PATH (no stdout body)")
    stats_cmd.add_argument(
        "--wait-key",
        action="append",
        default=[],
        help="extra step key treated as wait (repeatable)",
    )
    stats_cmd.add_argument(
        "--no-default-wait-keys",
        action="store_true",
        help="do not treat the built-in wait-* keys as wait",
    )
    stats_cmd.set_defaults(fn=cmd_stats)

    report_cmd = sub.add_parser(
        "report",
        help="done slices since a UTC date, with recorded repo_work PR links",
    )
    report_cmd.add_argument(
        "--since",
        required=True,
        help="UTC date YYYY-MM-DD inclusive; required",
    )
    report_cmd.add_argument("--json", dest="json_output", action="store_true", help="emit JSON instead of markdown")
    report_cmd.add_argument("-o", "--out", help="write the chosen render to PATH (no stdout body)")
    report_cmd.set_defaults(fn=cmd_report)

    instruction = sub.add_parser(
        "instruction",
        help="emit deterministic instructions for the saved cursor (or pick-next when the slice is exhausted)",
    )
    instruction.add_argument(
        "--current",
        action="store_true",
        help="deprecated no-op; instruction always uses the resolved claim's derived position",
    )
    instruction.add_argument(
        "--owner",
        help="claim owner to emit instructions for; defaults to $PI_JOB_OWNER, then the sole active claim",
    )
    instruction.set_defaults(fn=cmd_instruction)

    add_slice = sub.add_parser("add-slice", help="append a slice to plan.slices (--kind required; steps from slice_kinds template)")
    add_slice.add_argument("--key", required=True)
    add_slice.add_argument("--kind", required=True, choices=slice_kinds)
    add_slice.add_argument("--title", required=True)
    add_slice.add_argument("--goal", required=True)
    add_slice.add_argument("--repos", help="comma-separated; required if this file's #Slice schema declares repos as non-optional")
    add_slice.add_argument("--depends-on", help="comma-separated slice keys this slice depends on")
    add_slice.add_argument(
        "--layer",
        help=(
            "task.layers name for implement/spike/research slices; required when "
            "task.layers is non-empty"
        ),
    )
    add_slice.add_argument("--after", help="insert after this existing slice key; default: end of plan.slices")
    add_slice.add_argument("--dry-run", action="store_true")
    add_slice.set_defaults(fn=cmd_add_slice)

    set_project = sub.add_parser("set-project", help="update task title and/or project fields (merge into existing project)")
    set_project.add_argument("--title", help="task.title (non-empty)")
    set_project.add_argument("--key", help="project key")
    set_project.add_argument("--name", help="project name")
    set_project.add_argument("--route", help="project route path")
    set_project.add_argument("--context", help="project context description")
    set_project.set_defaults(fn=cmd_set_project)

    set_context = sub.add_parser("set-context", help="replace task.context")
    set_context.add_argument(
        "--context",
        help="new context as Markdown (preferred); rendered formatted in `pi-job markdown`",
    )
    set_context.add_argument("--file", dest="file_path", type=Path, help="read context from a file")
    set_context.set_defaults(fn=cmd_set_context)

    add_decision = sub.add_parser(
        "add-decision",
        help=add_decision_help,
        description=add_decision_help,
    )
    add_decision.add_argument("--date", default="", help="decision date (default: today UTC)")
    add_decision.add_argument(
        "--note",
        required=True,
        help=add_decision_note_help,
    )
    add_decision.add_argument("--source", default="", help="decision origin (default: pi-job add-decision)")
    add_decision.add_argument(
        "--plan-file",
        type=Path,
        default=None,
        help=(
            "write the long note body to this path (relative to the task dir OK); "
            "YAML stores a Plan file: pointer. Auto-spills under .plans/ when note "
            f"or task file exceeds soft limits ({NOTE_WARN_CHARS} chars / {TASK_FILE_WARN_BYTES} bytes)."
        ),
    )
    add_decision.set_defaults(fn=cmd_add_decision_cli)

    add_finding = sub.add_parser(
        "add-finding",
        help="append RCA/evidence to <task-stem>.plans/_findings.md (not the task YAML)",
    )
    add_finding.add_argument("--note", required=True, help="finding body (Markdown OK)")
    add_finding.add_argument("--source", default="", help="origin label (default: pi-job add-finding)")
    add_finding.set_defaults(fn=cmd_add_finding)

    investigate = sub.add_parser(
        "investigate",
        help=(
            "interrupt/RCA packet against a task without moving the saved cursor; "
            "optional --note appends via add-finding"
        ),
    )
    investigate.add_argument("--topic", required=True, help="short investigation topic")
    investigate.add_argument(
        "--note",
        default="",
        help="optional finding body to append immediately to .plans/_findings.md",
    )
    investigate.add_argument("--source", default="", help="finding source label")
    investigate.add_argument(
        "--owner",
        help="claim owner to report as parked; defaults to $PI_JOB_OWNER, then the sole active claim",
    )
    investigate.set_defaults(fn=cmd_investigate)

    loop = sub.add_parser(
        "loop",
        help=(
            "print a named loop packet from profile.yaml (manager by default; no --task); "
            "use --worker as the worker compatibility alias"
        ),
    )
    loop_selection = loop.add_mutually_exclusive_group()
    loop_selection.add_argument(
        "--worker",
        action="store_true",
        help="print the worker packet instead of the manager packet",
    )
    loop_selection.add_argument(
        "--type",
        dest="type_name",
        metavar="NAME",
        help="print the exact, case-sensitive named loop packet",
    )
    loop.set_defaults(fn=cmd_loop)

    acknowledge_edit = sub.add_parser(
        "acknowledge-edit",
        help=(
            "refresh content digest after a legitimate hand-edit; "
            "appends --reason to the target slice note"
        ),
    )
    acknowledge_edit.add_argument(
        "--reason",
        required=True,
        help="why the out-of-band edit was made (appended to the target slice note)",
    )
    acknowledge_edit.add_argument("--slice", help="explicit slice key; defaults to the resolved claim's slice")
    acknowledge_edit.add_argument(
        "--owner",
        help="claim owner whose slice to target; defaults to $PI_JOB_OWNER, then the sole active claim",
    )
    acknowledge_edit.set_defaults(fn=cmd_acknowledge_edit)

    remove_slice = sub.add_parser("remove-slice", help="remove a slice from the plan (refuses when other slices depend on it)")
    remove_slice.add_argument("--key", required=True, help="slice key to remove")
    remove_slice.set_defaults(fn=cmd_remove_slice)

    set_plan_note = sub.add_parser("set-plan-note", help="set task.plan.note")
    set_plan_note.add_argument(
        "--note",
        default="",
        help="plan note as Markdown (preferred); rendered formatted in `pi-job markdown`",
    )
    set_plan_note.set_defaults(fn=cmd_set_plan_note_cli)

    set_slice = sub.add_parser(
        "set-slice",
        help=str(cli_help["set_slice"]["command"]),
        description=str(cli_help["set_slice"]["command"]),
        epilog=str(cli_help["set_slice"]["note"]),
    )
    set_slice.add_argument("--slice", required=True, help="slice key to update")
    set_slice.add_argument("--title", help="new slice title")
    set_slice.add_argument("--goal", help="new slice goal")
    set_slice_layer = set_slice.add_mutually_exclusive_group()
    set_slice_layer.add_argument(
        "--layer",
        help="bind slice to a task.layers name (implement/spike/research when registry set)",
    )
    set_slice_layer.add_argument(
        "--clear-layer",
        action="store_true",
        help="remove slice layer binding (not allowed for layered kinds when registry set)",
    )
    set_slice.add_argument(
        "--depends-on",
        action="append",
        default=[],
        metavar="KEY",
        help="append a missing dependency to this consumer slice; repeatable",
    )
    set_slice.add_argument(
        "--clear-depends-on",
        action="store_true",
        help="clear every dependency from this slice",
    )
    set_slice.set_defaults(fn=cmd_set_slice)

    block_slice = sub.add_parser("block-slice", help="mark a slice blocked and append a reason to its note")
    block_slice.add_argument("--slice", required=True, help="slice key to block")
    block_slice.add_argument("--reason", required=True, help="blocker reason appended to the slice note")
    block_slice.add_argument(
        "--gate",
        default="",
        help="optional slice key to append to this slice's depends_on (hard gate)",
    )
    block_slice.set_defaults(fn=cmd_block_slice)

    unblock_slice = sub.add_parser("unblock-slice", help="restore a blocked slice to planned without changing its note")
    unblock_slice.add_argument("--slice", required=True, help="slice key to unblock")
    unblock_slice.set_defaults(fn=cmd_unblock_slice)

    set_step_note = sub.add_parser(
        "set-step-note",
        help=set_step_note_help,
        description=set_step_note_help,
    )
    set_step_note.add_argument("--slice", required=True, help="slice key")
    set_step_note.add_argument("--step", required=True, help="step key")
    set_step_note.add_argument(
        "--note",
        help=set_step_note_note_help,
    )
    set_step_note.add_argument(
        "--replace",
        action="store_true",
        help="overwrite the existing note instead of appending; requires --note",
    )
    set_step_note.set_defaults(fn=cmd_set_step_note)

    set_slice_note = sub.add_parser(
        "set-slice-note",
        help=set_slice_note_help,
        description=set_slice_note_help,
    )
    set_slice_note.add_argument("--slice", required=True, help="slice key")
    set_slice_note.add_argument(
        "--note",
        help=set_slice_note_note_help,
    )
    set_slice_note.add_argument(
        "--replace",
        action="store_true",
        help="overwrite the existing note instead of appending; requires --note",
    )
    set_slice_note.set_defaults(fn=cmd_set_slice_note)

    set_source = sub.add_parser(
        "set-source",
        help=set_source_help,
        description=set_source_help,
        epilog=set_source_note_help,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    set_source.add_argument("--jira", help="Jira issue key or URL")
    set_source.add_argument("--discovered", help="discovery date or identifier")
    set_source.add_argument(
        "--context",
        help="brief discovery note (why this task was opened; not task.context)",
    )
    set_source.set_defaults(fn=cmd_set_source)

    profile_cmd = sub.add_parser("profile", help="show the active execution profile")
    profile_cmd.add_argument("--json", dest="json_output", action="store_true", help="output full validated profile as JSON")
    profile_cmd.set_defaults(fn=cmd_profile)

    channels_cmd = sub.add_parser(
        "channels",
        help="show the full task-record channel map, or one step's record_channels blurbs",
    )
    channels_cmd.add_argument("--step", help="step kind key; print only that step's record_channels blurbs")
    channels_cmd.set_defaults(fn=cmd_channels)

    schema_cmd = sub.add_parser("schema", help="show the task document schema")
    schema_cmd.add_argument("--json", dest="json_output", action="store_true", help="output JSON Schema for task and bootstrap documents")
    schema_cmd.set_defaults(fn=cmd_schema)

    kinds_cmd = sub.add_parser("kinds", help="list or show slice kinds")
    kinds_sub = kinds_cmd.add_subparsers(dest="kinds_action", required=True)
    kinds_list = kinds_sub.add_parser("list", help="list all slice kinds")
    kinds_list.add_argument("--json", dest="json_output", action="store_true", help="output machine-readable JSON")
    kinds_list.set_defaults(fn=cmd_kinds)
    kinds_show = kinds_sub.add_parser("show", help="show details for a specific slice kind")
    kinds_show.add_argument("kind_key", help="slice kind to show")
    kinds_show.add_argument("--json", dest="json_output", action="store_true", help="output machine-readable JSON")
    kinds_show.set_defaults(fn=cmd_kinds)

    add_step = sub.add_parser("add-step", help="append a step to a slice's steps (or final_steps with --final)")
    add_step.add_argument("--slice", required=True)
    add_step.add_argument("--key", required=True)
    add_step.add_argument("--title", required=True)
    add_step.add_argument("--note", default="")
    add_step.add_argument("--final", action="store_true")
    add_step.add_argument("--after", help="insert after this existing step key; default: end of the target array")
    add_step.add_argument("--dry-run", action="store_true")
    add_step.set_defaults(fn=cmd_add_step)

    set_worktree = sub.add_parser("set-worktree", help="record or update the filesystem worktree path for a slice's repo work")
    set_worktree.add_argument("--slice", required=True)
    set_worktree.add_argument("--repo", required=True)
    set_worktree_mode = set_worktree.add_mutually_exclusive_group(required=False)
    set_worktree_mode.add_argument(
        "--path",
        help=(
            "record or update the filesystem worktree path; when omitted (and --clear is "
            "not given), pi-job prints the recommended path and exits non-zero instead of "
            "recording anything. Convention: $PI_JOB_WORKTREES/<slug>/<slice>/<repo> "
            "(default worktree home ~/.local/share/pi-job/worktrees)"
        ),
    )
    set_worktree_mode.add_argument(
        "--clear",
        action="store_true",
        help="clear the recorded worktree path for this slice/repo",
    )
    set_worktree.add_argument("--dry-run", action="store_true")
    set_worktree.set_defaults(fn=cmd_set_worktree)

    add_pr = sub.add_parser("add-pr", help="record a PR for a slice's repo work, upserting by URL if it already exists")
    add_pr.add_argument("--slice", required=True)
    add_pr.add_argument("--repo", required=True)
    add_pr.add_argument("--url", required=True)
    add_pr.add_argument("--status", required=True, choices=["open", "merged", "closed"])
    add_pr.add_argument("--note", default="")
    add_pr.add_argument("--dry-run", action="store_true")
    add_pr.set_defaults(fn=cmd_add_pr)


    project_cmd = sub.add_parser(
        "project",
        help="convert a loose YAML task (+ sibling plans/artifacts) into a central task bundle",
    )
    project_cmd.add_argument(
        "--to", required=True, type=str,
        help=(
            "destination bundle: a task slug (under $PI_JOB_TASKS), a bundle directory, "
            "or its task.yaml (never a loose YAML file or directory store)"
        ),
    )
    project_cmd.set_defaults(fn=cmd_project)

    args = ap.parse_args()
    args.layout = layout
    if args.task is None:
        if args.cmd not in TASK_OPTIONAL_COMMANDS:
            die(missing_task_message(args.cmd))
    elif args.cmd == "create":
        args.task = resolve_create_task_arg(args.task, layout)
    else:
        args.task = resolve_task_arg(args.task, layout)
    args.fn(args)


if __name__ == "__main__":
    main()
