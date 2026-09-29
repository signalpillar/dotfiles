"""Read-only bundle access for the viewer.

Single source of truth for every task fact the routes render.
Takes a host layout in, returns validated snapshots out.
Knows nothing about HTTP, htmx, or templates.
"""

from __future__ import annotations

import logging
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path

import yaml

log = logging.getLogger(__name__)


@contextmanager
def _quiet_read(store):
    """Read without digest warnings.

    The viewer never mutates task state, so out-of-band-edit staleness is
    noise in server logs. Uses the store's own suppression hook (the same one
    internal mutations use); stores without the hook read as-is.
    """
    suppress = getattr(store, "_suppress_digest_warn", None)
    if suppress is None:
        yield
    else:
        with suppress():
            yield


@dataclass(frozen=True)
class BundleSummary:
    """One row on the bundle list page."""

    slug: str  # bundle directory name under the tasks home
    title: str  # task title from task.yaml, slug fallback when empty
    status: str  # derived overall status (slice statuses decide, never the stored field)
    updated: str  # UTC ISO timestamp of the task.yaml mtime


@dataclass(frozen=True)
class SliceSummary:
    """One slice row on the task overview page."""

    key: str  # stable slice identity used by dependencies and URLs
    title: str  # human slice name
    kind: str  # slice kind from the profile catalog
    status: str  # slice lifecycle status
    goal: str  # bounded completion outcome


@dataclass(frozen=True)
class BundleHandle:
    """An opened bundle: validated paths plus the parsed task mapping."""

    slug: str  # validated bundle slug
    root: Path  # bundle root directory (<root>/task.yaml lives here)
    task: dict  # parsed task mapping (already validated by the harness store)
    status: str  # derived overall status, same source as BundleSummary.status
    slices: tuple[SliceSummary, ...] = field(default_factory=tuple)
    files: tuple[tuple[str, bool], ...] = field(default_factory=tuple)
    # full `files` listing: (display path, browsable) pairs; display is
    # bundle-relative when inside the root, absolute otherwise


class BundleNotFound(FileNotFoundError):
    """A slug, slice, or file the bundles do not contain (maps to HTTP 404)."""


TERMINAL_STATUSES = frozenset({"done", "skipped"})  # slices needing no further work

STATUS_RANK = {
    "in_progress": 0,  # running work first ("started" in operator terms)
    "planned": 1,
    "blocked": 2,
    "done": 3,
    "skipped": 4,
}  # default table order; unknown statuses sort last


def topo_order(keys: list[str], edges: dict[str, list[str]]) -> list[str]:
    """Delivery order of slice keys: dependencies before dependents.

    Kahn's algorithm over dependency → dependent edges, stable by input
    order. Unknown dependency targets are ignored. Cycles cannot block the
    page: leftover keys append in input order.
    """
    order_index = {key: i for i, key in enumerate(keys)}
    dependents: dict[str, list[str]] = {key: [] for key in keys}
    pending: dict[str, int] = {key: 0 for key in keys}
    for key in keys:
        for dep in edges.get(key, []):
            if dep in pending and dep != key:
                dependents[dep].append(key)
                pending[key] += 1
    ready = sorted([key for key in keys if pending[key] == 0], key=order_index.get)
    ordered: list[str] = []
    while ready:
        key = ready.pop(0)
        ordered.append(key)
        for dependent in sorted(dependents[key], key=order_index.get):
            pending[dependent] -= 1
            if pending[dependent] == 0:
                ready.append(dependent)
        ready.sort(key=order_index.get)
    ordered.extend(key for key in keys if key not in ordered)
    return ordered


def slice_edges(slices: list[dict]) -> dict[str, list[str]]:
    """depends_on lists keyed by slice key for one task."""
    return {str(s.get("key")): [str(d) for d in (s.get("depends_on") or [])] for s in slices}


def status_rank(status: str) -> int:
    """Default table position of a status value; unknown statuses sort last."""
    return STATUS_RANK.get(status, 5)


def claim_recency(task: dict, key: str) -> str:
    """Newest claim heartbeat for a slice, else empty.

    Reads orchestration.cursors[] last_seen for this slice key.
    A live claim beats execution timestamps as the freshness signal.
    """
    orchestration = task.get("orchestration") or {}
    seen = [
        str(cursor.get("last_seen") or "")
        for cursor in (orchestration.get("cursors") or [])
        if str(cursor.get("slice")) == key
    ]
    seen = [value for value in seen if value]
    return max(seen) if seen else ""


def ordered_slices(task: dict) -> list[dict]:
    """Slice mappings in display order: status rank, then freshest first.

    Freshness prefers the claim heartbeat, then execution activity.
    Ties break by delivery order. Stable throughout.
    """
    raw = list((task.get("plan") or {}).get("slices") or [])
    topo_index = {key: i for i, key in enumerate(topo_order([str(s.get("key")) for s in raw], slice_edges(raw)))}
    fresh = {
        str(s.get("key")): claim_recency(task, str(s.get("key"))) or slice_activity(s) for s in raw
    }
    by_time = sorted(
        raw,
        key=lambda s: (fresh[str(s.get("key"))], -topo_index.get(str(s.get("key")), 0)),
        reverse=True,
    )
    return sorted(
        by_time,
        key=lambda s: (status_rank(effective_status(s)), 0 if fresh[str(s.get("key"))] else 1),
    )


def effective_status(sl: dict) -> str:
    """Ordering status of a slice mapping.

    A non-terminal slice with running steps counts as in_progress even when
    the stored slice status still says planned. Terminal and blocked slice
    statuses always win; display still shows the stored value.
    """
    status = str(sl.get("status") or "")
    if status in TERMINAL_STATUSES or status == "blocked":
        return status
    step_statuses = {
        str(st.get("status") or "")
        for st in (*(sl.get("steps") or []), *(sl.get("final_steps") or []))
    }
    if "in_progress" in step_statuses:
        return "in_progress"
    if "blocked" in step_statuses:
        return "blocked"
    return status or "planned"


def slice_activity(sl: dict) -> str:
    """Latest activity timestamp of a slice: own execution, else any step execution."""
    seen: list[str] = []
    for node in (sl, *(sl.get("steps") or []), *(sl.get("final_steps") or [])):
        execution = node.get("execution") or {}
        for moment in ("ended", "started"):
            value = str(execution.get(moment) or "")
            if value:
                seen.append(value)
    return max(seen) if seen else ""


def _harness():
    """Import harness collaborators lazily so `--help` never pays import cost.

    The viewer ships in its own isolated tool venv, so the harness is usually
    not importable there. Fall back to the applied harness tree before import.
    """
    import os
    import sys

    try:
        from pi_job_harness.app import (
            SliceDependencyMermaid,
            derived_task_status,
            is_task_slug,
            task_tasks_home,
        )
    except ModuleNotFoundError:
        candidates = []
        override = os.environ.get("PI_JOB_HARNESS_PATH")
        if override:
            candidates.append(Path(override).expanduser())
        candidates.append(Path.home() / ".local" / "share" / "pi-job-harness")
        for candidate in candidates:
            if (candidate / "pyproject.toml").is_file():
                if str(candidate) not in sys.path:
                    sys.path.insert(0, str(candidate))
                break
        from pi_job_harness.app import (
            SliceDependencyMermaid,
            derived_task_status,
            is_task_slug,
            task_tasks_home,
        )
    from pi_job_harness.layout import PiJobLayout
    from pi_job_harness.store.factory import open_task_store
    return SliceDependencyMermaid, derived_task_status, is_task_slug, task_tasks_home, PiJobLayout, open_task_store


def tasks_home() -> Path:
    """Resolve the central task home from the environment (PI_JOB_TASKS wins)."""
    _, _, _, task_tasks_home, PiJobLayout, _ = _harness()
    return task_tasks_home(PiJobLayout.from_environ())


def _mtime_iso(path: Path) -> str:
    """UTC ISO timestamp of a file mtime (readable fallback, never throws)."""
    try:
        return datetime.fromtimestamp(path.stat().st_mtime, tz=UTC).isoformat(timespec="seconds")
    except OSError:
        return ""


def list_bundles(home: Path | None = None) -> list[BundleSummary]:
    """List every bundle directory holding a task.yaml, newest first.

    Unreadable bundles are skipped with a warning so one broken
    bundle never blanks the whole list page.
    """
    _, derived_task_status, _, _, PiJobLayout, open_task_store = _harness()
    root = home or tasks_home()
    found: list[BundleSummary] = []
    if not root.is_dir():
        return found
    for child in sorted(root.iterdir(), key=lambda p: p.name):
        doc = child / "task.yaml"
        if not child.is_dir() or not doc.is_file():
            continue
        try:
            store = open_task_store(doc, PiJobLayout.from_environ())
            with _quiet_read(store):
                task = store.read()
            title = str(task.get("title") or child.name)
            status = str(derived_task_status(task))
            found.append(BundleSummary(slug=child.name, title=title, status=status, updated=_mtime_iso(doc)))
        # Accepted trade-off: one broken bundle skips with a warning instead of
        # blanking the list. Narrow to data and I/O errors so programming errors
        # and harness misconfiguration still surface as 500s.
        except (OSError, ValueError, KeyError) as exc:
            log.warning("viewer-skip-bundle", extra={"slug": child.name, "error": str(exc)})
    found.sort(key=lambda b: b.updated, reverse=True)
    return found


def open_bundle(slug: str, home: Path | None = None) -> BundleHandle:
    """Open one bundle by slug, fail closed on unknown slugs and traversal.

    Raises BundleNotFound for malformed or missing slugs.
    """
    _, derived_task_status, is_task_slug, _, PiJobLayout, open_task_store = _harness()
    if not is_task_slug(slug):
        log.warning("viewer-bad-slug", extra={"slug": slug})
        raise BundleNotFound(f"unknown bundle: {slug!r}")
    root = (home or tasks_home()) / slug
    doc = root / "task.yaml"
    if not doc.is_file():
        log.warning("viewer-missing-bundle", extra={"slug": slug})
        raise BundleNotFound(f"unknown bundle: {slug!r}")
    task_store = open_task_store(doc, PiJobLayout.from_environ())
    with _quiet_read(task_store):
        task = task_store.read()
    slices = tuple(
        SliceSummary(
            key=str(s.get("key")),
            title=str(s.get("title") or s.get("key")),
            kind=str(s.get("kind") or ""),
            status=str(s.get("status") or ""),
            goal=str(s.get("goal") or ""),
        )
        for s in ((task.get("plan") or {}).get("slices") or [])
    )
    files = bundle_files(task_store, task, root)
    return BundleHandle(
        slug=slug, root=root, task=task, status=str(derived_task_status(task)), slices=slices, files=files
    )


def bundle_files(task_store, task: dict, root: Path) -> tuple[tuple[str, bool], ...]:
    """Full `files` listing for a bundle: (display path, browsable) pairs.

    Reuses TaskFileListing (references plus plans plus registered artifacts
    plus maintain URIs). In-bundle paths show bundle-relative and open in the
    viewer; out-of-bundle paths show absolute with a copy button instead of
    a link, since the viewer only serves inside the root.
    """
    _harness()  # ensures the harness import path before the direct import below
    from pi_job_harness.app import TaskFileListing

    try:
        listing = TaskFileListing.from_store(task_store, task)
    except (OSError, ValueError, KeyError):
        # Listing is navigation only; a broken artifact entry must not break the page.
        log.warning("viewer-files-fallback", extra={"slug": root.name})
        return ()
    root_resolved = root.resolve()
    pairs = []
    for path in listing.paths:
        try:
            pairs.append((path.relative_to(root_resolved).as_posix(), True))
        except ValueError:
            pairs.append((str(path), False))
    return tuple(pairs)


def slice_detail(handle: BundleHandle, key: str) -> dict:
    """Return the raw slice mapping for a key, or raise BundleNotFound."""
    for s in (handle.task.get("plan") or {}).get("slices") or []:
        if str(s.get("key")) == key:
            return s
    log.warning("viewer-missing-slice", extra={"slug": handle.slug, "slice": key})
    raise BundleNotFound(f"unknown slice: {key!r} in bundle {handle.slug!r}")


def bundle_file(handle: BundleHandle, *parts: str) -> Path:
    """Resolve a plans/ or references/ file inside the bundle root.

    The first part names the section (`plans` or `references`); the resolved
    file must stay under that section directory, so `plans/../task.yaml`
    shares the same 404 as a missing file.
    Returns the path only when it stays inside the section and is a file.
    Raises BundleNotFound otherwise (missing file and traversal share
    one 404 so callers never branch on attacker input).
    """
    if not parts:
        raise BundleNotFound("unknown file: ''")
    candidate = handle.root.joinpath(*parts)
    try:
        resolved = candidate.resolve()
        section = (handle.root / parts[0]).resolve()
    except OSError:
        raise BundleNotFound(f"unknown file: {'/'.join(parts)!r}") from None
    if section not in (*resolved.parents, resolved) or not resolved.is_file():
        log.warning("viewer-bad-file", extra={"slug": handle.slug, "path": "/".join(parts)})
        raise BundleNotFound(f"unknown file: {'/'.join(parts)!r}")
    return resolved


def decision_records(handle: BundleHandle) -> list:
    """Current decisions with spill bodies resolved, newest first.

    Reuses DecisionIndex (SUPERSEDES filtering plus spill-body resolve);
    the viewer never parses spill files or SUPERSEDES itself.
    """
    _harness()  # ensures the harness import path before the direct import below
    from pi_job_harness.decision_index import DecisionIndex

    records = DecisionIndex.load(handle.task.get("decisions") or [], handle.root).current()
    return sorted(records, key=lambda r: r.date, reverse=True)


def file_frontmatter_slices(path: Path, *, max_bytes: int = 200 * 1024) -> list[str]:
    """Explicit slice links from a file's YAML frontmatter `slices:` list.

    Convention (documented in the viewer README): a leading `---` block with
    `slices: [key]` claims the file for those slices. Malformed or missing
    frontmatter yields no links, never an error.
    """
    try:
        if path.stat().st_size > max_bytes:
            return []
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return []
    if not text.startswith("---"):
        return []
    end = text.find("\n---", 3)
    if end < 0:
        return []
    try:
        data = yaml.safe_load(text[3:end]) or {}
    except yaml.YAMLError:
        return []
    if not isinstance(data, dict):
        return []
    raw = data.get("slices") or []
    if isinstance(raw, str):
        raw = [raw]
    return [str(item) for item in raw if isinstance(item, (str, int)) and str(item)]


def slice_references(root: Path, key: str, *, max_bytes: int = 200 * 1024) -> list[str]:
    """Reference files relevant to a slice: explicit frontmatter links first.

    Explicit `slices:` frontmatter matches lead; the name-or-content mention
    heuristic fills the rest. Skips oversized files instead of loading them whole.
    """
    key = str(key)
    explicit: list[str] = []
    heuristic: list[str] = []
    references = root / "references"
    if not references.is_dir() or not key:
        return []
    for path in sorted(references.rglob("*")):
        if not path.is_file():
            continue
        rel = f"references/{path.relative_to(references).as_posix()}"
        if key in file_frontmatter_slices(path, max_bytes=max_bytes):
            explicit.append(rel)
            continue
        if key in path.name:
            heuristic.append(rel)
            continue
        try:
            if path.stat().st_size > max_bytes:
                continue
            if key in path.read_text(encoding="utf-8", errors="replace"):
                heuristic.append(rel)
        except OSError:
            continue
    return explicit + [rel for rel in heuristic if rel not in explicit]


def unlinked_references(root: Path, *, max_bytes: int = 200 * 1024) -> list[str]:
    """Reference files claiming no slice via frontmatter `slices:`.

    Powers the Unlinked section that drives agents to link their files.
    Heuristic-only matches still count as unlinked: only explicit links clear it.
    """
    unlinked: list[str] = []
    references = root / "references"
    if not references.is_dir():
        return unlinked
    for path in sorted(references.rglob("*")):
        if not path.is_file():
            continue
        if not file_frontmatter_slices(path, max_bytes=max_bytes):
            unlinked.append(f"references/{path.relative_to(references).as_posix()}")
    return unlinked


def bundle_stats(handle: BundleHandle) -> str:
    """Stats markdown for a bundle: the same body `pi-job stats` prints.

    Reuses the harness stats builder and Markdown renderer verbatim.
    """
    _harness()  # ensures the harness import path before the direct import below
    from pi_job_harness.stats import DEFAULT_WAIT_KEYS, build_stats
    from pi_job_harness.stats import render_markdown as render_stats_markdown

    payload = build_stats(handle.task, handle.slug, frozenset(DEFAULT_WAIT_KEYS))
    return render_stats_markdown(payload)


def dependency_graph(handle: BundleHandle) -> str:
    """Mermaid flowchart of slice depends_on edges for this bundle.

    Left-to-right: wide dependency graphs stay readable where top-down
    stacks past the zoom limit. The harness owns TD rendering; the viewer
    only flips the direction line (no harness change).
    """
    _harness()  # ensures the harness import path before the direct import below
    from pi_job_harness.app import SliceDependencyMermaid

    graph = SliceDependencyMermaid().render(handle.task)
    return graph.replace("flowchart TD", "flowchart LR", 1)
