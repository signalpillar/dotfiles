"""YAML task layouts and YamlTaskStore."""

from __future__ import annotations

import fcntl
from collections.abc import Callable, Iterator, Mapping
from contextlib import contextmanager
from pathlib import Path
from typing import Any

from pi_job_harness.errors import die
from pi_job_harness.store.text import merge_note, utc_now
from pi_job_harness.store.yaml_io import (
    atomic_create_text,
    atomic_write_text,
    canonical_task_mapping,
    is_content_dirty,
    load_yaml_mapping,
    render_yaml_task,
    set_content_digest,
    warn_if_content_dirty,
    yaml_task_lock_path,
)


def slice_kind_requires_plan_file(kind: str) -> bool:
    from pi_job_harness.app import slice_kind_requires_plan_file as impl

    return impl(kind)


def render_slice_plan_stub(*, key: str, goal: str, depends_on: list[str]) -> str:
    from pi_job_harness.app import render_slice_plan_stub as impl

    return impl(key=key, goal=goal, depends_on=depends_on)


def make_layer_entry(*, name: str, description: str, references: list[str] | None = None) -> dict[str, Any]:
    from pi_job_harness.app import make_layer_entry as impl

    return impl(name=name, description=description, references=references)


def layers_with_added(layers: list[Any], entry: dict[str, Any], *, after: str | None) -> list[Any]:
    from pi_job_harness.app import layers_with_added as impl

    return impl(layers, entry, after=after)


def layers_with_updated(
    layers: list[Any],
    *,
    name: str,
    description: str | None = None,
    references: list[str] | None = None,
) -> list[Any]:
    from pi_job_harness.app import layers_with_updated as impl

    return impl(layers, name=name, description=description, references=references)


def slice_keys_bound_to_layer(task: Mapping[str, Any], name: str) -> tuple[str, ...]:
    from pi_job_harness.app import slice_keys_bound_to_layer as impl

    return impl(task, name)


def layers_without(layers: list[Any], *, name: str) -> list[Any]:
    from pi_job_harness.app import layers_without as impl

    return impl(layers, name=name)


def layers_with_renamed(layers: list[Any], *, old: str, new: str) -> list[Any]:
    from pi_job_harness.app import layers_with_renamed as impl

    return impl(layers, old=old, new=new)


def slices_with_layer_renamed(slices: list[Any], *, old: str, new: str) -> list[Any]:
    from pi_job_harness.app import slices_with_layer_renamed as impl

    return impl(slices, old=old, new=new)


def layers_reordered(layers: list[Any], names: list[str]) -> list[Any]:
    from pi_job_harness.app import layers_reordered as impl

    return impl(layers, names)


def load_profile_contract() -> dict[str, Any]:
    from pi_job_harness.profile import load_profile_contract as impl

    return impl()


def render_finding_entry(*, note: str, source: str, stamp: str) -> str:
    from pi_job_harness.app import render_finding_entry as impl

    return impl(note=note, source=source, stamp=stamp)


class YamlTaskLayout:
    """Sibling filesystem layout for a loose YAML task file.

    Owns paths under `<task-stem>.plans/` for store-managed sibling artifacts.
    """

    FINDINGS_NAME = "_findings.md"

    def __init__(self, task_path: Path) -> None:
        self.task_path = task_path

    @property
    def document_path(self) -> Path:
        return self.task_path

    @property
    def plans_dir(self) -> Path:
        return self.task_path.parent / f"{self.task_path.stem}.plans"

    @property
    def plans_pointer(self) -> str:
        return f"{self.task_path.stem}.plans"

    def findings_file(self) -> Path:
        return self.plans_dir / self.FINDINGS_NAME

    def findings_pointer(self) -> str:
        """Task-dir-relative pointer for CLI tips and packets."""
        return f"{self.task_path.stem}.plans/{self.FINDINGS_NAME}"

    def slice_plan_file(self, slice_key: str) -> Path:
        return self.plans_dir / f"{slice_key}.md"

    def slice_plan_pointer(self, slice_key: str) -> str:
        return f"Plan file: {self.task_path.stem}.plans/{slice_key}.md"

    def decision_spill_file(self, *, date: str, stamp: str) -> Path:
        """Pure path for a spilled decision body. Caller supplies stamp (no clock here)."""
        return self.plans_dir / f"_decision-{date}-{stamp}.md"

    def describe_store(self) -> str:
        return f"YAML task file {self.task_path}"


class BundleTaskLayout:
    """Layout for a `$PI_JOB_TASKS/<slug>/` task bundle directory.

    Bundle document lives at `<root>/task.yaml` (exact name, not `.yml`).
    Plans/findings/decision spills live under `<root>/plans/` (not
    `<stem>.plans/`); `<root>/references/` is reserved bundle metadata for
    later slices.
    """

    DOCUMENT_NAME = "task.yaml"
    FINDINGS_NAME = "_findings.md"

    def __init__(self, bundle_root: Path) -> None:
        self.bundle_root = bundle_root

    @property
    def document_path(self) -> Path:
        return self.bundle_root / self.DOCUMENT_NAME

    @property
    def plans_dir(self) -> Path:
        return self.bundle_root / "plans"

    @property
    def plans_pointer(self) -> str:
        return "plans"

    @property
    def references_dir(self) -> Path:
        return self.bundle_root / "references"

    def findings_file(self) -> Path:
        return self.plans_dir / self.FINDINGS_NAME

    def findings_pointer(self) -> str:
        """Task-dir-relative pointer for CLI tips and packets."""
        return f"plans/{self.FINDINGS_NAME}"

    def slice_plan_file(self, slice_key: str) -> Path:
        return self.plans_dir / f"{slice_key}.md"

    def slice_plan_pointer(self, slice_key: str) -> str:
        return f"Plan file: plans/{slice_key}.md"

    def decision_spill_file(self, *, date: str, stamp: str) -> Path:
        """Pure path for a spilled decision body. Caller supplies stamp (no clock here)."""
        return self.plans_dir / f"_decision-{date}-{stamp}.md"

    def describe_store(self) -> str:
        return f"task bundle {self.bundle_root}"


class YamlTaskStore:
    """Preferred TaskStore implementation using one machine-owned YAML document.

    The layout owns document location, plans-dir arithmetic, and describe
    wording; the store only performs I/O against `layout.document_path`.
    """

    def __init__(self, layout: YamlTaskLayout | BundleTaskLayout, *, create_only: bool = False) -> None:
        self.layout = layout
        self.create_only = create_only
        self._lock_depth = 0
        self._digest_warn_suppressed = 0

    @property
    def path(self) -> Path:
        """Read-only compat alias for `layout.document_path`."""
        return self.layout.document_path

    def describe(self) -> str:
        return self.layout.describe_store()

    @contextmanager
    def _suppress_digest_warn(self) -> Iterator[None]:
        self._digest_warn_suppressed += 1
        try:
            yield
        finally:
            self._digest_warn_suppressed -= 1

    def read(self) -> dict[str, Any]:
        data = load_yaml_mapping(self.path, label="task file")
        task = canonical_task_mapping(data, source=str(self.path))
        if self._digest_warn_suppressed == 0:
            warn_if_content_dirty(task, self.path)
        return task

    def _write_validated(self, task: Mapping[str, Any]) -> None:
        """Atomically write a validated task document without changing content_digest."""

        content = render_yaml_task(task, source=str(self.path))
        if self.create_only:
            atomic_create_text(self.path, content)
            self.create_only = False
        else:
            atomic_write_text(self.path, content)

    def replace(self, task: Mapping[str, Any]) -> None:
        """Replace the complete task after validation, using one atomic write."""

        task_dict = task if isinstance(task, dict) else canonical_task_mapping(task, source=str(self.path))
        set_content_digest(task_dict)
        self._write_validated(task_dict)

    @contextmanager
    def exclusive(self) -> Iterator[None]:
        """Hold the task's advisory lock; nested mutations reuse the outer lock."""

        if self._lock_depth:
            self._lock_depth += 1
            try:
                yield
            finally:
                self._lock_depth -= 1
            return

        lock_path = yaml_task_lock_path(self.path)
        lock_path.parent.mkdir(parents=True, exist_ok=True)
        with lock_path.open("a+", encoding="utf-8") as lock:
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
            self._lock_depth = 1
            try:
                yield
            finally:
                self._lock_depth = 0

    def _mutate(
        self,
        mutation: Callable[[dict[str, Any]], None],
        *,
        refresh_digest: bool = False,
    ) -> None:
        with self.exclusive():
            with self._suppress_digest_warn():
                task = self.read()
            dirty = is_content_dirty(task)
            mutation(task)
            if refresh_digest or not dirty:
                set_content_digest(task)
            self._write_validated(task)

    def acknowledge_edit(self, *, reason: str, slice_key: str) -> None:
        """Refresh content digest and append the reason to the given slice's note."""

        def mutation(task: dict[str, Any]) -> None:
            task_slice = self._slice(task, slice_key)
            entry = f"Hand-edit acknowledged: {reason}"
            task_slice["note"] = merge_note(str(task_slice.get("note") or ""), entry, replace=False)

        self._mutate(mutation, refresh_digest=True)

    @staticmethod
    def _slice(task: Mapping[str, Any], slice_key: str) -> dict[str, Any]:
        for task_slice in task.get("plan", {}).get("slices", []):
            if task_slice.get("key") == slice_key:
                return task_slice
        die(f"slice not found in YAML task: {slice_key!r}")

    @classmethod
    def _step(cls, task: Mapping[str, Any], slice_key: str, step_key: str) -> dict[str, Any]:
        task_slice = cls._slice(task, slice_key)
        for step in [*task_slice.get("steps", []), *task_slice.get("final_steps", [])]:
            if step.get("key") == step_key:
                return step
        die(f"step not found in YAML task: {slice_key!r}/{step_key!r}")

    def init_orchestration(self) -> None:
        def mutation(task: dict[str, Any]) -> None:
            orchestration = task.setdefault("orchestration", {})
            orchestration.setdefault("cursors", [])
            orchestration.setdefault(
                "policy",
                {"coding_execution": {
                    "subagent_required": True,
                    "lower_power_model_preferred": True,
                    "orchestrator_reviews_subagent": True,
                    "exceptions": [],
                }},
            )
            orchestration.setdefault("artifacts", {})

        self._mutate(mutation)

    def claim_slice(self, *, owner: str, slice_key: str, now: str) -> None:
        """Upsert an owned cursor: drops any prior claim by this owner or on this
        slice (stale-displacement is decided by the caller before this is invoked;
        this is the unconditional write half of `claim`)."""

        def mutation(task: dict[str, Any]) -> None:
            orchestration = task.get("orchestration")
            if not orchestration:
                die("task has no orchestration block (run create first)")
            cursors = orchestration.setdefault("cursors", [])
            cursors[:] = [
                c for c in cursors if c.get("owner") != owner and c.get("slice") != slice_key
            ]
            cursors.append({"owner": owner, "slice": slice_key, "claimed_at": now, "last_seen": now})

        self._mutate(mutation)

    def release_claim(self, *, owner: str) -> bool:
        """Drop owner's claim, if any. Returns True when a claim was removed."""
        removed = False

        def mutation(task: dict[str, Any]) -> None:
            nonlocal removed
            orchestration = task.get("orchestration") or {}
            cursors = orchestration.get("cursors") or []
            kept = [c for c in cursors if c.get("owner") != owner]
            removed = len(kept) != len(cursors)
            orchestration["cursors"] = kept

        self._mutate(mutation)
        return removed

    def touch_claim(self, *, owner: str, now: str) -> None:
        """Bump last_seen for owner's claim, if any (no-op if the claim is gone)."""

        def mutation(task: dict[str, Any]) -> None:
            orchestration = task.get("orchestration") or {}
            for c in orchestration.get("cursors") or []:
                if c.get("owner") == owner:
                    c["last_seen"] = now
                    return

        self._mutate(mutation)

    def auto_release_slice(self, *, slice_key: str) -> str | None:
        """Drop any claim on slice_key, unconditionally. Returns the released owner,
        if any. Callers check the slice reached a terminal status first."""
        released: str | None = None

        def mutation(task: dict[str, Any]) -> None:
            nonlocal released
            orchestration = task.get("orchestration") or {}
            cursors = orchestration.get("cursors") or []
            kept = []
            for c in cursors:
                if c.get("slice") == slice_key:
                    released = c.get("owner")
                    continue
                kept.append(c)
            orchestration["cursors"] = kept

        self._mutate(mutation)
        return released

    def init_task(
        self,
        *,
        title: str,
        status: str,
        source: dict[str, str],
        project_info: dict[str, str],
        context: str,
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task.update({
                "title": title,
                "status": status,
                "source": source,
                "project": project_info,
                "context": context,
            })

        self._mutate(mutation)

    def set_plan_note(self, note: str) -> None:
        self._mutate(lambda task: task.setdefault("plan", {}).update({"note": note}))

    def add_slice(
        self,
        *,
        key: str,
        kind: str,
        title: str,
        goal: str,
        extra_fields: dict[str, list[str]],
        steps: list[tuple[str, str]],
        final_steps: list[tuple[str, str]],
        after: str | None,
        status: str = "planned",
        note: str = "",
        layer: str | None = None,
    ) -> None:
        task_slice: dict[str, Any] = {
            "key": key,
            "kind": kind,
            "title": title,
            "goal": goal,
            "status": status,
            "note": note,
            **extra_fields,
            "steps": [
                {"key": step_key, "title": step_title, "status": "planned", "note": ""}
                for step_key, step_title in steps
            ],
            "final_steps": [
                {"key": step_key, "title": step_title, "status": "planned", "note": ""}
                for step_key, step_title in final_steps
            ],
        }
        if layer is not None:
            task_slice["layer"] = layer

        def mutation(task: dict[str, Any]) -> None:
            slices = task.setdefault("plan", {}).setdefault("slices", [])
            if after is None:
                slices.append(task_slice)
                return
            for index, existing in enumerate(slices):
                if existing.get("key") == after:
                    slices.insert(index + 1, task_slice)
                    return
            die(f"--after slice not found in YAML task: {after!r}")

        self._mutate(mutation)

    def add_step(
        self,
        *,
        slice_key: str,
        key: str,
        title: str,
        note: str,
        terminal: bool,
        after: str | None,
        status: str = "planned",
    ) -> None:
        new_step = {"key": key, "title": title, "status": status, "note": note}

        def mutation(task: dict[str, Any]) -> None:
            group = "final_steps" if terminal else "steps"
            steps = self._slice(task, slice_key).setdefault(group, [])
            if after is None:
                steps.append(new_step)
                return
            for index, existing in enumerate(steps):
                if existing.get("key") == after:
                    steps.insert(index + 1, new_step)
                    return
            die(f"--after step not found in YAML task: {slice_key!r}/{after!r}")

        self._mutate(mutation)

    def set_step_status(
        self,
        *,
        slice_key: str,
        step_key: str,
        status: str,
        note: str,
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            self._step(task, slice_key, step_key).update({"status": status, "note": note})

        self._mutate(mutation)

    def set_execution(
        self,
        *,
        slice_key: str,
        step_key: str | None,
        status: str,
        note: str,
        execution: dict[str, str],
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            target = (
                self._step(task, slice_key, step_key)
                if step_key is not None
                else self._slice(task, slice_key)
            )
            target.update({"status": status, "note": note, "execution": execution})

        self._mutate(mutation)

    def set_worktree(self, *, slice_key: str, repo: str, path: str) -> None:
        def mutation(task: dict[str, Any]) -> None:
            repo_work = self._slice(task, slice_key).setdefault("repo_work", {})
            repo_work.setdefault(repo, {"prs": []})["worktree"] = path

        self._mutate(mutation)

    def clear_worktree(self, *, slice_key: str, repo: str) -> None:
        def mutation(task: dict[str, Any]) -> None:
            repo_work = self._slice(task, slice_key).get("repo_work") or {}
            if repo not in repo_work:
                die(f"repo work not found: {slice_key}/{repo}")
            repo_work[repo].pop("worktree", None)

        self._mutate(mutation)

    def add_pr(
        self,
        *,
        slice_key: str,
        repo: str,
        url: str,
        status: str,
        note: str,
    ) -> str:
        action = "added"

        def mutation(task: dict[str, Any]) -> None:
            nonlocal action
            repo_work = self._slice(task, slice_key).setdefault("repo_work", {})
            prs = repo_work.setdefault(repo, {}).setdefault("prs", [])
            replacement = {"url": url, "status": status, "note": note}
            for index, pull_request in enumerate(prs):
                if pull_request.get("url") == url:
                    prs[index] = replacement
                    action = "updated"
                    return
            prs.append(replacement)

        self._mutate(mutation)
        return action

    def write_artifact(
        self,
        key: str,
        *,
        status: str,
        path: str | None,
        note: str,
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            orchestration = task.get("orchestration")
            if not orchestration:
                die("cannot register toolbelt aid: task has no orchestration block (run init first)")
            artifact = {"status": status, "note": note}
            if path is not None:
                artifact["path"] = path
            orchestration.setdefault("artifacts", {})[key] = artifact

        self._mutate(mutation)

    def upsert_maintain(self, *, uri: str, note: str) -> str:
        """Add or replace a keep-current surface by uri. Returns 'added' or 'updated'."""
        cleaned_uri = uri.strip()
        cleaned_note = note.strip()
        if not cleaned_uri:
            die("maintain add requires a non-empty --uri")
        if not cleaned_note:
            die("maintain add requires a non-empty --note (what current means / when to update)")
        action = "added"

        def mutation(task: dict[str, Any]) -> None:
            nonlocal action
            orchestration = task.get("orchestration")
            if not orchestration:
                die("cannot register maintain item: task has no orchestration block (run init first)")
            items = list(orchestration.get("maintain") or [])
            replacement = {"uri": cleaned_uri, "note": cleaned_note}
            for index, item in enumerate(items):
                if str(item.get("uri") or "") == cleaned_uri:
                    items[index] = replacement
                    action = "updated"
                    orchestration["maintain"] = items
                    return
            items.append(replacement)
            orchestration["maintain"] = items

        self._mutate(mutation)
        return action

    def remove_maintain(self, *, uri: str) -> None:
        cleaned_uri = uri.strip()
        if not cleaned_uri:
            die("maintain remove requires a non-empty --uri")

        def mutation(task: dict[str, Any]) -> None:
            orchestration = task.get("orchestration")
            if not orchestration:
                die("cannot remove maintain item: task has no orchestration block (run init first)")
            items = list(orchestration.get("maintain") or [])
            kept = [item for item in items if str(item.get("uri") or "") != cleaned_uri]
            if len(kept) == len(items):
                die(f"maintain uri not found: {cleaned_uri!r}")
            orchestration["maintain"] = kept

        self._mutate(mutation)

    def set_project(self, fields: Mapping[str, str]) -> None:
        self._mutate(lambda task: task.setdefault("project", {}).update(fields))

    def set_title(self, title: str) -> None:
        cleaned = title.strip()
        if not cleaned:
            die("title must be non-empty")
        self._mutate(lambda task: task.update({"title": cleaned}))

    def set_context(self, context: str) -> None:
        self._mutate(lambda task: task.update({"context": context}))

    def set_step_note(
        self, *, slice_key: str, step_key: str, note: str, replace: bool
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            step = self._step(task, slice_key, step_key)
            existing = str(step.get("note") or "")
            step["note"] = merge_note(existing, note, replace=replace)

        self._mutate(mutation)

    def set_slice_note(self, *, slice_key: str, note: str, replace: bool) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task_slice = self._slice(task, slice_key)
            existing = str(task_slice.get("note") or "")
            task_slice["note"] = merge_note(existing, note, replace=replace)

        self._mutate(mutation)

    def set_source(self, fields: Mapping[str, str]) -> None:
        self._mutate(lambda task: task.setdefault("source", {}).update(fields))

    def remove_slice(self, *, key: str) -> None:
        def mutation(task: dict[str, Any]) -> None:
            slices = task.get("plan", {}).get("slices", [])
            before = len(slices)
            task["plan"]["slices"] = [s for s in slices if s.get("key") != key]
            if len(task["plan"]["slices"]) == before:
                die(f"slice not found: {key!r}")

        self._mutate(mutation)

    def set_slice_fields(
        self,
        *,
        slice_key: str,
        title: str | None = None,
        goal: str | None = None,
        layer: str | None = None,
        clear_layer: bool = False,
        depends_on: list[str] | None = None,
        clear_depends_on: bool = False,
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task_slice = self._slice(task, slice_key)
            if title is not None:
                task_slice["title"] = title
            if goal is not None:
                task_slice["goal"] = goal
            if clear_layer:
                task_slice.pop("layer", None)
            elif layer is not None:
                task_slice["layer"] = layer
            if clear_depends_on:
                task_slice["depends_on"] = []
            elif depends_on:
                deps = task_slice.setdefault("depends_on", [])
                for dep in depends_on:
                    if dep not in deps:
                        deps.append(dep)

        self._mutate(mutation)

    def set_slice_status(
        self, *, slice_key: str, status: str, note: str | None = None
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task_slice = self._slice(task, slice_key)
            task_slice["status"] = status
            if note is not None:
                task_slice["note"] = note

        self._mutate(mutation)

    def block_slice(
        self, *, slice_key: str, reason: str, gate: str | None = None
    ) -> bool:
        """Block a slice (append reason) and optionally wire depends_on in one mutation.

        Returns True when gate was newly appended to depends_on.
        Task advisory lock also serializes sibling .plans/ writes for this store.
        """
        gate_added = False

        def mutation(task: dict[str, Any]) -> None:
            nonlocal gate_added
            task_slice = self._slice(task, slice_key)
            existing = str(task_slice.get("note") or "")
            task_slice["status"] = "blocked"
            task_slice["note"] = merge_note(existing, reason, replace=False)
            if gate:
                deps = task_slice.setdefault("depends_on", [])
                if gate not in deps:
                    deps.append(gate)
                    gate_added = True

        self._mutate(mutation)
        return gate_added

    def append_slice_depends_on(self, *, slice_key: str, dep_key: str) -> bool:
        """Append dep_key to slice depends_on if missing. Returns True when added."""
        added = False

        def mutation(task: dict[str, Any]) -> None:
            nonlocal added
            task_slice = self._slice(task, slice_key)
            deps = task_slice.setdefault("depends_on", [])
            if dep_key not in deps:
                deps.append(dep_key)
                added = True

        self._mutate(mutation)
        return added

    def ensure_slice_plan_stub(
        self,
        *,
        key: str,
        kind: str,
        goal: str,
        depends_on: list[str] | None = None,
    ) -> Path | None:
        """Create a missing plan stub from the profile template. None if skipped."""
        if not slice_kind_requires_plan_file(kind):
            return None
        path = self.layout.slice_plan_file(key)
        body = render_slice_plan_stub(
            key=key,
            goal=goal,
            depends_on=depends_on or [],
        )
        with self.exclusive():
            if path.exists():
                return None
            # Task lock serializes .plans/ writers; atomic replace avoids partial stubs.
            atomic_write_text(path, body)
        return path
    def add_layer(
        self,
        *,
        name: str,
        description: str,
        references: list[str] | None = None,
        after: str | None = None,
        binds: list[tuple[str, str]] | None = None,
    ) -> None:
        entry = make_layer_entry(name=name, description=description, references=references)

        def mutation(task: dict[str, Any]) -> None:
            task["layers"] = layers_with_added(
                task.get("layers") or [],
                entry,
                after=after,
            )
            for slice_key, layer in binds or []:
                self._slice(task, slice_key)["layer"] = layer

        self._mutate(mutation)

    def set_layer(
        self,
        *,
        name: str,
        description: str | None = None,
        references: list[str] | None = None,
    ) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task["layers"] = layers_with_updated(
                task.get("layers") or [],
                name=name,
                description=description,
                references=references,
            )

        self._mutate(mutation)

    def remove_layer(self, *, name: str) -> None:
        def mutation(task: dict[str, Any]) -> None:
            bound = slice_keys_bound_to_layer(task, name)
            if bound:
                die(
                    f"cannot remove layer {name!r}: still bound by slices "
                    f"{', '.join(bound)}; rebind with set-slice --layer first"
                )
            task["layers"] = layers_without(task.get("layers") or [], name=name)

        self._mutate(mutation)

    def rename_layer(self, *, old: str, new: str) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task["layers"] = layers_with_renamed(
                task.get("layers") or [],
                old=old,
                new=new,
            )
            plan = task.setdefault("plan", {})
            plan["slices"] = slices_with_layer_renamed(
                plan.get("slices") or [],
                old=old,
                new=new,
            )

        self._mutate(mutation)

    def reorder_layers(self, *, names: list[str]) -> None:
        def mutation(task: dict[str, Any]) -> None:
            task["layers"] = layers_reordered(task.get("layers") or [], names)

        self._mutate(mutation)


    def add_decision(
        self,
        *,
        date: str,
        note: str,
        source: str,
        spill_body: str | None = None,
        spill_path: Path | None = None,
    ) -> Path | None:
        """Append a decision. Optional spill_body writes long prose via layout then stores a pointer.

        Returns the spill path when a body was spilled; otherwise None.
        """
        written: Path | None = None
        yaml_note = note
        if spill_body is not None:
            path = spill_path
            if path is None:
                stamp = utc_now().replace(":", "").replace("-", "")[:15]
                path = self.layout.decision_spill_file(date=date, stamp=stamp)
            spill_text = (
                f"# Decision {date}\n\nSource: {source}\n\n{spill_body.rstrip()}\n"
            )
            try:
                rel = path.relative_to(self.path.parent)
            except ValueError:
                rel = path
            yaml_note = f"Plan file: {rel}"
            with self.exclusive():
                atomic_write_text(path, spill_text)

                def mutation(task: dict[str, Any]) -> None:
                    task.setdefault("decisions", []).append(
                        {"date": date, "note": yaml_note, "source": source}
                    )

                # Nested exclusive reuses the outer lock.
                self._mutate(mutation)
            written = path
            return written

        def mutation(task: dict[str, Any]) -> None:
            task.setdefault("decisions", []).append(
                {"date": date, "note": yaml_note, "source": source}
            )

        self._mutate(mutation)
        return None

    def add_finding(self, *, note: str, source: str = "") -> Path:
        """Append RCA/evidence to the layout findings file (not the task YAML).

        Uses the task advisory lock so all `.plans/` writers for this task serialize.
        """
        if not note:
            die("add_finding: note is required")
        path = self.layout.findings_file()
        stamp = utc_now()
        block = render_finding_entry(note=note, source=source, stamp=stamp)
        with self.exclusive():
            if path.exists():
                existing = path.read_text(encoding="utf-8")
                if not existing.endswith("\n"):
                    existing += "\n"
                atomic_write_text(path, existing + block)
            else:
                preamble = str(
                    load_profile_contract()["instruction_packets"]["findings_file_header"]
                ).rstrip()
                atomic_write_text(path, f"{preamble}\n\n{block}")
        return path