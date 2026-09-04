"""Open a TaskStore from a path and copy task state between backends."""

from __future__ import annotations

from pathlib import Path
from typing import NoReturn

from pi_job_harness.errors import die
from pi_job_harness.layout import PiJobLayout
from pi_job_harness.store.fs import FsTaskStore
from pi_job_harness.store.protocol import TaskStore
from pi_job_harness.store.yaml import BundleTaskLayout, YamlTaskLayout, YamlTaskStore
from pi_job_harness.store.yaml_io import validate_task_mapping


def unsupported_storage(task_arg: Path) -> NoReturn:
    die(f"unsupported task storage for {task_arg}; use a .yaml/.yml file or a directory")


def layout_for_document_path(doc: Path) -> YamlTaskLayout | BundleTaskLayout:
    """Return the read layout that matches a task document path.

    A document literally named `task.yaml` lives in a bundle root (`plans/`,
    `references/` as siblings of the bundle root, not of a `<stem>`); anything
    else is a loose YAML file using the sibling `<stem>.plans/` layout.
    """
    if doc.name == BundleTaskLayout.DOCUMENT_NAME:
        return BundleTaskLayout(doc.parent)
    return YamlTaskLayout(doc)


def _bundle_root_for(task_arg: Path) -> Path | None:
    """Return the bundle root if `task_arg` is a bundle dir or its `task.yaml`, else None."""
    if task_arg.is_dir():
        return task_arg if (task_arg / BundleTaskLayout.DOCUMENT_NAME).is_file() else None
    if task_arg.name == BundleTaskLayout.DOCUMENT_NAME:
        return task_arg.parent
    return None


def open_task_store(task_arg: Path, layout: PiJobLayout) -> TaskStore:
    """Select a bundle, loose YAML, or existing directory task backend.

    Bundle detection (a directory containing `task.yaml`, or a path to that
    `task.yaml` itself) runs before the `FsTaskStore` fallback so bundle
    directories are never opened as the experimental directory store.
    """

    bundle_root = _bundle_root_for(task_arg)
    if bundle_root is not None:
        return YamlTaskStore(BundleTaskLayout(bundle_root), layout)
    suffix = task_arg.suffix.lower()
    if suffix in {".yaml", ".yml"}:
        return YamlTaskStore(YamlTaskLayout(task_arg), layout)
    if task_arg.is_dir():
        return FsTaskStore(task_arg)
    unsupported_storage(task_arg)


def project(src: TaskStore, dst: TaskStore) -> None:
    """Copy a task's full state between YAML and directory-backed TaskStores."""
    task = src.read()
    validate_task_mapping(task, source=src.describe())

    if isinstance(dst, YamlTaskStore):
        dst.replace(task)
        return

    dst.init_task(
        title=task.get("title", ""),
        status=task.get("status", ""),
        source=task.get("source") or {},
        project_info=task.get("project") or {},
        context=task.get("context", ""),
    )

    orch = task.get("orchestration")
    if orch:
        # Owned cursors are a YAML-only feature (like block-slice/add-finding); a non-YAML
        # destination starts with no claims regardless of what src had claimed.
        dst.init_orchestration()
        for key, artifact in (orch.get("artifacts") or {}).items():
            dst.write_artifact(
                key, status=artifact.get("status", "planned"), path=artifact.get("path"), note=artifact.get("note", "")
            )

    for decision in task.get("decisions", []):
        dst.add_decision(date=decision.get("date", ""), note=decision.get("note", ""), source=decision.get("source", ""))

    plan = task.get("plan") or {}
    dst.set_plan_note(plan.get("note", ""))

    prev_slice_key: str | None = None
    for sl in plan.get("slices", []):
        extra_fields: dict[str, list[str]] = {}
        if sl.get("repos"):
            extra_fields["repos"] = sl["repos"]
        if sl.get("depends_on"):
            extra_fields["depends_on"] = sl["depends_on"]

        dst.add_slice(
            key=sl["key"],
            kind=str(sl.get("kind") or "implement"),
            title=sl.get("title", ""),
            goal=sl.get("goal", ""),
            status=sl.get("status", "planned"),
            note=sl.get("note", ""),
            extra_fields=extra_fields,
            steps=[],
            final_steps=[],
            after=prev_slice_key,
        )
        prev_slice_key = sl["key"]

        prev_step_key: str | None = None
        for step in sl.get("steps", []):
            dst.add_step(
                slice_key=sl["key"], key=step["key"], title=step.get("title", ""), note=step.get("note", ""),
                status=step.get("status", "planned"), terminal=False, after=prev_step_key,
            )
            if step.get("execution"):
                dst.set_execution(
                    slice_key=sl["key"], step_key=step["key"], status=step.get("status", "planned"),
                    note=step.get("note", ""), execution=step["execution"],
                )
            prev_step_key = step["key"]

        prev_final_step_key: str | None = None
        for step in sl.get("final_steps", []):
            dst.add_step(
                slice_key=sl["key"], key=step["key"], title=step.get("title", ""), note=step.get("note", ""),
                status=step.get("status", "planned"), terminal=True, after=prev_final_step_key,
            )
            if step.get("execution"):
                dst.set_execution(
                    slice_key=sl["key"], step_key=step["key"], status=step.get("status", "planned"),
                    note=step.get("note", ""), execution=step["execution"],
                )
            prev_final_step_key = step["key"]

        for repo, rw in (sl.get("repo_work") or {}).items():
            if rw.get("worktree"):
                dst.set_worktree(slice_key=sl["key"], repo=repo, path=rw["worktree"])
            for pr in rw.get("prs") or []:
                dst.add_pr(
                    slice_key=sl["key"], repo=repo, url=pr.get("url", ""), status=pr.get("status", "open"),
                    note=pr.get("note", ""),
                )
        if sl.get("execution"):
            dst.set_execution(
                slice_key=sl["key"], step_key=None, status=sl.get("status", "planned"),
                note=sl.get("note", ""), execution=sl["execution"],
            )

