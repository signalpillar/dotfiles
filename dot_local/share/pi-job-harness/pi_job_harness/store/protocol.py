"""TaskStore protocol shared by YAML and directory backends."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any, Protocol


class TaskStore(Protocol):
    """Protocol for YAML and directory-backed task storage backends."""

    def describe(self) -> str:
        """Return a human-readable description of the store (e.g., file path)."""
        ...

    def read(self) -> dict[str, Any]:
        """Read and export the task as a dict."""
        ...

    def init_orchestration(self) -> None:
        """Initialize orchestration with an empty owned-cursor list and default policy.
        No cursor is seeded; agents claim a Ready slice explicitly (`pi-job claim`)."""
        ...

    def init_task(
        self,
        *,
        title: str,
        status: str,
        source: dict[str, str],
        project_info: dict[str, str],
        context: str,
    ) -> None:
        """Set the task's top-level identity fields (everything outside
        orchestration/decisions/plan). Requires the destination to already exist in
        some minimal form (a YAML file destination or an existing FsTaskStore
        directory)."""
        ...

    def set_plan_note(self, note: str) -> None:
        """Set task.plan.note."""
        ...

    def add_decision(self, *, date: str, note: str, source: str) -> None:
        """Append a decision record."""
        ...

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
        """Add a new slice to the task plan. extra_fields maps a non-baseline
        #Slice field name to its plain string values - backend-neutral, no
        pre-rendered backend-specific syntax. steps/final_steps are
        [(step_key, title), ...] tuples. layer binds implement/spike/research
        when task.layers is non-empty."""
        ...

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
        """Add a new step to a slice (terminal=True for final_steps, False for steps)."""
        ...

    def set_step_status(
        self, *, slice_key: str, step_key: str, status: str, note: str
    ) -> None:
        """Set a step's status and note (used by advance --force to record a skip)."""
        ...

    def set_execution(
        self,
        *,
        slice_key: str,
        step_key: str | None,
        status: str,
        note: str,
        execution: dict[str, str],
    ) -> None:
        """Persist lifecycle state for a slice or one of its steps."""
        ...

    def set_worktree(self, *, slice_key: str, repo: str, path: str) -> None:
        """Set the worktree path for a slice's repo work."""
        ...

    def clear_worktree(self, *, slice_key: str, repo: str) -> None:
        """Remove the worktree path for an existing slice repo entry."""
        ...

    def add_pr(
        self, *, slice_key: str, repo: str, url: str, status: str, note: str
    ) -> str:
        """Add or update a PR for a slice's repo work. Returns 'added' or 'updated'."""
        ...

    def write_artifact(
        self, key: str, *, status: str, path: str | None, note: str
    ) -> None:
        """Record a toolbelt artifact."""
        ...

    def set_project(self, fields: Mapping[str, str]) -> None:
        """Update task.project fields (merge, not replace)."""
        ...

    def set_title(self, title: str) -> None:
        """Replace task.title."""
        ...

    def set_context(self, context: str) -> None:
        """Replace task.context."""
        ...

    def remove_slice(self, *, key: str) -> None:
        """Remove a slice from the task plan. Must validate that no other slice
        depends on it and that no owned cursor claims it before removing."""
        ...

    def set_slice_fields(
        self,
        *,
        slice_key: str,
        title: str | None = None,
        goal: str | None = None,
        layer: str | None = None,
        clear_layer: bool = False,
    ) -> None:
        """Update a slice's title, goal, and/or layer binding."""
        ...

    def set_slice_status(
        self, *, slice_key: str, status: str, note: str | None = None
    ) -> None:
        """Update a slice's status and optionally its note."""
        ...

    def acknowledge_edit(self, *, reason: str, slice_key: str) -> None:
        """Refresh content digest and append the reason to the given slice's note."""
        ...




