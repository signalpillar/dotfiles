"""Persisted task documents and runtime slice/step views.

Shared by the CLI, stats, and report. Profile documents live in profile.py.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from typing import Any, Literal

from pydantic import BaseModel, ConfigDict, Field, model_validator

TaskStatus = Literal["planned", "in_progress", "blocked", "done", "skipped"]
ArtifactStatus = TaskStatus
PullRequestStatus = Literal["open", "merged", "closed"]
SliceKindKey = Literal["setup", "implement", "closing", "research", "spike", "follow-work", "fog"]
ExecutionOwner = Literal["orchestrator", "subagent", "external_tool"]
TASK_STATUSES = ("planned", "in_progress", "blocked", "done", "skipped")
ARTIFACT_STATUSES = TASK_STATUSES
PULL_REQUEST_STATUSES = ("open", "merged", "closed")
# Slice kinds that must bind to exactly one task.layers entry when the registry is non-empty.
LAYERED_SLICE_KINDS = frozenset({"implement", "spike", "research"})
STATUS_DONE = {"done", "skipped"}


class StrictDocument(BaseModel):
    """Base for persisted records: coercion and unknown fields are rejected."""

    model_config = ConfigDict(extra="forbid", strict=True)


class ExecutionDocument(StrictDocument):
    """Provenance for one slice or step execution."""

    model: str = Field(description="Fully qualified provider/model identifier for the executor.")
    started: str = Field(description="UTC ISO 8601 timestamp recorded when execution started.")
    ended: str | None = Field(default=None, description="UTC ISO 8601 completion timestamp, if finished.")


class StepDocument(StrictDocument):
    """One ordered unit of work inside a task slice."""

    key: str = Field(description="Stable step identifier used by cursors and profile lookups.")
    title: str = Field(description="Short human-readable step name.")
    status: TaskStatus = Field(description="Current lifecycle state of the step.")
    note: str = Field(
        description=(
            "Evidence, decision, blocker, or skip reason recorded for the step. "
            "Prefer Markdown; `pi-job markdown` renders notes formatted."
        )
    )
    execution: ExecutionDocument | None = Field(default=None, description="Executor provenance, when recorded.")


class DecisionDocument(StrictDocument):
    """Product/scope decision that later sessions must honor without re-grilling."""

    date: str = Field(description="Decision date, normally formatted as YYYY-MM-DD.")
    note: str = Field(
        description=(
            "Product, scope, architecture, or policy agreement - not step evidence "
            "(use finish --note for e2e, PR, deploy, or progress). "
            "Prefer Markdown; `pi-job markdown` renders it as a blockquote."
        )
    )
    source: str = Field(description="Origin of the decision, such as a chat, issue, or review.")


class LayerDocument(StrictDocument):
    """Ordered business or tech band the task touches (not a repository name)."""

    name: str = Field(description="Stable layer slug used by slices and diagram order.")
    description: str = Field(description="One-line description of what this band covers.")
    references: list[str] = Field(
        default_factory=list,
        description="Optional paths or URLs that ground this layer for agents.",
    )


class ArtifactDocument(StrictDocument):
    """A durable planning or delivery artifact tracked by the harness."""

    status: ArtifactStatus = Field(description="Current lifecycle state of the artifact.")
    path: str | None = Field(default=None, description="Repository-relative artifact path, when applicable.")
    note: str = Field(description="Evidence, rationale, or an explicit reason the artifact was skipped.")


class MaintainItemDocument(StrictDocument):
    """A surface the orchestrator must keep aligned with current task reality."""

    uri: str = Field(
        description="Path, pull-request URL, or ticket URL the orchestrator must keep current."
    )
    note: str = Field(
        description="What current means and when to update this surface. One sentence. Not a journal."
    )


class PullRequestDocument(StrictDocument):
    """A pull request associated with work in one repository."""

    url: str = Field(description="Canonical pull request URL; also used as the upsert identity.")
    status: PullRequestStatus = Field(description="Current pull request lifecycle state.")
    note: str = Field(description="Review state, caveat, or other synchronization evidence.")


class RepositoryWorkDocument(StrictDocument):
    """Repository-specific execution state for a slice."""

    worktree: str | None = Field(default=None, description="Filesystem path of the isolated worktree, if used.")
    prs: list[PullRequestDocument] = Field(default_factory=list, description="Pull requests opened for this repository.")


class SliceDocument(StrictDocument):
    """An atomic, ordered unit of task delivery."""

    key: str = Field(description="Stable slice identifier used by dependencies and the cursor.")
    kind: str = Field(description="Profile slice-kind key controlling policies and default steps.")
    title: str = Field(description="Short human-readable slice name.")
    goal: str = Field(description="Bounded outcome that makes the slice complete.")
    status: TaskStatus = Field(description="Current lifecycle state of the slice.")
    note: str = Field(description="Slice-level evidence, decision, blocker, or skip reason.")
    execution: ExecutionDocument | None = Field(default=None, description="Slice-level orchestrator provenance.")
    repos: list[str] = Field(default_factory=list, description="Repositories whose state this slice changes.")
    depends_on: list[str] = Field(default_factory=list, description="Slice keys that must finish before this slice is actionable.")
    layer: str | None = Field(
        default=None,
        description=(
            "Registered task.layers name when the registry is non-empty and this slice "
            "kind is implement, spike, or research."
        ),
    )
    repo_work: dict[str, RepositoryWorkDocument] = Field(
        default_factory=dict,
        description="Worktrees and pull requests keyed by repository name.",
    )
    steps: list[StepDocument] = Field(default_factory=list, description="Ordered primary execution steps.")
    final_steps: list[StepDocument] = Field(default_factory=list, description="Ordered terminal or cleanup steps.")

    @model_validator(mode="after")
    def unique_step_keys(self) -> SliceDocument:
        for group_name, steps in (("steps", self.steps), ("final_steps", self.final_steps)):
            keys = [step.key for step in steps]
            duplicates = sorted({key for key in keys if keys.count(key) > 1})
            if duplicates:
                raise ValueError(
                    f"slice {self.key!r} has duplicate keys in {group_name}: {', '.join(duplicates)}"
                )
        keys = [step.key for step in [*self.steps, *self.final_steps]]
        duplicates = sorted({key for key in keys if keys.count(key) > 1})
        if duplicates:
            raise ValueError(
                f"slice {self.key!r} repeats keys across steps and final_steps: "
                f"{', '.join(duplicates)}"
            )
        return self


class SourceDocument(StrictDocument):
    """Where and why a task was discovered."""

    jira: str = Field(default="", description="Originating Jira issue key or URL, when any.")
    discovered: str = Field(default="", description="Discovery date or source-specific discovery identifier.")
    context: str = Field(default="", description="Brief explanation of why the task exists.")


class ProjectDocument(StrictDocument):
    """Project identity and navigation context for a task."""

    key: str = Field(default="", description="Stable project identifier.")
    name: str = Field(default="", description="Human-readable project name.")
    route: str = Field(default="", description="Repository-relative route to the project workflow or context.")
    context: str = Field(default="", description="Brief explanation of where the work belongs.")


class CodingExecutionPolicyDocument(StrictDocument):
    """Policy governing how code-changing steps are delegated and reviewed."""

    subagent_required: bool = Field(default=True, description="Whether coding should run in a delegated subagent.")
    lower_power_model_preferred: bool = Field(default=True, description="Whether a lower-cost capable model is preferred.")
    orchestrator_reviews_subagent: bool = Field(default=True, description="Whether the orchestrator must review delegated work.")
    exceptions: list[str] = Field(default_factory=list, description="Named conditions that permit policy exceptions.")


class OrchestrationPolicyDocument(StrictDocument):
    """Policies persisted with a task rather than looked up from the live profile."""

    coding_execution: CodingExecutionPolicyDocument = Field(
        default_factory=CodingExecutionPolicyDocument,
        description="Delegation policy for code-changing work.",
    )


class OwnedCursorDocument(StrictDocument):
    """One owner's claim on a whole slice. No stored step: the active step is always
    derived as the claimed slice's first non-terminal step (see within_slice_cursor)."""

    owner: str = Field(description="Claim owner identity: CLI --owner or PI_JOB_OWNER; agent-chosen.")
    slice: str = Field(description="Key of the claimed slice; one owner claims at most one whole slice.")
    claimed_at: str = Field(description="UTC ISO 8601 timestamp recorded when the claim was created.")
    last_seen: str = Field(description="UTC ISO 8601 timestamp bumped by mutating commands run by this owner.")


class OrchestrationDocument(StrictDocument):
    """Persisted owned cursors, execution policy, and artifact state."""

    cursors: list[OwnedCursorDocument] = Field(
        default_factory=list,
        description=(
            "Active owned claims. Replaces the single orchestration.cursor (hard cut; no "
            "read shim). Each owner holds at most one claim; each slice has at most one "
            "non-stale claim."
        ),
    )
    policy: OrchestrationPolicyDocument = Field(
        default_factory=OrchestrationPolicyDocument,
        description="Task-specific execution policy.",
    )
    artifacts: dict[str, ArtifactDocument] = Field(
        default_factory=dict,
        description="Planning and delivery artifacts keyed by profile artifact identifier.",
    )
    maintain: list[MaintainItemDocument] = Field(
        default_factory=list,
        description=(
            "Surfaces the orchestrator must keep current (aid files, PR bodies, Jira comments). "
            "Keyed by uri. Empty means none registered."
        ),
    )
    content_digest: str | None = Field(
        default=None,
        description="SHA-256 hex of last pi-job semantic write; excludes this field from the hash input.",
    )

    @model_validator(mode="after")
    def unique_maintain_uris(self) -> OrchestrationDocument:
        for item in self.maintain:
            if not item.uri.strip() or not item.note.strip():
                raise ValueError("orchestration.maintain items require non-empty uri and note")
        uris = [item.uri for item in self.maintain]
        duplicates = sorted({uri for uri in uris if uris.count(uri) > 1})
        if duplicates:
            raise ValueError(f"orchestration.maintain has duplicate uri values: {', '.join(duplicates)}")
        return self


class PlanDocument(StrictDocument):
    """Ordered delivery plan for a task."""

    note: str = Field(default="", description="High-level plan context; detailed slice plans live in sibling Markdown files.")
    slices: list[SliceDocument] = Field(default_factory=list, description="Ordered task slices traversed by next and advance.")


class TaskDocument(StrictDocument):
    """Complete persisted task state shared by every storage backend."""

    title: str = Field(description="Human-readable task title.")
    status: TaskStatus = Field(
        description=(
            "Persisted overall status (schema compat). Display and reporting ignore this field "
            "and derive overall status from plan.slices[].status."
        ),
    )
    source: SourceDocument = Field(default_factory=SourceDocument, description="Task discovery metadata.")
    project: ProjectDocument = Field(default_factory=ProjectDocument, description="Owning project metadata.")
    context: str = Field(default="", description="Free-form background required before acting on the task.")
    orchestration: OrchestrationDocument | None = Field(default=None, description="Cursor and policy state after initialization.")
    decisions: list[DecisionDocument] = Field(
        default_factory=list,
        description="Product/scope decisions in chronological order - not step evidence (use finish --note).",
    )
    layers: list[LayerDocument] = Field(
        default_factory=list,
        description=(
            "Ordered business/tech bands this task touches. Empty means N/A "
            "(single-band or no cross-cutting flow)."
        ),
    )
    plan: PlanDocument = Field(default_factory=PlanDocument, description="Ordered slice and step plan.")

    @model_validator(mode="after")
    def unique_slice_keys(self) -> TaskDocument:
        keys = [task_slice.key for task_slice in self.plan.slices]
        duplicates = sorted({key for key in keys if keys.count(key) > 1})
        if duplicates:
            raise ValueError(f"task has duplicate slice keys: {', '.join(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_layer_bindings(self) -> TaskDocument:
        names = [layer.name for layer in self.layers]
        duplicates = sorted({name for name in names if names.count(name) > 1})
        if duplicates:
            raise ValueError(f"task has duplicate layer names: {', '.join(duplicates)}")
        known = set(names)
        if not known:
            for task_slice in self.plan.slices:
                if task_slice.layer:
                    raise ValueError(
                        f"slice {task_slice.key!r} has layer {task_slice.layer!r} "
                        "but task.layers is empty"
                    )
            return self
        for task_slice in self.plan.slices:
            if task_slice.kind in LAYERED_SLICE_KINDS:
                if not task_slice.layer:
                    raise ValueError(
                        f"slice {task_slice.key!r} kind {task_slice.kind!r} requires "
                        "a layer when task.layers is set"
                    )
                if task_slice.layer not in known:
                    raise ValueError(
                        f"slice {task_slice.key!r} layer {task_slice.layer!r} "
                        "is not in task.layers"
                    )
            elif task_slice.layer:
                raise ValueError(
                    f"slice {task_slice.key!r} kind {task_slice.kind!r} must not set layer"
                )
        return self


class BootstrapSliceDocument(StrictDocument):
    """Slice declaration in a bootstrap input document."""

    key: str = Field(description="Stable slice identifier.")
    kind: str = Field(description="Slice kind, validated against the active profile.")
    title: str = Field(description="Short human-readable slice name.")
    goal: str = Field(description="Bounded outcome that makes the slice complete.")
    depends_on: list[str] = Field(default_factory=list, description="Slice keys that must finish before this slice is actionable.")
    repos: list[str] = Field(default_factory=list, description="Repositories whose state this slice changes.")


class BootstrapDocument(StrictDocument):
    """Input document for the transactional bootstrap command."""

    title: str = Field(description="Human-readable task title.")
    status: TaskStatus = Field(default="in_progress", description="Overall task lifecycle state.")
    initial_slice_kind: str | None = Field(default=None, description="When set, bootstrap seeds a slice of this kind (from profile step_template) before the declared slices.")
    initial_slice_key: str | None = Field(default=None, description="Key for the seeded initial slice; defaults to task-{kind}.")
    source: SourceDocument = Field(default_factory=SourceDocument, description="Task discovery metadata.")
    project: ProjectDocument = Field(default_factory=ProjectDocument, description="Owning project metadata.")
    context: str = Field(default="", description="Free-form background required before acting on the task.")
    plan_note: str = Field(default="", description="High-level plan context.")
    decisions: list[DecisionDocument] = Field(
        default_factory=list,
        description="Product/scope decisions in chronological order - not step evidence (use finish --note).",
    )
    slices: list[BootstrapSliceDocument] = Field(default_factory=list, description="Ordered task slices declared by the user.")

    @model_validator(mode="after")
    def validate_initial_slice_fields(self) -> BootstrapDocument:
        if self.initial_slice_key is not None and self.initial_slice_kind is None:
            raise ValueError("initial_slice_key requires initial_slice_kind")
        if not self.initial_slice_kind and not self.slices:
            raise ValueError("create intent must declare initial_slice_kind or at least one slice")
        return self



@dataclass(frozen=True)
class Cursor:
    """A derived position in the plan (slice, optional step). Never persisted directly:
    within a claimed slice the step is always derived (see within_slice_cursor)."""

    slice: str
    step: str | None = None

    def label(self) -> str:
        if self.step:
            return f"{self.slice} / {self.step}"
        return self.slice


@dataclass(frozen=True)
class OwnedCursor:
    """One owner's claim on a whole slice, as persisted in orchestration.cursors.
    Distinct from step_owner()'s "orchestrator|subagent" execution owner - this is the
    claim identity (CLI --owner / PI_JOB_OWNER), not a role."""

    owner: str
    slice: str
    claimed_at: str
    last_seen: str


@dataclass(frozen=True)
class ExecutionRecord:
    model: str
    started: str
    ended: str | None = None

    @classmethod
    def from_mapping(cls, value: Mapping[str, Any] | None) -> ExecutionRecord | None:
        if not value:
            return None
        return cls(
            model=str(value.get("model") or ""),
            started=str(value.get("started") or ""),
            ended=str(value["ended"]) if value.get("ended") is not None else None,
        )


@dataclass(frozen=True)
class TaskStep:
    key: str
    title: str
    status: str
    note: str
    execution: ExecutionRecord | None = None

    @classmethod
    def from_mapping(cls, value: Mapping[str, Any]) -> TaskStep:
        return cls(
            key=str(value.get("key") or ""),
            title=str(value.get("title") or ""),
            status=str(value.get("status") or "").lower(),
            note=str(value.get("note") or ""),
            execution=ExecutionRecord.from_mapping(value.get("execution")),
        )


@dataclass(frozen=True)
class TaskSlice:
    key: str
    kind: str
    title: str
    goal: str
    status: str
    note: str
    steps: tuple[TaskStep, ...]
    final_steps: tuple[TaskStep, ...]
    repos: tuple[str, ...] = ()
    depends_on: tuple[str, ...] = ()
    layer: str | None = None
    repo_work: Mapping[str, Any] | None = None
    execution: ExecutionRecord | None = None

    @classmethod
    def from_mapping(cls, value: Mapping[str, Any]) -> TaskSlice:
        layer_raw = value.get("layer")
        layer = str(layer_raw) if layer_raw else None
        return cls(
            key=str(value.get("key") or ""),
            kind=str(value.get("kind") or ""),
            title=str(value.get("title") or ""),
            goal=str(value.get("goal") or ""),
            status=str(value.get("status") or "").lower(),
            note=str(value.get("note") or ""),
            steps=tuple(TaskStep.from_mapping(step) for step in value.get("steps") or []),
            final_steps=tuple(TaskStep.from_mapping(step) for step in value.get("final_steps") or []),
            repos=tuple(str(repo) for repo in value.get("repos") or []),
            depends_on=tuple(str(key) for key in value.get("depends_on") or []),
            layer=layer,
            repo_work=value.get("repo_work") or None,
            execution=ExecutionRecord.from_mapping(value.get("execution")),
        )

    @property
    def all_steps(self) -> tuple[TaskStep, ...]:
        return self.steps + self.final_steps

    def find_step(self, key: str) -> TaskStep | None:
        return next((step for step in self.all_steps if step.key == key), None)

    def ended_stamps(self) -> tuple[str, ...]:
        """Step `execution.ended` values, else the slice-level ended stamp."""
        stamps = tuple(
            step.execution.ended
            for step in self.all_steps
            if step.execution is not None and step.execution.ended
        )
        if stamps:
            return stamps
        if self.execution is not None and self.execution.ended:
            return (self.execution.ended,)
        return ()


def task_slices(task: Mapping[str, Any]) -> tuple[TaskSlice, ...]:
    plan = task.get("plan") or {}
    slices = plan.get("slices") or []
    return tuple(TaskSlice.from_mapping(value) for value in slices)
