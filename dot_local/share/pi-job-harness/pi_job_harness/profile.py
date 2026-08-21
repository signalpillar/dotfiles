"""Execution profile documents and the validated contract loader.

Separate from task.py: this module owns profile.yaml shape, not task YAML shape.
"""

from __future__ import annotations

from collections.abc import Mapping
from functools import lru_cache
from pathlib import Path
from typing import Any, Literal

from pydantic import Field, ValidationError, model_validator

from pi_job_harness.errors import die
from pi_job_harness.task import (
    CodingExecutionPolicyDocument,
    ExecutionOwner,
    SliceKindKey,
    StrictDocument,
)

PROFILE = Path(__file__).resolve().parent / "profile.yaml"


class ConfigLayeringDocument(StrictDocument):
    """Precedence rules for future user and repository profile overlays."""

    user_defaults: bool = Field(description="Whether user-level defaults participate in configuration.")
    repo_overrides: bool = Field(description="Whether repository configuration may override user defaults.")
    precedence: Literal["repo-over-user"] = Field(description="Deterministic precedence order for configuration layers.")


class OrchestrationDefaultsDocument(StrictDocument):
    """Task-independent defaults for the owned-cursor claim layer."""

    claim_stale_after_hours: float = Field(
        default=24.0,
        description=(
            "Claim staleness TTL: a claim is stale when now - last_seen exceeds this many "
            "hours. No heartbeat daemon; mutating commands for an owner bump last_seen."
        ),
    )


class ArtifactGateDocument(StrictDocument):
    """Artifact that must exist before a governed step can complete."""

    key: str = Field(description="Artifact identifier recorded by the task.")
    required: bool = Field(description="Whether the gate is mandatory when its condition applies.")
    when: str = Field(description="Human-readable condition under which the gate applies.")
    source: str = Field(description="Task evidence used to decide whether the gate is satisfied.")
    output: str = Field(description="Expected durable output when the gate is satisfied.")


class ArtifactRuleDocument(StrictDocument):
    """Reusable rule for producing and validating an artifact."""

    key: str = Field(description="Stable artifact-rule identifier; must match its mapping key.")
    purpose: str = Field(description="Why the artifact exists.")
    trigger: str = Field(description="Condition that makes the artifact relevant.")
    inputs: list[str] = Field(description="Task or session inputs used to produce the artifact.")
    outputs: list[str] = Field(description="Expected files, links, or records.")
    guardrails: list[str] = Field(description="Constraints that govern artifact production.")
    validator: str = Field(description="Evidence required to consider the artifact complete or skipped.")


class ToolbeltAidDocument(StrictDocument):
    """Optional planning aid suggested for selected slice kinds."""

    key: str = Field(description="Stable aid identifier; must match its mapping key.")
    title: str = Field(description="Human-readable aid name.")
    purpose: str = Field(description="Planning question or risk the aid addresses.")
    suits: list[SliceKindKey] = Field(description="Slice kinds for which this aid is useful.")
    example: str | None = Field(default=None, description="Optional concise usage example.")


class RecordChannelsDocument(StrictDocument):
    """Channel catalog and packet blurbs. Channel ids are profile-owned; Python validates membership only."""

    catalog: str = Field(description="Full channel map printed by `pi-job channels`.")
    blurbs: dict[str, str] = Field(description="One-line blurbs keyed by channel id for RECORD RESULTS packets.")


class StepKindDocument(StrictDocument):
    """Live execution contract looked up by persisted step key."""

    key: str = Field(description="Stable step-kind identifier; must match its mapping key.")
    title: str = Field(description="Default title used when creating a step.")
    owner: ExecutionOwner = Field(description="Actor responsible for executing the step.")
    record_channels: list[str] = Field(
        description="Non-empty channel ids emitted in RECORD RESULTS for this step; each must exist in record_channels.blurbs.",
    )
    requires_user_decision: bool = Field(default=False, description="Whether execution requires an explicit user choice.")
    different_model_from_step: str | None = Field(
        default=None,
        description="Step key whose recorded executor model must differ from this step's model.",
    )
    validators: list[str] = Field(default_factory=list, description="Evidence checks shown to the orchestrator.")
    skip_rule: str | None = Field(default=None, description="Condition under which the step may be skipped.")
    guidance: str | None = Field(default=None, description="Deterministic execution guidance included in instructions.")
    artifact_gates: list[ArtifactGateDocument] = Field(default_factory=list, description="Artifacts required by this step.")


class SlicePoliciesDocument(StrictDocument):
    """Policies applied to all slices of one kind."""

    coding_execution: CodingExecutionPolicyDocument | None = Field(
        default=None,
        description="Delegation policy for code-changing slices.",
    )
    no_code_changes: bool = Field(default=False, description="Whether the slice kind prohibits code changes.")


class SliceKindDocument(StrictDocument):
    """Live contract and creation template for one class of slice."""

    key: SliceKindKey = Field(description="Stable slice-kind identifier; must match its mapping key.")
    title: str = Field(description="Human-readable slice-kind name.")
    description: str = Field(description="Purpose and lifecycle role of the slice kind.")
    policies: SlicePoliciesDocument = Field(default_factory=SlicePoliciesDocument, description="Policies applied to this kind.")
    step_template: list[str] = Field(description="Ordered step keys created for new slices of this kind.")
    required_steps: list[str] | None = Field(
        default=None,
        description="Stable structural minimum for persisted slices; defaults to the creation template.",
    )


class CliHelpAddDecisionDocument(StrictDocument):
    """Argparse help for add-decision (profile is the only body)."""

    command: str = Field(description="Subparser help/description for add-decision.")
    note: str = Field(description="Help text for add-decision --note.")


class CliHelpFinishDocument(StrictDocument):
    """Argparse help for finish channel-related flags (profile is the only body)."""

    note: str = Field(description="Help text for finish --note.")


class CliHelpDocument(StrictDocument):
    """CLI help snippets owned by the profile. Python must not hardcode these strings."""

    add_decision: CliHelpAddDecisionDocument = Field(description="Help for add-decision.")
    finish: CliHelpFinishDocument = Field(description="Help for finish channel-related flags.")


class InstructionPacketsDocument(StrictDocument):
    """Required instruction text blocks. Profile is the single source of truth; no Python fallbacks."""

    todo_tracking: str = Field(description="Todo-tracking lines in every instruction packet. Supports {cursor}.")
    future_work: str = Field(description="Future-work capture lines in every instruction packet.")
    record_results_intro: str = Field(
        description=(
            "Intro lines for RECORD RESULTS in execution instruction packets. "
            "Use literal TASK_FILE / SLICE_KEY command hints (not interpolated paths). Supports {cursor}."
        )
    )
    task_record_discipline: str = Field(
        description=(
            "Legacy full task-record discipline text kept in the profile for reference and CLI help. "
            "Execution packets use record_results_intro plus step-scoped channel blurbs instead."
        )
    )
    next_action: str = Field(
        description=(
            "Concrete next-action checklist at the top of every instruction packet. "
            "Use literal TASK_FILE command hints (not interpolated paths). Supports {cursor}."
        )
    )
    pick_next_slice: str = Field(
        description=(
            "Instruction packet body when the current slice has no unfinished steps. "
            "Orchestrator must pick from Ready via show + advance --slice/--step. "
            "Use literal TASK_FILE command hints. Supports {cursor}."
        )
    )
    orchestrator: str = Field(description="Orchestrator-owned step execution guidance.")
    subagent_orchestrator: str = Field(
        description=(
            "Orchestrator dispatch guidance when the step owner is subagent. "
            "Use literal TASK_FILE / SLICE_KEY command hints (not interpolated paths)."
        )
    )
    subagent_prompt: str = Field(
        description=(
            "Subagent-facing prompt body. Supports {cursor}. "
            "Use literal TASK_FILE / SLICE_KEY command hints (not interpolated paths)."
        )
    )
    plan_todo: str = Field(description="Todo guidance appended to `pi-job plan` output.")
    missing_task_hint: str = Field(
        description="Error text when --task path is missing. Supports {task_file}."
    )
    out_of_band_edit_warning: str = Field(
        description=(
            "Stderr warning when orchestration.content_digest does not match semantic content. "
            "Supports {task_file}. Profile is the only body; Python must not hardcode the message."
        )
    )
    seed_slice_plans: str = Field(description="Preamble for SEED SLICE PLAN FILES NOW blocks after add-slice/bootstrap.")
    slice_plan_stub: str = Field(
        description=(
            "Markdown body written when add-slice auto-creates a missing plan file. "
            "Supports {key}, {goal}, {depends_on}. Profile is the only body."
        )
    )
    findings_file_header: str = Field(
        description=(
            "Preamble written once when YamlTaskStore.create/append creates the "
            "layout findings file (`<stem>.plans/_findings.md`). Profile is the only "
            "body; path ownership is YamlTaskLayout / YamlTaskStore."
        )
    )
    bigpicture_stub: str = Field(
        description=(
            "Body written once as references/bigpicture.txt when layers are first "
            "registered and the file is missing. Supports {stub_marker}, {top_layer}, "
            "{bottom_layer}, {layer_names}, {layer_bands}, {bigpicture_path}. "
            "Profile is the only body; Python must not hardcode the spine shape."
        )
    )
    status_interrupt_hint: str = Field(
        description=(
            "One-line status coaching when the saved cursor sits on an interrupt-park "
            "step (see ProfileDocument.interrupt_park_steps). Profile is the only body."
        )
    )
    investigate_interrupt: str = Field(
        description=(
            "Body printed by `pi-job investigate`. Supports {task_file}, {cursor_label}, "
            "{topic}, {pointer}, {source}, {finding_status}. Profile is the only body; "
            "Python must not hardcode the packet."
        )
    )
    orchestrator_heartbeat: str = Field(
        description=(
            "Body printed by `pi-job loop` as one physical line (manager fleet metronome). "
            "No format placeholders. Use literal TASK in command hints (not {task_file}). "
            "Profile is the only body; Python must not hardcode the packet."
        )
    )
    slice_worker_boot: str = Field(
        description=(
            "Body printed by `pi-job loop --worker` as one physical line "
            "(first prompt for a spawned slice-worker window). "
            "No format placeholders. Use literal OWNER / SLICE / TASK in command hints. "
            "Profile is the only body; Python must not hardcode the packet."
        )
    )
    maintain_header: str = Field(
        description=(
            "Heading for the keep-current inventory in plan/instruction packets. "
            "Profile is the only body."
        )
    )
    maintain_empty: str = Field(
        description=(
            "Body when orchestration.maintain is empty. "
            "Use literal TASK_FILE command hints. Profile is the only body."
        )
    )
    maintain_item: str = Field(
        description=(
            "One maintain row. Supports {uri} and {note}. Profile is the only body."
        )
    )


class ProfileDocument(StrictDocument):
    """Complete live execution profile loaded by every command."""

    config_layering: ConfigLayeringDocument = Field(description="Configuration overlay capabilities and precedence.")
    orchestration_defaults: OrchestrationDefaultsDocument = Field(
        default_factory=OrchestrationDefaultsDocument,
        description="Task-independent defaults for the owned-cursor claim layer (e.g. staleness TTL).",
    )
    artifact_rules: dict[str, ArtifactRuleDocument] = Field(description="Artifact rules keyed by stable identifier.")
    toolbelt: dict[str, ToolbeltAidDocument] = Field(description="Planning aids keyed by stable identifier.")
    pr_template_guardrail: str = Field(description="Shared pull-request template guidance expanded into relevant rules.")
    plan_and_grill_guardrail: str = Field(description="Shared create-plan and grill-plan execution guidance.")
    sync_pipeline_instructions: str = Field(
        description=(
            "Mandatory ACTION REQUIRED body for sync output: offline read-only checklist; "
            "orchestrator must run live gh/Jira verification. Python must not duplicate a fallback body."
        )
    )
    cli_help: CliHelpDocument = Field(
        description="CLI help snippets for channel-sensitive commands. Profile is the only body; Python must not hardcode them."
    )
    instruction_packets: InstructionPacketsDocument = Field(
        description="Required instruction text blocks. All packet bodies live here; Python must not duplicate them."
    )
    record_channels: RecordChannelsDocument = Field(
        description="Channel catalog and blurbs. Python validates step_kinds.record_channels membership only."
    )
    interrupt_park_steps: list[str] = Field(
        description=(
            "Step keys that park the cursor for user decision (grill/clarify). "
            "status prints instruction_packets.status_interrupt_hint when the "
            "saved cursor step is in this list. Each entry must exist in step_kinds."
        )
    )
    step_kinds: dict[str, StepKindDocument] = Field(description="Step execution contracts keyed by persisted step key.")
    slice_kinds: dict[SliceKindKey, SliceKindDocument] = Field(description="Slice contracts keyed by slice kind.")

    @model_validator(mode="after")
    def validate_catalog_references(self) -> ProfileDocument:
        catalogs: tuple[tuple[str, Mapping[Any, Any]], ...] = (
            ("artifact_rules", self.artifact_rules),
            ("toolbelt", self.toolbelt),
            ("step_kinds", self.step_kinds),
            ("slice_kinds", self.slice_kinds),
        )
        for catalog_name, catalog in catalogs:
            mismatches = [key for key, item in catalog.items() if item.key != key]
            if mismatches:
                raise ValueError(f"{catalog_name} entries whose key field does not match: {', '.join(mismatches)}")

        known_steps = set(self.step_kinds)
        unknown_park = [step for step in self.interrupt_park_steps if step not in known_steps]
        if unknown_park:
            raise ValueError(
                "interrupt_park_steps references unknown step kinds: "
                + ", ".join(unknown_park)
            )
        for key, kind in self.slice_kinds.items():
            unknown = [step for step in [*kind.step_template, *(kind.required_steps or [])] if step not in known_steps]
            if unknown:
                raise ValueError(f"slice kind {key!r} references unknown steps: {', '.join(unknown)}")
            missing_from_template = [
                step for step in (kind.required_steps or []) if step not in kind.step_template
            ]
            if missing_from_template:
                raise ValueError(
                    f"slice kind {key!r} requires steps absent from its creation template: "
                    f"{', '.join(missing_from_template)}"
                )
        known_channel_ids = set(self.record_channels.blurbs)
        for key, step in self.step_kinds.items():
            if step.different_model_from_step and step.different_model_from_step not in known_steps:
                raise ValueError(
                    f"step kind {key!r} requires a different model from unknown step "
                    f"{step.different_model_from_step!r}"
                )
            if not step.record_channels:
                raise ValueError(f"step kind {key!r} must declare non-empty record_channels")
            unknown_channels = [channel for channel in step.record_channels if channel not in known_channel_ids]
            if unknown_channels:
                raise ValueError(
                    f"step kind {key!r} references unknown record_channels: {', '.join(unknown_channels)}"
                )
        return self


@lru_cache(maxsize=1)
def load_profile_contract() -> dict[str, Any]:
    from pi_job_harness.store.yaml_io import load_yaml_mapping

    data = load_yaml_mapping(PROFILE, label="execution profile")
    try:
        profile = ProfileDocument.model_validate(data)
    except ValidationError as exc:
        die(f"profile validation failed for {PROFILE}:\n{exc}")
    return profile.model_dump(mode="json", exclude_none=True)

