package harness

// Deterministic profile/phase contract for the pi-agent job harness.
// Source: projects/pi-agent-job-harness/tasks/2026-07-09-bootstrap-pi-agent-job-harness.cue

#Status: "planned" | "in_progress" | "blocked" | "done" | "skipped"

#ProfileKey: "small" | "full" | "research" | "review-only" | "share-only" | "spike-prototype"

#ExecutionOwner: "orchestrator" | "subagent" | "external_tool"

#ArtifactKey: "task_record" | "decision_review_deck" | "daily_boo" | "share_with_team" | "ticket" | "pull_request" | "e2e_evidence"

#PhaseKey: "explore_context" | "clarify_scope" | "clarify_if_needed" | "grill_plan" | "plan_slices" | "implement" | "implement_slices" | "verify" | "review" | "investigate" | "synthesize" | "share_with_team" | "wait_for_team_feedback" | "clarify_feedback" | "address_feedback" | "decision_review_deck" | "daily_boo_reflection" | "update_task_record"

#ConfigLayering: {
	user_defaults:  true
	repo_overrides: true
	precedence:     "repo-over-user"
}

#CodingExecutionPolicy: {
	subagent_required:             bool | *true
	lower_power_model_preferred:   bool | *true
	orchestrator_reviews_subagent: bool | *true
	exceptions?: [...string]
}

#ArtifactGate: {
	key:      #ArtifactKey
	required: bool
	when:     string
	source:   string
	output:   string
}

#ArtifactRule: {
	key:     #ArtifactKey
	purpose: string
	trigger: string
	inputs: [...string]
	outputs: [...string]
	guardrails: [...string]
	validator: string
}

#Phase: {
	key:      #PhaseKey
	title:    string
	owner:    #ExecutionOwner
	required: bool | *true
	inputs: [...string]
	outputs: [...string]
	validators: [...string]
	skip_rule?: string
	// Human-readable advice surfaced when the phase runs (non-blocking guidance).
	guidance?: string
	artifact_gates?: [...#ArtifactGate]
}

#Profile: {
	key:         #ProfileKey
	title:       string
	description: string
	policies: {
		coding_execution?: #CodingExecutionPolicy
		no_code_changes?:  bool
	}
	phases: [#Phase, ...#Phase]
}

config_layering: #ConfigLayering

artifact_rules: {
	decision_review_deck: #ArtifactRule & {
		key:     "decision_review_deck"
		purpose: "Create a two-minute async decision review deck when durable decisions need team review."
		trigger: "task.decisions contains decisions worth presenting, or the user asks for a decision walkthrough"
		inputs: ["task.decisions", "task.plan.slices"]
		outputs: ["projects/<project>/<YYYY-MM-DD>-<topic>-review.md"]
		guardrails: [
			"pull decisions from the durable task record, not the diff",
			"use slides separated by ---",
			"include ASCII before/after or flow diagrams for decision slides",
			"avoid file names, function names, and test counts",
		]
		validator: "deck path recorded or skip reason recorded"
	}

	daily_boo: #ArtifactRule & {
		key:     "daily_boo"
		purpose: "Append a personal aha-moment entry worth revisiting later."
		trigger: "session produced a real aha or revisit-worthy domain insight"
		inputs: ["session_insights"]
		outputs: ["daily.boo.txt append"]
		guardrails: [
			"append only via skills/daily-boo/scripts/append.sh",
			"never read daily.boo.txt",
			"do not use for task tracking, PR status, or agent lessons",
			"skip rather than pad a thin entry",
		]
		validator: "append command recorded or skip reason recorded"
	}

	share_with_team: #ArtifactRule & {
		key:     "share_with_team"
		purpose: "Create team-facing tickets and PRs from the completed work."
		trigger: "full or share-only profile reaches the handoff phase"
		inputs: ["task.plan", "verification_evidence", "review_findings_resolved"]
		outputs: ["ticket keys", "pull request URLs", "team feedback thread"]
		guardrails: [
			"invoke share-with-team once per repository changed",
			"reconcile against repos already shared during implement_slices; do not double-open a ticket or PR for an already-shared repo",
			"use the share-with-team ticket template",
			"use the repository PR template when present, otherwise the share-with-team fallback",
			"include e2e evidence or explicitly record the gap",
			"do not treat sharing as terminal; wait for team feedback and loop on it",
		]
		validator: "ticket and PR links recorded with feedback wait state, or skip reason recorded for profiles where optional"
	}
}

profiles: {
	small: #Profile & {
		key:         "small"
		title:       "Small Task"
		description: "Bounded work with little ambiguity; keeps task-state discipline without full ticket/PR ceremony."
		policies: {
			coding_execution: #CodingExecutionPolicy & {
				exceptions: ["trivial-single-file-edit", "user-explicitly-requests-orchestrator"]
			}
		}
		phases: [
			#Phase & {key: "explore_context", title: "Explore context", owner: "orchestrator", inputs: ["task.source", "task.context"], outputs: ["known_repos", "adjacent_context"], validators: ["repos-or-explicitly-not-applicable"]},
			#Phase & {key: "clarify_if_needed", title: "Clarify only blocking ambiguity", owner: "orchestrator", inputs: ["adjacent_context"], outputs: ["resolved_scope_or_no_questions"], validators: ["blocking-ambiguity-resolved"], skip_rule: "skip when exploration answers the implementation-relevant questions"},
			#Phase & {key: "implement", title: "Implement", owner: "subagent", inputs: ["resolved_scope_or_no_questions"], outputs: ["changed_files", "commands_run", "risks"], validators: ["coding-policy-recorded", "worktree-used-when-repo-editing"]},
			#Phase & {key: "verify", title: "Verify", owner: "orchestrator", inputs: ["changed_files", "commands_run"], outputs: ["verification_evidence"], validators: ["relevant-tests-or-gap-recorded"]},
			#Phase & {key: "update_task_record", title: "Update task record", owner: "orchestrator", inputs: ["verification_evidence"], outputs: ["task_record_updated"], validators: ["task-cursor-advanced"]},
		]
	}

	full: #Profile & {
		key:         "full"
		title:       "Full Delivery"
		description: "End-to-end implementation through review, team sharing, ticket, and PR."
		policies: {
			coding_execution: #CodingExecutionPolicy
		}
		phases: [
			#Phase & {key: "explore_context", title: "Explore context", owner: "orchestrator", inputs: ["task.source"], outputs: ["repo_paths", "task_record", "tracker_state"], validators: ["repo-paths-known"]},
			#Phase & {key: "clarify_scope", title: "Clarify scope", owner: "orchestrator", inputs: ["repo_paths", "task_record"], outputs: ["scope", "definition_of_done", "epic_decision"], validators: ["scope-echoed"]},
			#Phase & {key: "grill_plan", title: "Grill plan", owner: "orchestrator", inputs: ["scope"], outputs: ["plan_decisions"], validators: ["grill-done-or-user-declined"], skip_rule: "only when user explicitly declines"},
			#Phase & {
				key:   "plan_slices"
				title: "Plan slices"
				owner: "orchestrator"
				inputs: ["scope", "plan_decisions"]
				outputs: ["task.plan.slices"]
				validators: ["at-least-one-slice", "slices-repo-scoped-and-atomic"]
				guidance: "Prefer one slice per repository. Make each slice atomic — self-contained end to end — so it carries its own terminal delivery steps (e2e-evidence and share-with-team: ticket + PR) and the repo change ships within the slice. Split multi-repo work into one atomic slice per repo, ordered by dependency (a prerequisite repo's slice ships before the slices that consume it)."
			},
			#Phase & {
				key:   "implement_slices"
				title: "Implement slices"
				owner: "subagent"
				inputs: ["task.plan.slices"]
				outputs: ["slice_evidence", "changed_repos"]
				validators: ["each-slice-loop-complete"]
				guidance: "Each atomic slice runs its full loop and ends with its own terminal steps: e2e-evidence, then share-with-team (ticket + PR for that slice's repo). An atomic slice ships its repo change as it completes rather than waiting for a terminal bundle; downstream slices build on the merged prerequisite."
				artifact_gates: [
					#ArtifactGate & {key: "ticket", required: true, when: "an atomic slice completes its repo change", source: "changed_repos", output: "one ticket for the slice's repo"},
					#ArtifactGate & {key: "pull_request", required: true, when: "an atomic slice completes its repo change", source: "changed_files", output: "one PR for the slice's repo, opened as the slice completes"},
				]
			},
			#Phase & {key: "review", title: "Review", owner: "orchestrator", inputs: ["slice_evidence"], outputs: ["review_findings_resolved"], validators: ["volod-style-review-done"]},
			#Phase & {
				key:   "decision_review_deck"
				title: "Decision review deck"
				owner: "orchestrator"
				inputs: ["task.decisions"]
				outputs: ["decision_review_deck_path_or_skip_reason"]
				validators: ["deck-created-or-not-needed-recorded"]
				artifact_gates: [#ArtifactGate & {key: "decision_review_deck", required: false, when: "durable decisions need async team review", source: "task.decisions", output: "dated markdown deck in project directory"}]
			},
			#Phase & {
				key:   "share_with_team"
				title: "Share with team (reconciliation)"
				owner: "orchestrator"
				inputs: ["review_findings_resolved", "verification_evidence", "changed_repos"]
				outputs: ["ticket_keys", "pull_request_urls"]
				validators: ["share-with-team-template-used", "every-changed-repo-has-ticket-and-pr"]
				guidance: "With atomic per-repo slices this is a reconciliation catch-all, usually a no-op: each slice already shared its repo. Only share here the repos that were NOT shared atomically (e.g. bundled or cross-cutting changes). Reconcile — never re-open a ticket/PR for an already-shared repo."
				artifact_gates: [
					#ArtifactGate & {key: "ticket", required: true, when: "a changed repo was not shared atomically during implement_slices", source: "changed_repos", output: "one ticket per unshared changed repo"},
					#ArtifactGate & {key: "pull_request", required: true, when: "a changed repo was not PR'd atomically during implement_slices", source: "changed_repos", output: "one PR per unshared changed repo"},
				]
			},
			#Phase & {key: "wait_for_team_feedback", title: "Wait for team feedback", owner: "orchestrator", inputs: ["ticket_keys", "pull_request_urls"], outputs: ["team_feedback_or_no_response"], validators: ["feedback-window-recorded"]},
			#Phase & {key: "clarify_feedback", title: "Clarify team feedback", owner: "orchestrator", inputs: ["team_feedback_or_no_response"], outputs: ["clarified_feedback_or_no_action"], validators: ["ambiguous-feedback-clarified-or-deferred"], skip_rule: "skip only when no feedback arrives or feedback is already actionable"},
			#Phase & {key: "address_feedback", title: "Address feedback", owner: "subagent", required: false, inputs: ["clarified_feedback_or_no_action"], outputs: ["feedback_changes_or_noop_reason"], validators: ["requested-changes-addressed-or-deferred"], skip_rule: "skip when feedback requires no changes"},
			#Phase & {key: "verify", title: "Verify feedback changes", owner: "orchestrator", required: false, inputs: ["feedback_changes_or_noop_reason"], outputs: ["post_feedback_verification"], validators: ["relevant-tests-or-gap-recorded"], skip_rule: "skip when no feedback changes were made"},
			#Phase & {key: "update_task_record", title: "Update task record after feedback", owner: "orchestrator", inputs: ["team_feedback_or_no_response", "post_feedback_verification"], outputs: ["feedback_loop_recorded"], validators: ["feedback-status-recorded"]},
			#Phase & {
				key:   "daily_boo_reflection"
				title: "Daily boo reflection"
				owner: "orchestrator"
				inputs: ["session_insights"]
				outputs: ["daily_boo_append_or_skip_reason"]
				validators: ["append-only-or-skip-recorded"]
				skip_rule: "skip when there is no real aha worth revisiting"
				artifact_gates: [#ArtifactGate & {key: "daily_boo", required: false, when: artifact_rules.daily_boo.trigger, source: "session_insights", output: "daily.boo.txt append or skip reason"}]
			},
		]
	}

	research: #Profile & {
		key:         "research"
		title:       "Research"
		description: "Investigation and synthesis without code changes."
		policies: {no_code_changes: true}
		phases: [
			#Phase & {key: "explore_context", title: "Explore context", owner: "orchestrator", inputs: ["task.context"], outputs: ["sources"], validators: ["sources-recorded"]},
			#Phase & {key: "clarify_scope", title: "Clarify question", owner: "orchestrator", inputs: ["sources"], outputs: ["research_question"], validators: ["question-clear"]},
			#Phase & {key: "investigate", title: "Investigate", owner: "orchestrator", inputs: ["research_question"], outputs: ["findings"], validators: ["evidence-backed-findings"]},
			#Phase & {key: "synthesize", title: "Synthesize", owner: "orchestrator", inputs: ["findings"], outputs: ["answer_or_doc"], validators: ["open-questions-called-out"]},
			#Phase & {key: "update_task_record", title: "Update task record", owner: "orchestrator", inputs: ["answer_or_doc"], outputs: ["task_record_updated"], validators: ["task-cursor-advanced"]},
		]
	}

	review_only: #Profile & {
		key:         "review-only"
		title:       "Review Only"
		description: "Code, design, or plan review where findings are the primary output."
		policies: {no_code_changes: true}
		phases: [
			#Phase & {key: "explore_context", title: "Gather review context", owner: "orchestrator", inputs: ["review_target"], outputs: ["review_material"], validators: ["target-readable"]},
			#Phase & {key: "review", title: "Review", owner: "orchestrator", inputs: ["review_material"], outputs: ["findings"], validators: ["findings-first-format"]},
			#Phase & {key: "synthesize", title: "Summarize risk", owner: "orchestrator", inputs: ["findings"], outputs: ["risk_summary"], validators: ["residual-risk-stated"]},
			#Phase & {key: "update_task_record", title: "Update task record", owner: "orchestrator", required: false, inputs: ["risk_summary"], outputs: ["task_record_updated_or_skip_reason"], validators: ["skip-reason-recorded"]},
		]
	}

	share_only: #Profile & {
		key:         "share-only"
		title:       "Share Only"
		description: "Turn already-finished work into team-facing ticket, PR, or review artifacts."
		policies: {}
		phases: [
			#Phase & {key: "explore_context", title: "Gather finished work", owner: "orchestrator", inputs: ["task_record", "diff_or_summary"], outputs: ["share_context"], validators: ["work-status-known"]},
			#Phase & {key: "decision_review_deck", title: "Decision review deck", owner: "orchestrator", required: false, inputs: ["task.decisions"], outputs: ["decision_review_deck_path_or_skip_reason"], validators: ["deck-created-or-not-needed-recorded"], artifact_gates: [#ArtifactGate & {key: "decision_review_deck", required: false, when: artifact_rules.decision_review_deck.trigger, source: "task.decisions", output: "dated markdown deck in project directory"}]},
			#Phase & {key: "share_with_team", title: "Share with team", owner: "orchestrator", inputs: ["share_context"], outputs: ["ticket_keys_or_pr_urls"], validators: ["share-with-team-template-used"], artifact_gates: [#ArtifactGate & {key: "share_with_team", required: true, when: artifact_rules.share_with_team.trigger, source: "share_context", output: "ticket keys and pull request URLs"}]},
			#Phase & {key: "wait_for_team_feedback", title: "Wait for team feedback", owner: "orchestrator", inputs: ["ticket_keys_or_pr_urls"], outputs: ["team_feedback_or_no_response"], validators: ["feedback-window-recorded"]},
			#Phase & {key: "clarify_feedback", title: "Clarify team feedback", owner: "orchestrator", inputs: ["team_feedback_or_no_response"], outputs: ["clarified_feedback_or_no_action"], validators: ["ambiguous-feedback-clarified-or-deferred"], skip_rule: "skip only when no feedback arrives or feedback is already actionable"},
			#Phase & {key: "address_feedback", title: "Address feedback", owner: "subagent", required: false, inputs: ["clarified_feedback_or_no_action"], outputs: ["feedback_changes_or_noop_reason"], validators: ["requested-changes-addressed-or-deferred"], skip_rule: "skip when feedback requires no changes"},
			#Phase & {key: "verify", title: "Verify feedback changes", owner: "orchestrator", required: false, inputs: ["feedback_changes_or_noop_reason"], outputs: ["post_feedback_verification"], validators: ["relevant-tests-or-gap-recorded"], skip_rule: "skip when no feedback changes were made"},
			#Phase & {key: "update_task_record", title: "Update task record", owner: "orchestrator", inputs: ["team_feedback_or_no_response", "post_feedback_verification"], outputs: ["task_record_updated"], validators: ["feedback-status-recorded", "session-links-recorded"]},
		]
	}

	spike_prototype: #Profile & {
		key:         "spike-prototype"
		title:       "Spike / Prototype"
		description: "Time-boxed learning where the output is evidence and a keep/change/discard recommendation."
		policies: {
			coding_execution: #CodingExecutionPolicy & {
				exceptions: ["throwaway-local-prototype"]
			}
		}
		phases: [
			#Phase & {key: "clarify_scope", title: "Define learning question", owner: "orchestrator", inputs: ["task.context"], outputs: ["learning_question", "timebox"], validators: ["timebox-recorded"]},
			#Phase & {key: "implement", title: "Prototype", owner: "subagent", inputs: ["learning_question", "timebox"], outputs: ["prototype_evidence"], validators: ["scope-contained"]},
			#Phase & {key: "verify", title: "Evaluate", owner: "orchestrator", inputs: ["prototype_evidence"], outputs: ["keep_change_discard"], validators: ["recommendation-recorded"]},
			#Phase & {key: "update_task_record", title: "Update task record", owner: "orchestrator", inputs: ["keep_change_discard"], outputs: ["task_record_updated"], validators: ["learning-recorded"]},
		]
	}
}
