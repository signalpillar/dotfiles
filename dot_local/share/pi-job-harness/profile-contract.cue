package harness

// Deterministic step/slice-kind contract for the pi-agent job harness.
// Task files carry slices with `kind` and steps with keys; meaning (owner,
// guidance, validators) lives here and is looked up live so old tasks pick up
// contract updates without rewriting step bodies.

#Status: "planned" | "in_progress" | "blocked" | "done" | "skipped"

#SliceKindKey: "setup" | "implement" | "closing" | "research" | "spike"

#ExecutionOwner: "orchestrator" | "subagent" | "external_tool"

#ArtifactKey: "task_record" | "decision_review_deck" | "daily_boo" | "share_with_team" | "ticket" | "pull_request" | "e2e_evidence"

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

#ToolbeltAid: {
	key:     string
	title:   string
	purpose: string
	suits: [...#SliceKindKey]
	example?: string
}

#StepKind: {
	key:   string
	title: string
	owner: #ExecutionOwner
	validators: [...string]
	skip_rule?: string
	guidance?:  string
	artifact_gates?: [...#ArtifactGate]
}

#SliceKind: {
	key:         #SliceKindKey
	title:       string
	description: string
	policies: {
		coding_execution?: #CodingExecutionPolicy
		no_code_changes?:  bool
	}
	// Default step keys for a NEW slice of this kind (add-slice / init templates).
	step_template: [...string]
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
		trigger: "an implement slice reaches share-with-team, or a closing reconciliation finds an unshared repo"
		inputs: ["task.plan", "verification_evidence"]
		outputs: ["ticket keys", "pull request URLs", "team feedback thread"]
		guardrails: [
			"invoke share-with-team once per repository changed",
			"reconcile against repos already shared; do not double-open a ticket or PR for an already-shared repo",
			"use the share-with-team ticket template (skills/share-with-team/SKILL.md § Ticket)",
			pr_template_guardrail,
			"include e2e evidence or explicitly record the gap",
			"do not treat sharing as terminal; wait for team feedback and loop on it",
		]
		validator: "ticket and PR links recorded with feedback wait state, or skip reason recorded"
	}
}

toolbelt: [Key=string]: #ToolbeltAid & {key: Key}
toolbelt: {
	"httpyac-api-spec": {title: "httpyac API spec", purpose: "Request/response contract for new/changed endpoints; doubles as a manual test once built.", suits: ["setup", "implement", "spike"]}
	"sequence-diagram": {title: "Sequence diagram", purpose: "Cross-service / hand-off flow (park/resume, timers, polling).", suits: ["setup", "research"]}
	"test-case-table": {title: "Test-case table", purpose: "Core journeys + edge cases, each marked v0/v1/v2.", suits: ["setup", "implement", "spike"]}
	"state-transition-table": {title: "State-transition table", purpose: "Resource lifecycle / status transitions with the rule per edge.", suits: ["setup", "research"]}
	"config-flag-matrix": {title: "Config / flag matrix", purpose: "Feature-flag x market/programme x env/region grid with default values.", suits: ["setup"]}
	"data-shape-sketch": {title: "Data-shape sketch", purpose: "FHIR resource / extension / ValueSet shapes being added or read.", suits: ["setup", "research"]}
}

pr_template_guardrail: string & """
	Before writing any PR body: (1) check the target repo for a template at
	.github/pull_request_template.md, .github/PULL_REQUEST_TEMPLATE.md, or any file
	under .github/PULL_REQUEST_TEMPLATE/; (2) if found, fill that template exactly,
	section by section — do not invent a different structure; (3) if absent, use the
	share-with-team skill's fallback template (skills/share-with-team/SKILL.md §
	PR description: Vocabulary, How <feature> works, What this PR adds, quirks,
	Errors, Known limitation, Tests) — do not freehand a bespoke PR body in either
	case.
	"""

plan_and_grill_guardrail: string & """
	Before any other step in an implement/spike slice starts, that slice's steps must
	begin with exactly two leading steps, in order:
	(1) create-plan - write the actual implementation plan for THIS slice to a sibling
	    Markdown plan file (approach, files/functions touched, key tradeoffs). Not a
	    restatement of the slice's goal. Do NOT inline the plan body in the task file.
	    Naming: beside the task file, directory `<task-stem>.plans/`, file
	    `<slice-key>.md` (e.g. task `projects/foo/tasks/bar.cue` + slice `wire-api`
	    -> `projects/foo/tasks/bar.plans/wire-api.md`). create-plan's step note is ONLY
	    a pointer: `Plan file: <task-stem>.plans/<slice-key>.md` (path relative to the
	    task file's directory).
	(2) grill-plan - interrogate that plan file using the grill-me skill
	    (skills/grill-me/SKILL.md). grill-plan's note records what was challenged and
	    what changed. If grilling surfaces a real gap, revise the plan FILE (not the
	    task-file note), grill again, only mark grill-plan done once the plan survives
	    - this is a loop, not a one-shot rubber stamp.
	Mark both done only once the plan has survived grilling; advance refuses to pass an
	incomplete step, so edit-code-style steps stay unreachable until both are done.
	Distinct from the setup slice's `grill` step (overall scope before implement slices
	exist) - grill-plan is the per-slice equivalent.
	Exception: a genuinely trivial single-file edit may skip both steps (status:
	skipped, note: one-line reason) - same exception class as coding_execution.exceptions.
	"""

step_kinds: [Key=string]: #StepKind & {key: Key}
step_kinds: {
	"explore-context": {
		title:    "Explore context"
		owner:    "orchestrator"
		guidance: "Read the adjacent code/docs needed to act. Record known repos and adjacent context - or explicitly note none apply."
		validators: ["repos-or-explicitly-not-applicable"]
	}

	"clarify-scope": {
		title:    "Clarify scope"
		owner:    "orchestrator"
		guidance: "Resolve blocking ambiguity with the user before planning. Skip only when exploration already answered the implementation-relevant questions."
		validators: ["blocking-ambiguity-resolved"]
		skip_rule:  "skip when exploration answers the implementation-relevant questions"
	}

	"grill": {
		title:    "Grill overall scope"
		owner:    "orchestrator"
		guidance: "Interrogate overall task scope using the grill-me skill (skills/grill-me/SKILL.md). note records what was challenged and what changed. Loop: revise, grill again, until it survives - not a one-shot rubber stamp."
		validators: ["grill-done-or-user-declined"]
		skip_rule:  "only when user explicitly declines"
	}

	"select-toolbelt": {
		title:    "Select planning aids from the toolbelt"
		owner:    "orchestrator"
		guidance: "From the aids whose suits includes a relevant slice kind (see `pi-job toolbelt`), select the subset that will help write the plan. Register each with `pi-job toolbelt add <key> --status planned` plus a one-line why. Do not produce the aid files yet — they are written during plan-slices and flipped to done when the file exists."
		validators: ["toolbelt-selection-recorded"]
	}

	"plan-slices": {
		title:    "Plan slices"
		owner:    "orchestrator"
		guidance: "Prefer one slice per repository. Make each slice atomic - self-contained end to end - ordered by dependency. Add implement slices with `pi-job add-slice --kind implement`."
		validators: ["at-least-one-slice", "slices-repo-scoped-and-atomic"]
	}

	"create-plan": {
		title:     "Create plan"
		owner:     "subagent"
		guidance:  plan_and_grill_guardrail
		validators: ["plan-recorded", "create-plan-and-grill-plan-done-or-skip-reason-before-other-steps"]
	}

	"grill-plan": {
		title:     "Grill the plan file"
		owner:     "orchestrator"
		guidance:  plan_and_grill_guardrail
		validators: ["grill-done-or-user-declined", "create-plan-and-grill-plan-done-or-skip-reason-before-other-steps"]
	}

	"edit-code": {
		title:    "Edit code"
		owner:    "subagent"
		guidance: "Make the change described by this slice's create-plan step. Use a worktree when editing a repo other than the current one."
		validators: ["coding-policy-recorded", "worktree-used-when-repo-editing"]
	}

	"verify": {
		title:    "Verify"
		owner:    "orchestrator"
		guidance: "Run the relevant tests/checks for what changed, or record the gap."
		validators: ["relevant-tests-or-gap-recorded"]
	}

	"e2e-evidence": {
		title: "Provide e2e/acceptance evidence or record the gap"
		owner: "orchestrator"
		validators: ["evidence-or-gap-recorded"]
	}

	"share-with-team": {
		title:    "Share with team: ticket + PR for this slice's repo"
		owner:    "orchestrator"
		guidance: pr_template_guardrail
		artifact_gates: [
			#ArtifactGate & {key: "ticket", required: true, when: "this slice completes its repo change", source: "changed_repos", output: "one ticket for the slice's repo"},
			#ArtifactGate & {key: "pull_request", required: true, when: "this slice completes its repo change", source: "changed_files", output: "one PR, body from the checked repo template or the fallback"},
		]
		validators: ["pr-template-checked", "share-with-team-template-used"]
	}

	"wait-for-feedback": {
		title:    "Wait for team feedback: loop on PR review until merged"
		owner:    "orchestrator"
		guidance: "Not terminal until the PR merges or is explicitly abandoned. Re-check via `pi-job sync`."
		validators: ["feedback-window-recorded"]
	}

	"update-task-file": {
		title: "Update this task file"
		owner: "orchestrator"
		validators: ["task-file-current"]
	}

	"investigate": {
		title:    "Investigate"
		owner:    "orchestrator"
		guidance: "Gather evidence-backed findings for the research question."
		validators: ["evidence-backed-findings"]
	}

	"synthesize": {
		title:    "Synthesize"
		owner:    "orchestrator"
		guidance: "Produce the answer or doc; call out open questions."
		validators: ["open-questions-called-out"]
	}

	"update-test-plan": {
		title:    "Update the test plan"
		owner:    "orchestrator"
		guidance: "Reconcile the PRD/plan's test-case table against what actually shipped across all slices."
		validators: ["test-plan-reconciled-or-not-applicable"]
	}

	"update-docs": {
		title:    "Update documentation"
		owner:    "orchestrator"
		guidance: "Update FACT_MAP / workflow docs / README that the shipped slices affect, or record not applicable."
		validators: ["docs-updated-or-not-applicable"]
	}

	"capture-metrics": {
		title:    "Capture metrics to watch"
		owner:    "orchestrator"
		guidance: "Record which dashboards/alerts to watch post-ship, or that none apply."
		validators: ["metrics-recorded-or-not-applicable"]
	}
}

slice_kinds: [Key=#SliceKindKey]: #SliceKind & {key: Key}
slice_kinds: {
	setup: {
		title:       "Task setup"
		description: "Explore, clarify, grill scope, pick toolbelt, plan the implementation slices. Runs once, first."
		step_template: ["explore-context", "clarify-scope", "grill", "select-toolbelt", "plan-slices"]
	}

	implement: {
		title:       "Implementation"
		description: "One atomic, repo-scoped change - planned, grilled, built, verified, shipped as it completes."
		policies: coding_execution: #CodingExecutionPolicy & {
			exceptions: ["trivial-single-file-edit", "user-explicitly-requests-orchestrator"]
		}
		step_template: ["create-plan", "grill-plan", "edit-code", "verify", "e2e-evidence", "share-with-team", "update-task-file", "wait-for-feedback"]
	}

	closing: {
		title:       "Task closing"
		description: "Cross-slice bookkeeping once every implementation slice is done. Runs once, last."
		step_template: ["update-test-plan", "update-docs", "capture-metrics", "update-task-file"]
	}

	research: {
		title:       "Research"
		description: "Investigation without code changes."
		policies: no_code_changes: true
		step_template: ["explore-context", "clarify-scope", "investigate", "synthesize", "update-task-file"]
	}

	spike: {
		title:       "Spike / prototype"
		description: "Time-boxed learning; output is evidence + a keep/change/discard call."
		policies: coding_execution: #CodingExecutionPolicy & {
			exceptions: ["throwaway-local-prototype"]
		}
		step_template: ["clarify-scope", "create-plan", "grill-plan", "edit-code", "verify", "update-task-file"]
	}
}
