# pi-job as organizational second brain

Meta source: engineering.fb.com organizational second brain, 2026-09-02.
InfoQ summary: infoq.com news 2026-09 meta-organizational-agents.
This doc compares concepts only.
It skips narrative and shows status in tables.

## Terms used here

Fact means declarative domain truth versioned in text.
Method means imperative procedure that acts on facts.
Recipe means one composable procedure with inputs, loads, rules, done criteria.
Gate means automated or human check that blocks a bad change.
OKF means Google Open Knowledge Format v0.2 as used in pi-job.

## Fact versus method in pi-job

| Question               | Answer                                                                                                                                                                                                                                        |
|------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| What is fact           | Bundle text that states what is true: `references/*.md` concepts with `type/title/status`, `references/glossary.yaml` terms, `_decision-*.md` rulings, `references/bigpicture.txt` AS-IS spine, toolbelt aid outputs, `_findings.md` evidence |
| Where does fact live   | Bundle `references/` plus `plans/` evidence; never in model weights; never only in chat                                                                                                                                                       |
| What is method         | Procedure that states how to act: `profile.yaml` packets, `slice_kinds`, `step_kinds`, `loop_packets` manager and worker, `plans/<slice>.md` contracts, `instruction` rendering, claim plus start plus finish protocol                        |
| Where does method live | Harness `profile.yaml` plus Python rendering plus per-slice plan files                                                                                                                                                                        |
| Separation test        | Ask one question: did loaded sources contain the right answer; if yes and agent failed then fix method; if no then fix fact; if experts disagree then record ambiguity                                                                        |
| Your OKF mapping       | `references/index.md` is the routing map; `glossary.yaml` is the vocabulary file; `type` is the minimal OKF identity field                                                                                                                    |

## Evaluation framework in pi-job

Evaluation means each proposed change faces replay plus regression before landing.
Replay means run the trigger case again blind and compare against expert feedback.
Regression means run the full benchmark suite and detect behavior loss.
Judge means independent scorer with no knowledge of the diff.

| Layer            | Meta                        | pi-job today                                                                                | Gap                                                      |
|------------------|-----------------------------|---------------------------------------------------------------------------------------------|----------------------------------------------------------|
| Structural lint  | Deterministic file checks   | `ReferenceKnowledgeLint`: missing `index.md`, missing `type`; `validate` prints warnings    | Warnings do not fail; no size, link, cycle checks        |
| Schema check     | File boundaries enforced    | `validate` plus Pydantic task schema                                                        | Concept schema covers only `type`; no `depends_on` graph |
| Unit tests       | Not described               | `tests/executable_test_pi_job.py`                                                           | No per-bundle replay command                             |
| Step validators  | Benchmarks per domain       | Per-step `validators` in `profile.yaml`, plus `verify`, `code-review`, `vulnerability-scan` | Validators check process evidence, not answer quality    |
| Replay           | Blind judge on trigger case | Manual; no blind replay harness                                                             | Missing replay runner plus blind judge                   |
| Regression suite | Grows with each fix         | None durable per bundle                                                                     | Missing append-on-land rule                              |
| Human gate       | Expert reviews proven diff  | `grill`, `clarify-scope`, `add-decision`, PR review                                         | Review sees raw failure, not proven fix                  |

## Concept comparison tables

### 1. Knowledge system

| Meta concept | Meta mechanism | pi-job today | Status | Next action |
|---|---|---|---|---|
| Position file | Authoritative stance with constraints and routing hints | `_decision-*.md` plus concept notes with `type` | Partial | Add `position` type with constraints and boundary cases |
| Taxonomy file | Single glossary for entity types and tiers | `references/glossary.yaml`, YAML only, stable `id` | Done | Grow from grill plus research; flip aid to done |
| Routing index | Deterministic map from input to files | `references/index.md` stub from `b7d382e`; Current plus Vocabulary sections | Started | List current concepts only; link each to file path |
| Gateway file | Threshold test before entering a domain | `grill`, `clarify-scope`, `confirm-layers` gates | Partial | Add explicit `gateway` type with entry tests |
| Dependency graph | `depends_on` plus `referenced_by` frontmatter | OKF path is identity; no graph fields | Missing | Add `depends_on` list; derive reverse index in lint |
| Density split | Dense wiki plus sparse RAG | Curated `references/` plus raw tickets, code, logs | Partial | Mark each source dense or sparse in index |
| Versioning | Text diff, review, revert | Bundle files in git; `acknowledge-edit` refreshes digest | Done | Keep all fixes as text diffs |

### 2. Reasoning layer

| Meta concept | Meta mechanism | pi-job today | Status | Next action |
|---|---|---|---|---|
| Recipe | Composable multi-step workflow | `loop_packets`, `slice_kinds`, `step_kinds`, `plans/<slice>.md` | Partial | Strip facts from packets; keep loads plus rules plus done criteria |
| Separation | Knowledge holds facts; recipe holds method | Packets mix facts with method today | Gap | Move facts to `references/`; reference by path |
| Composition | Master recipe delegates to sub-recipes | Manager packet spawns slice workers; slice runs ordered steps | Done | Keep one owner plus one slice per worker |
| Progressive disclosure | Load only phase-relevant context | `instruction` emits cursor step; `markdown --slice` loads binding decisions | Partial | Measure tokens per turn; cut front-loaded dumps |
| Failure attribution | Knowledge gap versus recipe flaw versus ambiguity | `_findings.md` plus `add-decision`; no formal test | Missing | Apply separation test on each correction |

### 3. Human control

| Meta concept | Meta mechanism | pi-job today | Status | Next action |
|---|---|---|---|---|
| Checkpoint | Pause and review intermediate reasoning | `grill`, `grill-plan`, `clarify-scope`, `confirm-layers`, `wait-for-feedback` | Done | Define checkpoints inside each recipe |
| Escalation | Hand ambiguity to expert and follow choice | `block-slice`, `msg`, `investigate`, parked claims | Done | Trigger on split evidence, not on silence |
| Trust calibration | Show reasoning, flag uncertainty | `show --short`, `markdown`, pane-tail triage | Partial | Surface knowledge manifest per step: files loaded, time used |

### 4. Self-improvement flywheel

| Meta phase | Meta mechanism | pi-job today | Status | Next action |
|---|---|---|---|---|
| Diagnose | Extract signals plus knowledge manifest; classify gap, flaw, ambiguity | `investigate` plus `add-finding` | Started | Record manifest with each finding |
| Compile | Parallel impact agents plus adversarial review plus deterministic lint | Subagents ad hoc; `code-review` with distinct model; `ReferenceKnowledgeLint` | Partial | Add fresh-context reviewer on diff only |
| Evaluate | Blind replay plus regression benchmarks; retry on fail | `verify` plus manual tests | Missing | Build replay runner plus blind judge |
| Land plus enrich | Human reviews proven diff; failing case joins suite | `add-decision`, `add-pr`, `finish --note` | Partial | Append each landed case to regression suite |

## Direct mapping for your current work

| Your change | Meta equivalent | Keep | Add |
|---|---|---|---|
| `references/index.md` template (routing index) | Routing index | Open-first rule in `instruction`; Current table with Concept \| Path \| Type \| Status \| Use; Vocabulary link; How to update rules | Current-concept list with paths; dense versus sparse marks; `position`, `gateway` types |
| Concept notes with YAML `type/title/status` | Position plus taxonomy files | Reserved `index.md` plus `log.md`; soft warnings | `position`, `gateway` types; `depends_on` field |
| `references/glossary.yaml` | Taxonomy and vocabulary files | YAML-only schema with `id`, `means`, `rejects`, `evidence` | Link each term to evidence; update on grill |
| `toolbelt` aids plus `bigpicture.txt` | AS-IS spine plus understanding aids | Aid registry with suits and validators | Reading order before `clarify-scope` |
| `pi-job-feedback` step | Training signal | Existing feedback locks | Forward each correction into diagnose phase |

## Minimal template deltas

| File | Change |
|---|---|
| `references/index.md` | Current table with Concept \| Path \| Type (`position`, `gateway`, `concept`, `evidence`) \| Status (`current`, `deprecated`) \| Use (`dense`, `sparse`); Vocabulary link; How to update rules (append row in `synthesize`, mark superseded `deprecated`, never delete) |
| Concept note | Require `type`; recommend `title`, `status`, `depends_on`; keep body short and routable |
| `references/glossary.yaml` | Keep schema; add or amend one term per grill or research finish |
| Slice plan | Add Loads list with exact reference paths; add Done criteria; add Checkpoints |
| Finding | Add manifest: files loaded, step, observed versus expected; add class: gap, flaw, ambiguity |
