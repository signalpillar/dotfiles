# GCSE Roadmap Resource Skill Implementation Plan

## Outcome

Create a repository skill that repeats the proven M1 workflow without treating generated text as curriculum evidence.
The skill will support enriching an existing topic, preparing a new topic in any manifest subject, refreshing stale links, and later adding question-level paper mappings.
All generation will be draft-only and non-destructive, while publication will remain an explicit reviewed edit followed by the existing project validators.

## Approach

1. Keep the main skill concise and route schema detail, evidence rules, examples, and troubleshooting to one reference file.
2. Add dependency-free Node scripts that create deterministic topic and paper drafts outside the published resource tree.
3. Make each script validate all inputs before writing, create output atomically, and refuse any output path that already exists.
4. Require an agent to research and review every educational claim, URL, paper field, question number, page, and relevance note before publishing a draft.
5. Use the embedded roadmap manifest as the only source for topic IDs, subjects, curriculum wording, and existing F1 or DIY examples.
6. Keep `validate-resources.mjs` authoritative for publishable schema behavior and exercise generated fixtures through it rather than creating a competing validator.

## Files And Responsibilities

### Concise Skill Entry Point

- Add `prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/SKILL.md` with valid frontmatter, a trigger-rich third-person description, and fewer than 100 physical lines.
- Cover the required read-first files, source-of-truth rules, workflow selection, safe editing rule, validation commands, and the link to `REFERENCE.md`.
- Tell the agent to inspect the current manifest entry and existing resource files before deciding whether the request is an enrichment, a new topic, a link refresh, or paper ingestion.
- Tell the agent never to invent syllabus coverage, board requirements, F1 facts, DIY facts, paper metadata, question mappings, or link accessibility.
- Tell the agent never to regenerate or overwrite an existing topic directory, catalog entry, paper node, or paper edge.
- Require exact preservation of unrelated catalog entries, topic fields, paper nodes, and paper edges when publishing a focused change.
- Require an explicit evidence gap instead of a guessed value when a source cannot establish a fact.

### Detailed Reference

- Add `prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/REFERENCE.md` as the detailed operating contract.
- Document the catalog, topic record, constrained Markdown, paper node, and paper edge schemas exactly as enforced by `validate-resources.mjs`.
- Document the evidence hierarchy, with the embedded manifest authoritative for roadmap facts, inspected official material authoritative for paper metadata, and live page inspection required for resource usefulness and access claims.
- Document the M1 tracer as a structural example only, so its prose and links are not copied into unrelated topics.
- Define summary quality as concise, board-neutral GCSE revision prose that stays within established source material and the allowed Markdown subset.
- Define the tutor prompt contract as prior-knowledge checks, short interactive chunks, pauses for answers, honest manifest examples only, misconception checks, retrieval, adaptation, and exam-style practice with feedback.
- Define link selection and refresh rules, including at least two distinct free HTTPS supporting resources, one separate English Wikipedia URL, accurate labels and notes, and rejection of account or payment gates.
- Explain that an HTTP success alone does not prove relevance, accuracy, freedom from gates, or suitability, so the agent must inspect the final destination after redirects.
- Define four workflows for existing-topic enrichment, new-topic publication, link refresh, and paper ingestion.
- For existing topics, require reading all three files and applying the smallest field-level or prose edit without invoking the topic scaffolder.
- For new topics and new subjects, require manifest lookup, draft scaffolding outside `prototypes/gcse-science-f1-roadmap/topics`, research and completion, then one reviewed patch that adds the three files and exact catalog mapping.
- For link refreshes, require running the existing audit, checking redirected content manually, replacing only failed, gated, misleading, or unsuitable links, and preserving good links.
- For paper ingestion, require direct inspection of both the question paper and mark scheme, exact question-level mappings, optional pages only when confirmed, and one edge per paper and topic pair.
- State that paper edges are support relationships only and must never be translated into prerequisites or other curriculum state.
- Include concrete command examples for both scaffolders, project validation, networked link audit, and the browser suite.
- Include recovery guidance for unknown topic IDs, conflicting destinations, incomplete evidence, validator failures, duplicate mappings, and link-audit failures.

### Topic Draft Scaffolder

- Add `prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/scripts/scaffold-topic.mjs` as an importable module and command-line program using Node built-ins only.
- Accept explicit `--html`, `--resources`, `--topic`, and `--output` arguments so the script works from either the chezmoi source tree or an applied dotfiles checkout.
- Parse the embedded manifest, require exactly one matching topic ID, derive the lowercase subject and topic directory from that manifest entry, and reject unknown or ambiguous IDs.
- Read the current catalog and reject a topic that is already cataloged or whose published destination already exists.
- Produce exactly `topic.json`, `summary.md`, and `explain-prompt.md` in the requested draft output directory.
- Add `draft: true` as an explicit machine-readable marker that the strict publication schema rejects, and make every evidence placeholder conspicuous rather than inserting plausible syllabus prose, links, publishers, examples, or Wikipedia targets.
- Print the exact eventual catalog mapping and published destination as guidance without modifying `catalog.json` or the resource tree.
- Validate every precondition before creating files, write through a temporary sibling directory, rename only after all files succeed, and clean up its own temporary directory after an error.
- Refuse an existing output path regardless of whether its contents appear compatible, because silent reuse would risk overwriting authored work.
- Export the pure manifest lookup and draft rendering functions needed for deterministic unit tests without adding a general framework.

### Paper Draft Scaffolder

- Add `prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/scripts/scaffold-paper.mjs` as an importable module and command-line program using Node built-ins only.
- Accept explicit `--html`, `--resources`, `--paper-id`, and `--output` arguments.
- Require a lowercase `paper:` identifier, reject an identifier already present in `papers.json`, and refuse an existing output path.
- Emit one standalone draft JSON document with `draft: true`, one paper node template, and one question-level edge template that both reference the requested paper ID; this draft wrapper is intentionally not the publishable paper-graph shape.
- Leave board, qualification, series, component, tier, URLs, topic ID, questions, pages, and relevance note as conspicuous evidence-required placeholders.
- Keep `pages` out of the initial edge template so an agent adds it only when the source establishes page numbers.
- Do not modify `papers.json`, infer a topic from paper text, download a paper, or claim that a question maps to a topic.
- Write atomically and export small pure rendering functions for deterministic tests.

### Script Tests And Validator Integration

- Add `prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/tests/scaffold-scripts.test.mjs` using `node:test`, temporary directories, and fixture manifests and catalogs.
- Test that topic scaffolding derives `maths/m1` and a second subject correctly, emits `draft: true`, produces byte-identical output for identical inputs, and never changes the source catalog.
- Test rejection of missing arguments, unknown topic IDs, already cataloged topics, pre-existing published destinations, pre-existing output paths, malformed manifests, and malformed catalogs.
- Test that failures leave no partial output or temporary sibling directory.
- Test that paper scaffolding is deterministic, emits `draft: true`, omits unverified pages, preserves the requested paper ID, and never changes the source paper graph.
- Test rejection of malformed paper IDs, duplicate paper IDs, malformed paper graphs, and pre-existing output paths.
- Complete generated drafts inside test-only temporary fixtures with known synthetic facts and URLs, publish those fixtures into temporary resource trees, and pass them through the real `validateResourceLibrary` export.
- Add a test that merges a completed synthetic paper draft into a temporary graph and passes it through `validatePaperGraphData` with question-level edges while producing no prerequisite fields.
- Add structural assertions that `SKILL.md` remains below 100 lines, its frontmatter names the skill correctly, and its reference link resolves one level deep.
- Modify `prototype.tests/gcse-science-f1-roadmap/package.json` so `npm run validate` runs the new scaffold tests after the current manifest, resource-library, and schema tests.
- Do not add an npm dependency, duplicate the project schema in tests, or put generated drafts and fixtures in the repository.

## Publication Workflows

### Existing Topic

1. Resolve the topic in the embedded manifest and catalog, then read its current JSON, summary, and tutor prompt.
2. Establish the requested changes from cited sources and current live links.
3. Patch only the established files and fields, then run offline validation and the relevant link audit.

### New Topic Or Subject

1. Run the topic scaffolder into a new temporary draft path and verify the script-derived subject and destination.
2. Research the topic, replace every evidence-required placeholder, and keep curriculum facts in the manifest rather than duplicating them into `topic.json`.
3. Add the completed three-file directory and exact catalog entry together, then run the project validator before auditing links.

### Link Refresh

1. Run the networked audit and inspect each affected final page rather than relying only on status codes.
2. Preserve suitable links and replace only entries with established access, relevance, or quality problems.
3. Update labels, publishers, kinds, and notes to describe the final destination accurately, then rerun validation and the audit.

### Paper Ingestion

1. Scaffold a new paper draft only after obtaining the official question paper and mark scheme.
2. Inspect both documents and replace placeholders with observed metadata and exact question mappings.
3. Merge the reviewed node and edges into `papers.json` without reordering or rewriting unrelated entries, then run the paper and full-library validators.

## Verification Commands

Run these commands from the repository root during the later verification step.

```sh
node --test prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/tests/scaffold-scripts.test.mjs
npm run validate --prefix prototype.tests/gcse-science-f1-roadmap
node prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs prototypes/gcse-science-f1-roadmap
npm test --prefix prototype.tests/gcse-science-f1-roadmap
git diff --check
```

The script tests and offline validators must pass without network access.
The link audit remains explicitly networked and must report a successful, relevant, ungated final destination for every changed link.
The browser suite should be rerun because skill-driven publications change data consumed by the existing drawer even though this slice does not change the runtime.

## Key Tradeoffs

- Draft generation is separate from publication because a schema-valid placeholder could still publish false educational content or dead links.
- The scripts refuse existing resources instead of offering force or merge modes because authored Markdown and reviewed mappings must not be replaced mechanically.
- Subject paths come from the manifest rather than a fixed subject list, so a legitimate future subject works without weakening topic-ID validation.
- The scripts provide structure but no domain prose because deterministic generation cannot establish syllabus truth or paper relevance.
- The existing validator remains the schema authority, while script-local checks cover only safe command execution and conflict prevention.
- Paper drafts begin with one edge template for discoverability, but final ingestion may contain many reviewed topic edges for one paper.
- Network auditing is separate from offline validation because reproducible tests should not depend on third-party availability.

## Risks To Recheck During Grilling

- Verify that the explicit `draft: true` marker is rejected by the publication validator and must be removed as part of deliberate review.
- Confirm that the chosen temporary-output workflow is ergonomic for subject-sized batches without encouraging bulk unreviewed generation.
- Confirm that importing the project validator from skill tests remains stable when the skill is applied from chezmoi into the home directory.
- Confirm that strict refusal for an already cataloged topic still gives enough guidance for the existing-topic enrichment workflow.
- Confirm that paper placeholder shape is useful before real paper ingestion begins and cannot be mistaken for reviewed mapping evidence.
