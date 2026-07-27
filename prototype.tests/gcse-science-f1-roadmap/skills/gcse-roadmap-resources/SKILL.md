---
name: gcse-roadmap-resources
description: Creates and maintains reviewed support resources and past-paper mappings for the GCSE Science F1 Roadmap. Use when enriching roadmap topics, adding topic summaries or tutor prompts, refreshing educational links, or ingesting question-level past-paper evidence.
---

# GCSE Roadmap Resources

Build support material from inspected evidence without changing the curriculum prerequisite graph.

## Read First

1. Read `prototypes/gcse-science-f1-roadmap/AGENTS.md`.
2. Read the embedded manifest entry in `prototypes/gcse-science-f1-roadmap.html`.
3. Read `prototypes/gcse-science-f1-roadmap/catalog.json` and `papers.json`.
4. Read all existing files for the affected topic.
5. Read [REFERENCE.md](REFERENCE.md) before researching or publishing resources.

## Choose A Workflow

- Enrich an existing topic with the smallest field-level or prose edit.
- Scaffold a new topic draft outside the published resource tree.
- Refresh only links proven stale, gated, misleading, or unsuitable.
- Scaffold and ingest a paper only after inspecting its question paper and mark scheme.

Never run the topic scaffolder for an existing catalog entry.
Never regenerate or overwrite an existing topic directory, catalog entry, paper node, or paper edge.

## Evidence Rules

- Treat the embedded manifest as authoritative for topic IDs, subjects, curriculum wording, and existing F1 or DIY examples.
- Treat inspected official material as authoritative for paper metadata and question mappings.
- Inspect each resource's final live destination after redirects before claiming it is useful, relevant, free, or ungated.
- Never invent syllabus coverage, board requirements, examples, paper details, mappings, or accessibility claims.
- Record an evidence gap instead of guessing.

## Scaffold Drafts

```sh
node prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/scripts/scaffold-topic.mjs --html prototypes/gcse-science-f1-roadmap.html --resources prototypes/gcse-science-f1-roadmap --topic M2 --output /tmp/m2-draft
node prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/scripts/scaffold-paper.mjs --html prototypes/gcse-science-f1-roadmap.html --resources prototypes/gcse-science-f1-roadmap --paper-id paper:board-series-component --output /tmp/paper-draft.json
```

Drafts contain `draft: true` and conspicuous placeholders.
Research every placeholder, remove the draft marker, and review the complete result before publication.
The scripts never modify the catalog, topic tree, or paper graph.

## Publish Safely

- Preserve unrelated catalog entries, topic fields, paper nodes, and paper edges exactly.
- Add a new topic's three files and exact catalog mapping in one reviewed patch.
- Merge paper nodes and one edge per paper-topic pair without adding prerequisite fields.
- Keep paper edges as support relationships only.

## Validate

```sh
npm run validate --prefix prototype.tests/gcse-science-f1-roadmap
node prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs prototypes/gcse-science-f1-roadmap
npm test --prefix prototype.tests/gcse-science-f1-roadmap
git diff --check
```

Offline validation is authoritative for schema and file layout.
The networked audit does not replace manual inspection of relevance, accuracy, redirects, or access gates.
