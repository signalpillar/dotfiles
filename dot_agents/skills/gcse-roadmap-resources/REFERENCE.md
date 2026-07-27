# GCSE Roadmap Resource Reference

## Source Of Truth

The embedded manifest in `prototypes/gcse-science-f1-roadmap.html` is authoritative for topic IDs, subjects, titles, curriculum wording, prerequisites, checks, stages, drivers, and existing F1 or DIY examples.
Do not duplicate those curriculum facts in `topic.json`.
Official question papers and mark schemes are authoritative for paper metadata, question numbers, pages, and mappings.
The final live resource page is authoritative for its current destination, content, and access conditions.
An HTTP success alone does not establish relevance, accuracy, suitability, or freedom from account and payment gates.

## Publishable Schemas

`catalog.json` has exactly these keys and maps each topic to its manifest-derived path.

```json
{
  "schemaVersion": 1,
  "topics": {
    "M1": "topics/maths/m1/topic.json"
  }
}
```

`topic.json` has exactly these keys.
Every supporting link has exactly `kind`, `label`, `note`, `publisher`, and `url`.
Allowed kinds are `official-specification`, `lesson`, `video`, `interactive`, `practice`, and `reference`.

```json
{
  "schemaVersion": 1,
  "topicId": "M1",
  "summary": "summary.md",
  "explainPrompt": "explain-prompt.md",
  "links": [
    {
      "label": "Resource title",
      "url": "https://example.org/resource",
      "publisher": "Publisher",
      "kind": "lesson",
      "note": "Why this resource supports the topic."
    },
    {
      "label": "Practice title",
      "url": "https://example.org/practice",
      "publisher": "Publisher",
      "kind": "practice",
      "note": "What useful practice the learner will find."
    }
  ],
  "wikipedia": {
    "label": "English Wikipedia article",
    "url": "https://en.wikipedia.org/wiki/Article"
  }
}
```

Supporting URLs must be distinct, free, HTTPS, directly accessible, and separate from the required English Wikipedia URL.
Markdown files must sit beside `topic.json` and use headings up to level three, paragraphs, emphasis, lists, inline code, fenced code, and HTTPS links only.
Do not use raw HTML, images, tables, blockquotes, task lists, horizontal rules, strikethrough, or unsafe links.

`papers.json` has exactly `schemaVersion`, `nodes`, and `edges`.
A paper node has exactly the shown fields, and its two URLs must use HTTPS.

```json
{
  "schemaVersion": 1,
  "nodes": [
    {
      "id": "paper:board-series-component",
      "type": "past-paper",
      "board": "Board",
      "qualification": "Qualification code",
      "series": "Series",
      "component": "Component",
      "tier": "Tier or not applicable",
      "urls": {
        "questionPaper": "https://example.org/question-paper.pdf",
        "markScheme": "https://example.org/mark-scheme.pdf"
      }
    }
  ],
  "edges": [
    {
      "from": "paper:board-series-component",
      "to": "topic:M1",
      "questions": ["01.1", "01.2"],
      "pages": [2, 3],
      "note": "Concise explanation of the exact topic relevance."
    }
  ]
}
```

`pages` is optional and may appear only when inspected evidence establishes positive page numbers.
Question strings must be non-empty and unique within an edge.
Only one edge may connect a given paper and topic pair.
Paper edges never create prerequisites or alter readiness, progress, scores, stages, subjects, drivers, or track layout.

## Content Quality

Write concise, board-neutral GCSE revision prose that stays within inspected sources and the manifest's established scope.
Use the M1 resource only as a structural example, not as prose or link material for another topic.
Do not turn a plausible explanation into an unsupported syllabus claim.

An interactive tutor prompt must check prior knowledge, teach in short chunks, pause for answers, use only honest manifest examples, check misconceptions, test retrieval, adapt to responses, and finish with exam-style practice and feedback.
It must instruct the model not to invent facts beyond the established examples.

Choose at least two distinct credible supporting resources that directly help the learner understand or practise the topic.
Use accurate labels, publishers, kinds, and notes describing the final destination.
Reject payment gates, account requirements, misleading redirects, generic home pages without useful topic content, and resources outside the topic's established scope.

## Existing Topic Enrichment

1. Resolve the topic in the manifest and catalog.
2. Read `topic.json`, `summary.md`, and `explain-prompt.md` completely.
3. Establish each requested change from inspected sources and current pages.
4. Patch only the necessary fields or prose.
5. Preserve all unrelated topic and catalog content.
6. Run offline validation and audit changed links.

Do not invoke the topic scaffolder for this workflow.

## New Topic Or Subject

1. Look up the exact topic in the embedded manifest.
2. Run `scaffold-topic.mjs` with explicit HTML, resource root, topic ID, and temporary output paths.
3. Confirm the printed subject, catalog mapping, and destination.
4. Research and replace every `EVIDENCE_REQUIRED` placeholder.
5. Remove `draft: true` so the final record has exactly the publishable keys.
6. Add the completed three-file directory and exact catalog mapping in one patch.
7. Run validation before the networked link audit.

The scaffolder refuses cataloged topics, existing published destinations, and existing output paths.
It never publishes or edits the catalog.

## Link Refresh

1. Run the networked audit and inspect every affected final page after redirects.
2. Preserve links that remain suitable.
3. Replace only links with established access, relevance, accuracy, or quality problems.
4. Update metadata to describe the replacement destination accurately.
5. Rerun offline validation and the networked audit.

## Paper Ingestion

1. Obtain and directly inspect the official question paper and mark scheme.
2. Run `scaffold-paper.mjs` with explicit HTML, resource root, paper ID, and temporary output file paths.
3. Replace every placeholder with observed evidence and remove `draft: true`.
4. Add the reviewed node and exact question-level edges to `papers.json` without rewriting unrelated entries.
5. Add pages only when confirmed from the source.
6. Run paper graph and full resource validation.

The scaffolder does not download papers, infer topics, claim mappings, or modify `papers.json`.
One paper may support many topics, but each paper-topic pair has one edge containing all relevant question numbers.

## Commands

```sh
node dot_agents/skills/gcse-roadmap-resources/scripts/scaffold-topic.mjs --html prototypes/gcse-science-f1-roadmap.html --resources prototypes/gcse-science-f1-roadmap --topic P1 --output /tmp/p1-draft
node dot_agents/skills/gcse-roadmap-resources/scripts/scaffold-paper.mjs --html prototypes/gcse-science-f1-roadmap.html --resources prototypes/gcse-science-f1-roadmap --paper-id paper:aqa-june-2025-p1h --output /tmp/aqa-june-2025-p1h.json
npm run validate --prefix prototype.tests/gcse-science-f1-roadmap
node prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs prototypes/gcse-science-f1-roadmap
npm test --prefix prototype.tests/gcse-science-f1-roadmap
```

## Recovery

- For an unknown or duplicate topic ID, stop and inspect the manifest rather than choosing a near match.
- For a cataloged topic, switch to the existing-topic workflow and edit the authored files directly.
- For an existing output or destination, choose a new draft path or inspect the conflict without deleting or overwriting it.
- For incomplete evidence, retain the draft outside the published tree and record the gap.
- For validator failures, fix the authored data to meet the existing validator rather than weakening or duplicating its schema.
- For duplicate paper mappings, merge confirmed questions into the single existing paper-topic edge.
- For link-audit failures, inspect the final destination and replace only links proven unsuitable.
