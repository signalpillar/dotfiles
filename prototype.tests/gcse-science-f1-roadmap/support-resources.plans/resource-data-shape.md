# GCSE Roadmap Support Resource Shape

## Files

```text
prototypes/gcse-science-f1-roadmap/
├── AGENTS.md
├── catalog.json
├── papers.json
└── topics/
    └── <subject>/
        └── <lowercase-topic-id>/
            ├── topic.json
            ├── summary.md
            └── explain-prompt.md
```

The existing `prototypes/gcse-science-f1-roadmap.html` remains the application entry point.
The multi-file application requires HTTP and does not support direct `file://` opening.

## Catalog

`catalog.json` is the entry point for external topic resources.
It contains a schema version and an explicit map from every published curriculum topic ID to its resource record.

```json
{
  "schemaVersion": 1,
  "topics": {
    "M1": "topics/maths/m1/topic.json"
  }
}
```

The catalog is explicit rather than path-derived so validation can detect missing topics, orphan files, case mistakes, and future path migrations.

## Topic Record

```json
{
  "schemaVersion": 1,
  "topicId": "M1",
  "summary": "summary.md",
  "explainPrompt": "explain-prompt.md",
  "links": [
    {
      "label": "Resource title",
      "url": "https://example.org/topic",
      "publisher": "Publisher",
      "kind": "lesson",
      "note": "Why this resource is useful."
    }
  ],
  "wikipedia": {
    "label": "Scientific notation",
    "url": "https://en.wikipedia.org/wiki/Scientific_notation"
  }
}
```

Every topic requires at least two credible HTTPS links that are free and directly accessible without an account.
Allowed link kinds are `official-specification`, `lesson`, `video`, `interactive`, `practice`, and `reference`.
Wikipedia is stored separately and does not count toward the two-link minimum.
Topic JSON never duplicates title, specification text, prerequisites, F1 facts, DIY facts, or checks from the curriculum manifest.

## Markdown

`summary.md` contains a short board-neutral explanation suitable for GCSE revision.
`explain-prompt.md` contains a complete copy-paste prompt for an interactive tutor session.
Marked renders both files in the browser.
Validation rejects raw HTML and unsafe link schemes even though repository content is trusted.

## Paper Support Graph

`papers.json` stores paper nodes and question-level support edges independently from the prerequisite DAG.

```json
{
  "schemaVersion": 1,
  "nodes": [
    {
      "id": "paper:aqa-8463-2025-june-p1h",
      "type": "past-paper",
      "board": "AQA",
      "qualification": "8463",
      "series": "June 2025",
      "component": "Paper 1",
      "tier": "Higher",
      "urls": {
        "questionPaper": "https://example.org/question-paper.pdf",
        "markScheme": "https://example.org/mark-scheme.pdf"
      }
    }
  ],
  "edges": [
    {
      "from": "paper:aqa-8463-2025-june-p1h",
      "to": "topic:P14",
      "questions": ["03.1", "03.2"],
      "pages": [12, 13],
      "note": "Specific heat capacity calculation and method evaluation."
    }
  ]
}
```

Paper nodes may support many topics, and topics may have no paper edges.
Paper edges never participate in readiness, topological ordering, progress, driver points, stages, subjects, or track layout.
The first delivery ships an empty but valid paper graph.

## Runtime Invariants

- Load the catalog and paper graph once and cache them.
- Load topic JSON and its two Markdown files only when its drawer opens.
- Cache successfully loaded topic resources by topic ID.
- Ignore an asynchronous result when the selected topic changed while it was loading.
- Render loading, unavailable, invalid, empty-paper, and copy-result states from manifest copy.
- Fail clearly before application boot when the page protocol is not HTTP or HTTPS.
- Keep resource knowledge out of application JavaScript.
