# GCSE Roadmap Resource Instructions

- Keep each topic under `topics/<lowercase-subject>/<lowercase-topic-id>/` with exactly `topic.json`, `summary.md`, and `explain-prompt.md`.
- Add every published topic through an explicit `catalog.json` mapping and keep `schemaVersion` at exactly `1` until a coordinated migration.
- Require at least two supporting links plus one separate Wikipedia link per topic.
- Supporting links must be credible, free, HTTPS, directly accessible without payment or an account, and checked before publication.
- Supporting link kinds are `official-specification`, `lesson`, `video`, `interactive`, `practice`, or `reference`.
- Markdown may use headings, paragraphs, emphasis, lists, inline code, fenced code, and HTTPS links only.
- Do not use raw HTML, images, tables, blockquotes, or unsafe link schemes in resource Markdown.
- Model papers in `papers.json` as `past-paper` nodes with question-level edges, optional positive page numbers, and concise relevance notes.
- Paper support edges never change curriculum prerequisites, readiness, progress, scores, stages, subjects, drivers, or track layout.
- Keep curriculum titles, specification text, prerequisites, F1 facts, DIY facts, checks, stages, subjects, and drivers in the embedded manifest only.
- The multi-file runtime supports HTTP and HTTPS only. Do not add a `file://` resource fallback.
