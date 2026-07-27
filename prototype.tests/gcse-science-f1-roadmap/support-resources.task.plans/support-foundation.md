# Support Foundation Implementation Plan

## Outcome

Prove the external support-resource architecture end to end with M1 while leaving the existing 121-node curriculum manifest and prerequisite DAG unchanged.
The application will continue to use `prototypes/gcse-science-f1-roadmap.html` as its entry point, but it will require HTTP or HTTPS because topic resources are fetched from adjacent files.
The base roadmap, progress persistence, import and export, layout, and Jelly UI fallback will remain usable when an individual support resource or Marked is unavailable.

## Approach

1. Add scoped authoring rules and versioned external JSON entry points beneath `prototypes/gcse-science-f1-roadmap/`.
2. Publish one complete M1 tracer directory with declarative metadata, a short summary, and an interactive tutor prompt.
3. Extend the existing drawer with support sections that load lazily, validate before rendering, cache successful results, and contain failures to the selected topic.
4. Load and validate the catalog and paper graph independently once per page session without feeding either structure into curriculum ordering, readiness, progress, scores, stages, subjects, drivers, or track layout.
5. Render trusted but constrained Markdown through a pinned Marked CDN dependency, preserve the original prompt source for copying, and show manifest-driven unavailable states instead of falling back to unsafe HTML insertion.
6. Expand static and Playwright coverage for schema validity, M1 behavior, protocol failure, asynchronous races, dependency degradation, and existing roadmap regressions.

## Files And Functions

### Prototype Runtime

- Modify `prototypes/gcse-science-f1-roadmap.html`.
- Add a pinned Marked browser script alongside Jelly UI and Open Props, with its CDN URL represented in the test fixture routing.
- Extend `app.copy` with all support labels and states, including loading, unavailable, invalid, no papers, copy prompt, copy success, copy failure, Marked unavailable, and HTTP-required guidance.
- Extend `app.timing` only if a copy-result reset duration is needed, so no new user-facing copy or tuning value is hard-coded in JavaScript.
- Add support-resource CSS beside the existing detail drawer rules for rendered Markdown, resource links, prompt controls, status messages, and narrow-screen wrapping.
- Introduce `boot()` around normal application startup so the protocol check can render manifest-driven guidance and return before Jelly waiting, resource fetches, state loading, or roadmap rendering on unsupported protocols.
- Add `renderProtocolError()` to replace the shell with the HTTP-required message when `location.protocol` is neither `http:` nor `https:`.
- Add independent `loadSupportCatalog()` and `loadPaperGraph()` functions that each fetch and validate once per page session, so paper failure cannot suppress a valid topic summary, prompt, or links.
- Add `validateCatalog()`, `validateTopicRecord()`, `validatePaperGraph()`, and `validateMarkdownSource()` as small runtime boundary checks before fetched data reaches rendering code.
- Add `loadTopicResources(topicId)` to resolve the catalog entry, fetch `topic.json`, resolve `summary.md` and `explain-prompt.md` relative to that record, validate all three files, and cache only a fully successful result by topic ID.
- Add `renderMarkdown(source)` to reject unsupported source, require `window.marked`, call `marked.parse` with a fixed configuration, and return a fragment produced from the rendered result.
- Configure Marked for GitHub-flavored Markdown without raw HTML, and reject raw HTML tokens and non-HTTPS link destinations before assigning rendered output to a temporary template.
- Add a post-render DOM check that permits only the agreed Markdown element and attribute subset and rejects links whose parsed protocol is not `https:`.
- Add `renderSupportResources(node)` to render the topic loading placeholder immediately, then replace it with the summary, prompt preview, copy action, supporting links, and Wikipedia link for the currently selected topic while the independent paper section resolves or reports its own unavailable state.
- Add `copyExplainPrompt(source, resultNode)` to write the exact unrendered `explain-prompt.md` source through `navigator.clipboard.writeText` and show copy success or failure from manifest copy.
- Update `select(id)` and `renderDetail()` so opening a drawer starts resource rendering after the existing curriculum facts and prerequisite sections are present.
- Guard every asynchronous drawer update with the requested topic ID and a monotonically increasing selection token so a slow response cannot replace content after another topic is selected or the drawer is closed.
- Update `closeDetail()` and the drawer `close` listener to invalidate the active selection token.
- Keep `setStatus()`, `setNotes()`, `topologicalOrder()`, `relatedIds()`, readiness helpers, progress serialization, and track layout independent of support data.
- Extend `window.__ROADMAP_TEST__` only with read-only support state needed to await deterministic browser assertions, rather than exposing mutation hooks.

### Scoped Resource Rules

- Add `prototypes/gcse-science-f1-roadmap/AGENTS.md`.
- Record the three-file topic layout, lowercase path rule, explicit catalog mapping, strict schema version, two-link minimum, separate Wikipedia requirement, constrained Markdown subset, question-level paper-edge shape, HTTP-only runtime, and prohibition on changing prerequisite behavior from support files.
- State that supporting resources must be credible, free, HTTPS, directly accessible without payment or an account, and checked before publication.

### External Schema Files

- Add `prototypes/gcse-science-f1-roadmap/catalog.json` with exactly `schemaVersion: 1` and an explicit `topics` object mapping `M1` to `topics/maths/m1/topic.json`.
- Add `prototypes/gcse-science-f1-roadmap/papers.json` with exactly `schemaVersion: 1`, an empty `nodes` array, and an empty `edges` array.
- Add `prototypes/gcse-science-f1-roadmap/topics/maths/m1/topic.json` with `schemaVersion`, `topicId`, relative summary and prompt filenames, at least two supporting link records, and one separately typed Wikipedia record.
- Require every supporting link record to contain non-empty `label`, `url`, `publisher`, allowed `kind`, and `note` fields.
- Permit only `official-specification`, `lesson`, `video`, `interactive`, `practice`, and `reference` as supporting link kinds.
- Require supporting and Wikipedia URLs to parse as absolute HTTPS URLs, and do not count Wikipedia toward the two-link minimum.
- Resolve catalog paths relative to `catalog.json` and Markdown paths relative to each `topic.json`, while rejecting absolute paths, parent traversal, query strings, fragments, and paths outside the roadmap resource directory.
- Keep curriculum title, specification text, prerequisites, F1 fact, DIY fact, check, stage, subject, and driver out of `topic.json` so the embedded manifest remains the sole curriculum source.

### M1 Tracer Content

- Add `prototypes/gcse-science-f1-roadmap/topics/maths/m1/summary.md`.
- Explain standard form as `a x 10^n` with `1 <= a < 10`, movement of the decimal point, positive and negative powers, order-of-magnitude comparison, and multiplication or division by combining powers.
- Include brief worked examples based on the existing M1 values `15000`, `1.6 x 10^-3`, and `1 x 10^-5` without changing the manifest facts or claiming unsupported F1 details.
- Keep the summary board-neutral, concise enough for the drawer, and limited to headings, paragraphs, emphasis, lists, inline code, fenced code, and HTTPS links.
- Add `prototypes/gcse-science-f1-roadmap/topics/maths/m1/explain-prompt.md`.
- Make the prompt ask the tutor to check prior knowledge first, teach in short chunks, pause for learner answers, use the existing honest F1 telemetry and workshop drill-bit examples, correct misconceptions, run retrieval checks, and finish with GCSE exam-style practice plus feedback.
- Tell the tutor not to invent motorsport facts, not to reveal answers before the learner attempts them, and to adapt difficulty from the learner's responses.
- Seed `topic.json` with the Maths Genie standard-form lesson at `https://www.mathsgenie.co.uk/standard-form.html`, the Corbettmaths standard-form lesson at `https://corbettmaths.com/2012/08/10/standard-form/`, and Wikipedia at `https://en.wikipedia.org/wiki/Scientific_notation`.
- Confirm all three URLs during implementation and replace any candidate that redirects to a gate, fails, or no longer provides the described content before committing the tracer.

## Schema Validation

- Add `prototype.tests/gcse-science-f1-roadmap/tests/validate-resources.mjs` as both an importable validator module and a command-line validator.
- Have the command read the embedded curriculum manifest for known topic IDs, then validate the catalog, every cataloged topic record and Markdown file, and the paper graph without requiring all 121 topics at v0.
- Enforce exact allowed keys where the v1 schema is closed, integer `schemaVersion: 1`, unique IDs, exact catalog-to-record topic agreement, lowercase subject and topic directories, local relative file references, file existence, and no orphan files under the published topic tree.
- Reject empty required strings, unsupported link kinds, duplicate URLs, fewer than two non-Wikipedia supporting links, non-HTTPS URLs, malformed Wikipedia records, raw HTML, unsafe Markdown links, unsupported Markdown constructs, and Markdown files that are empty or unreasonably large.
- Validate paper node IDs, allowed `past-paper` type, required paper metadata and URLs, unique node IDs, edge endpoints, known `topic:<curriculum-id>` targets, non-empty unique question numbers, optional positive integer pages, and a concise non-empty relevance note.
- Accept the shipped empty paper graph while allowing non-empty fixture graphs in unit tests.
- Keep paper validation wholly separate from prerequisite validation so no support edge can satisfy or create a curriculum dependency.

## Marked Integration And Safety

- Pin one reviewed Marked release in the HTML URL and record the downloaded browser artifact with an exact checksum in `tests/fetch-fixtures.sh`.
- Route that exact CDN request to `tests/fixtures/marked.js` in Playwright, while retaining the existing rule that fetched fixtures are ignored and not committed.
- Use Marked only after source validation and never pass resource Markdown directly to `innerHTML`.
- Parse links from the rendered DOM again because source-level checks alone can miss encoded or parser-normalized destinations.
- Add `target="_blank"` and `rel="noopener noreferrer"` to accepted rendered links after validation.
- If Marked is absent, render an explicit safe unavailable state and keep the exact prompt copy action available because copying does not require Markdown rendering.
- Do not implement a fallback Markdown parser, because two parsers would expand the security surface and produce inconsistent rendering.

## HTTP-Only Behavior

- Check the protocol synchronously after parsing the embedded manifest and before invoking `boot()`.
- For `file:`, render only the manifest-defined HTTP-required title and instructions, including an example local server command and the expected HTTP URL shape.
- Do not load progress, fetch external resources, initialize tabs, or expose `window.__ROADMAP_TEST__` on the unsupported path.
- Keep `http:` valid for local development and `https:` valid for GitHub Pages.
- Treat catalog, paper, topic, and Markdown fetch failures over HTTP as contained support failures rather than reasons to stop the existing roadmap.

## Test Changes

- Modify `prototype.tests/gcse-science-f1-roadmap/package.json` so `npm run validate` runs the existing curriculum validator, the new resource validator, and the new Node schema tests before Playwright.
- Do not add an npm Markdown parser or schema package, because production behavior must exercise the browser Marked fixture and the small static validators can use Node built-ins.
- Add `prototype.tests/gcse-science-f1-roadmap/tests/support-resources.test.mjs` using `node:test` for valid M1, missing files, catalog and topic ID mismatch, path traversal, invalid link kinds, fewer than two links, Wikipedia separation, raw HTML, unsafe Markdown schemes, valid empty papers, valid many-topic paper fixtures, unknown paper endpoints, malformed question mappings, and proof that paper edges are never interpreted as prerequisites.
- Add `prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs` as an explicit networked audit command that reports original URL, redirect chain or final URL, status, content type, and likely account or payment gates for every published supporting and Wikipedia link.
- Keep the link audit outside the default offline test command, but run it for all M1 links as acceptance evidence for this slice.
- Modify `prototype.tests/gcse-science-f1-roadmap/tests/fetch-fixtures.sh` to download the pinned Marked artifact and verify its SHA-256 checksum beside Jelly UI and Open Props.
- Modify `prototype.tests/gcse-science-f1-roadmap/tests/roadmap.spec.js` so its local server safely serves the HTML and files below `prototypes/gcse-science-f1-roadmap/` with correct content types and rejects traversal.
- Extend `routeDependencies()` to independently provide or block Jelly UI, Open Props, and Marked.
- Add a normal-path browser case that opens M1, observes loading and rendered summary states, checks declared supporting and Wikipedia URLs, copies the exact prompt source, and sees the explicit empty-paper state.
- Add a rapid-switch case that delays M1 responses, opens another topic, and proves the late M1 result never appears in the new drawer.
- Add contained-failure cases for missing `topic.json`, malformed topic JSON, invalid Markdown, and catalog failure while confirming status and notes remain usable.
- Add a paper-graph failure case that proves valid summary, prompt, supporting links, and Wikipedia still render while only the paper section reports failure.
- Add a Marked-blocked case that proves the unavailable state is safe, the prompt source still copies exactly, and no unrendered Markdown is inserted as HTML.
- Add a `file://` case that opens the real HTML directly, checks the HTTP guidance, and proves the roadmap does not boot.
- Preserve and rerun the existing exact prerequisite, ancestor-blocking, degraded Jelly UI, persistence, import and export, tab, and viewport-overflow assertions over HTTP.
- Add an assertion that loading the empty and non-empty paper fixtures does not change `topologicalIds`, `ancestors`, `descendants`, `readiness`, or `actionableIds`.

## Verification Commands

Run these commands from the repository root during the later verification step.

```sh
node prototype.tests/gcse-science-f1-roadmap/tests/validate-manifest.mjs prototypes/gcse-science-f1-roadmap.html
node prototype.tests/gcse-science-f1-roadmap/tests/validate-resources.mjs prototypes/gcse-science-f1-roadmap.html prototypes/gcse-science-f1-roadmap
node --test prototype.tests/gcse-science-f1-roadmap/tests/support-resources.test.mjs
prototype.tests/gcse-science-f1-roadmap/tests/fetch-fixtures.sh
npm test --prefix prototype.tests/gcse-science-f1-roadmap
node prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs prototypes/gcse-science-f1-roadmap
```

Use Playwright evidence from the normal, Jelly-blocked, Marked-blocked, malformed-resource, rapid-switch, and `file://` paths, including zero page-level horizontal overflow at 320, 390, 768, and 1360 pixels.

## Key Tradeoffs

- The v0 catalog intentionally contains only M1, so foundation validation proves every published entry but does not pretend that all 121 topics are complete.
- The later library-integration slice will change coverage from published-subset validation to exact 121-topic validation.
- Separate files improve reviewability and incremental enrichment but deliberately remove direct filesystem opening, so explicit HTTP guidance replaces an embedded resource fallback.
- Runtime validation duplicates a small subset of the richer Node validator because fetched boundaries still need protection in the browser, but the runtime checks should remain narrow and failure-oriented.
- Successful topic bundles are cached, while failures are not cached, so temporary server errors can recover when a drawer is reopened.
- The catalog and paper graph use separate cached promises so either can fail independently, while topic bundles remain lazy so the application does not fetch hundreds of files at startup.
- Marked is preferred over a custom parser for standards-compliant rendering, while source and DOM allowlists constrain the larger parser surface.
- Prompt copying uses source Markdown rather than rendered text so the pasted tutor instructions preserve headings, lists, and emphasis exactly.
- An empty paper graph is visible rather than omitted, which proves schema and UI readiness without implying that paper research has already happened.
- Support resource errors do not disable curriculum facts or progress controls, because enrichment is additive and must not make the established roadmap less reliable.

## Risks To Recheck During Grilling

- Confirm the selected pinned Marked release and global browser API before implementation, then lock its fixture checksum.
- Confirm the three M1 candidate URLs remain directly accessible and accurately described at implementation time.
- Challenge the Markdown allowlist against the actual M1 files so it is neither broader than needed nor incompatible with the tutor prompt.
- Verify that clipboard permissions behave consistently in installed Chrome and Playwright, with a deterministic test fallback only at the browser boundary.
- Verify that wrapping current module startup in `boot()` does not change top-level timing, Jelly UI detection, or the existing test hook.
