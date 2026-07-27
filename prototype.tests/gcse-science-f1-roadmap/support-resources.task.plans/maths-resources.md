# Maths Resource Publication Plan

## Outcome

Publish complete support bundles for M2 through M20 while preserving the reviewed M1 catalog entry and its three files byte for byte.
The finished maths catalog will contain all 20 manifest maths topics, and each newly published topic will have a concise board-neutral summary, an interactive tutor prompt, two or more reviewed free supporting links, and one separate English Wikipedia link.
This slice will add content only and will not change the embedded manifest, prerequisite graph, runtime, paper graph, validators, audit script, tests, or M1 resources.

## Source Boundaries

The embedded manifest remains authoritative for each topic ID, title, specification wording, prerequisites, checks, and established F1 and DIY examples.
The M1 bundle remains the structural quality baseline, but none of its prose or links will be reused as evidence for another topic.
The final live destination of each candidate link remains authoritative for its current content, redirects, access requirements, and publisher metadata.
The existing validator remains authoritative for the closed JSON schemas, catalog paths, allowed Markdown, link kinds, and absence of orphan files.
No summary or prompt will add an exam-board requirement, curriculum claim, motorsport fact, practical fact, or formula outside inspected evidence and the topic's manifest scope.

## Research Delegation

Assign each group to one researcher so related terminology and examples can be checked together without turning the work into one unreviewable bulk generation pass.
Require every researcher to read each assigned manifest record independently and return completed drafts plus a short evidence log containing the final URL, redirect result, access check, topic relevance, and the claims supported by each source.
Require a reviewer who did not author the group to compare every draft against the manifest, inspect every final link destination, and approve each topic separately.
Do not publish a whole group merely because most topics pass review, and retain any incomplete topic as a draft outside the published tree.

### Measurement And Arithmetic

- M2 covers SI units, kilo, centi, milli, micro, consistent units, `320 km/h` to `m/s`, and `250 kW` to watts, and its prompt must diagnose factor-of-ten and squared-or-cubed unit mistakes before using only the manifest road-speed and workshop examples.
- M5 covers percentages, increase, decrease, and reverse percentages, and its prompt must distinguish percentage change from percentage points before using only the manifest lap-time and tile-order examples.
- M13 covers significant figures and measurement-supported precision, and its prompt must distinguish decimal places from significant figures and challenge false precision using only the manifest sensor and tape-measure examples.

### Algebra And Proportional Relationships

- M3 covers changing the subject with products, fractions, and squares, and its prompt must check inverse-operation and square-root mistakes before using only `F = ma`, `Ek = 0.5mv^2`, and the decking example from the manifest.
- M4 covers ratio, direct proportion, inverse proportion, and scale factors, and its prompt must separate part-to-part from part-to-whole reasoning before using only the manifest brake-bias, gear, air-fuel, and filler examples.
- M14 covers substitution with units and one-step and two-step equations, and its prompt must require units to be made consistent before using only the manifest simulator, room-area, and kinetic-energy examples.
- M18 covers rates and compound units, and its prompt must make the learner interpret the numerator and denominator before using only the manifest fuel-flow, speed, heat-transfer, pressure-change, paint-coverage, drill-speed, and pump examples.
- M19 covers direct, inverse, and inverse-square relationships in calculations and graphs, and its prompt must check constant-product and square-factor reasoning before using only the manifest air-density, field-intensity, worker-time, and radiation examples.

### Graphs And Data Presentation

- M6 covers `y = mx + c`, gradient, tangent gradient, and intercept meaning, and its prompt must make the learner attach units and context to both gradient and intercept using only the manifest telemetry, spring, board-cost, and distance-time examples.
- M7 covers estimating areas under curves with squares, triangles, and rectangles and interpreting the result, and its prompt must distinguish area under a graph from geometric area before using only the manifest velocity-time and wall-area examples.
- M16 covers tables, headings, units, scales, and independent and dependent variables, and its prompt must diagnose reversed axes and omitted units using only the manifest telemetry, drilling, and spring-extension examples.

### Statistics, Probability, And Uncertainty

- M10 covers mean, range, anomalies, and measurement uncertainty, and its prompt must avoid deleting a value without contextual justification before using only the manifest lap-time and repeated-measurement examples.
- M12 covers single and combined event probabilities and decision-making, and its prompt must establish when multiplication is justified and must not assert independence unless the question states it, while using only the manifest strategy, drilling, rain, and safety-car examples.
- M15 covers representative sampling, scatter graphs, correlation, causation, and confounding variables, and its prompt must challenge causal claims from association using only the manifest tyre-data and damp-wall examples.
- M20 covers mean and half-range uncertainty, repeatability, reproducibility, random error, and systematic error, and its prompt must distinguish tight agreement from accuracy using only the manifest sensor, worn-tape, shaky-marking, extension, and ruler examples.

### Geometry And Spatial Reasoning

- M8 covers Pythagoras and right-angled sine, cosine, and tangent, and its prompt must check side identification and calculator angle mode before using only the manifest wing-force, square-corner, and roof-pitch examples.
- M9 covers vector addition, perpendicular components, resultants, and the traction-circle constraint, and its prompt must distinguish scalar magnitudes from directions before using only the manifest tyre-grip and ladder examples.
- M11 covers surface area, volume, and their ratio as applied to transfer rates, and its prompt must compare equal-shape scale changes without making extra engineering claims beyond the manifest radiator, brake-disc, heatsink, and food examples.
- M17 covers areas and volumes of circles, cylinders, cuboids, and common shapes, and its prompt must check radius-versus-diameter and squared-versus-cubed units before using only the manifest piston, cylinder, brake-disc, paint, concrete, and circular-cutout examples.

## Drafting Workflow

1. Record a checksum or clean diff baseline for `catalog.json` and the complete `topics/maths/m1/` directory before creating any draft.
2. Run `scaffold-topic.mjs` once for each of M2 through M20 with explicit HTML, resource-root, topic-ID, and unique temporary output paths outside the published resource tree.
3. Confirm every scaffolder result reports subject `maths`, directory `m2` through `m20`, and the exact matching catalog destination before research starts.
4. Replace every `EVIDENCE_REQUIRED` placeholder, keep `draft: true` while evidence or review is incomplete, and never run the scaffolder against M1.
5. Write each summary as compact revision prose that defines the key idea, gives the needed method or relationships, works through at least one manifest-grounded example where useful, and highlights the most likely misconception without duplicating the manifest specification verbatim.
6. Keep each summary small enough to scan in the drawer, use no heading deeper than level three, and use only the Markdown constructs accepted by `validateMarkdownSource`.
7. Write each tutor prompt as an interactive session that first checks prerequisites, teaches in short chunks, pauses for answers, adapts difficulty, probes the topic-specific misconceptions listed above, performs retrieval checks, and ends with GCSE-style questions followed by marking and actionable feedback.
8. Tell every tutor not to reveal answers before an attempt and not to invent facts, examples, syllabus claims, or motorsport context beyond the selected topic's manifest record.
9. Remove `draft: true` only after the topic's prose, prompt, two supporting links, Wikipedia link, and exact destination have all passed independent review.
10. Publish each approved topic by adding exactly `topic.json`, `summary.md`, and `explain-prompt.md` beneath its lowercase topic directory and adding its one catalog mapping in the same reviewed patch.

## Link Research Strategy

Search by the exact manifest topic and the specific skill required rather than by broad terms such as GCSE maths or revision.
Prefer stable direct lesson, practice, interactive, video, reference, or official specification pages from established educational publishers, public institutions, universities, or reputable open educational projects.
For each topic, assemble at least four candidates so weak, duplicate, gated, generic, or unstable pages can be rejected without lowering the two-link minimum.
Open every candidate in a fresh browser context, follow all redirects, and verify that the final page directly teaches or practises the exact topic at GCSE-appropriate depth without requiring an account, subscription, payment, trial, or app installation.
Reject search-result pages, generic publisher home pages, generic topic indexes unless they directly expose clearly labelled material for the exact topic, snippets without instruction or practice, pages whose useful content is gated, and pages whose labels overstate their scope.
Choose at least two distinct final HTTPS URLs per topic whose useful content is genuinely complementary where possible, such as one clear explanation and one set of practice questions or an interactive activity.
Do not count an official specification or Wikipedia page as practical teaching coverage when it merely names the requirement, although a directly useful official resource may remain one of the two supporting links.
Record the exact final-page title as the label, the actual publisher, the closest allowed link kind, and a concrete note stating what the learner can understand or practise there.
Select a separate English Wikipedia article only when its subject is directly relevant to the mathematical concept, and prefer a focused article over a broad mathematics portal or a forced near-match.
If no honest focused Wikipedia article exists, stop that topic for review rather than using a misleading article or silently weakening the requirement.
Run the audit script after provisional links are entered, but treat its status, redirect, content-type, and likely-gate output as a prompt for manual reinspection rather than proof of quality.
Replace only candidates demonstrated to be inaccessible, gated, misleading, off-topic, or unsuitable, and rerun both manual inspection and the audit after replacement.

## Exact Publication Files

Preserve `prototypes/gcse-science-f1-roadmap/topics/maths/m1/topic.json`, `summary.md`, and `explain-prompt.md` without modification.
Add exactly three files under each of `prototypes/gcse-science-f1-roadmap/topics/maths/m2/` through `prototypes/gcse-science-f1-roadmap/topics/maths/m20/`.
Name those three files `topic.json`, `summary.md`, and `explain-prompt.md` in every new directory.
Edit only `prototypes/gcse-science-f1-roadmap/catalog.json` in addition to those 57 new files.
Keep `schemaVersion` equal to `1` and preserve the existing M1 mapping exactly.

## Exact Catalog Edits

- Add M2 as `topics/maths/m2/topic.json`.
- Add M3 as `topics/maths/m3/topic.json`.
- Add M4 as `topics/maths/m4/topic.json`.
- Add M5 as `topics/maths/m5/topic.json`.
- Add M6 as `topics/maths/m6/topic.json`.
- Add M7 as `topics/maths/m7/topic.json`.
- Add M8 as `topics/maths/m8/topic.json`.
- Add M9 as `topics/maths/m9/topic.json`.
- Add M10 as `topics/maths/m10/topic.json`.
- Add M11 as `topics/maths/m11/topic.json`.
- Add M12 as `topics/maths/m12/topic.json`.
- Add M13 as `topics/maths/m13/topic.json`.
- Add M14 as `topics/maths/m14/topic.json`.
- Add M15 as `topics/maths/m15/topic.json`.
- Add M16 as `topics/maths/m16/topic.json`.
- Add M17 as `topics/maths/m17/topic.json`.
- Add M18 as `topics/maths/m18/topic.json`.
- Add M19 as `topics/maths/m19/topic.json`.
- Add M20 as `topics/maths/m20/topic.json`.

## Review Gates

### Topic Gate

Each topic must independently pass an exact manifest-scope review, summary review, interactive-prompt review, JSON metadata review, manual final-destination review for all three links, and strict resource validation before publication.
The reviewer must confirm that the summary is mathematically accurate, board-neutral, concise, useful in the drawer, and free of unsupported scope expansion.
The reviewer must run the prompt mentally as a session and confirm that it waits for responses, adapts, handles the named misconceptions, uses only honest manifest examples, and supplies exam-style marking and feedback.
The reviewer must confirm that supporting links are distinct from one another and Wikipedia, accurately labelled, directly relevant, free, ungated, HTTPS, and described by truthful notes.

### Group Gate

After all topics in a research group pass the topic gate, validate the whole currently published library to detect path errors, duplicate URLs within records, unsupported Markdown, draft markers, or orphan files introduced by the group.
Audit all currently cataloged links and manually recheck any redirect, unexpected content type, likely gate, HTTP error, or network error before accepting the group.
Review the group diff for accidental edits outside its new topic directories and intended catalog mappings.

### Subject Gate

After all five groups pass, require exactly 20 cataloged maths IDs matching M1 through M20 and exactly three published files for every maths topic directory.
Confirm that no topic record duplicates curriculum title, specification, prerequisites, stage, driver, check, F1, or DIY fields from the manifest.
Compare the M1 checksum or path-specific diff against the baseline and reject the slice if any M1 byte changed.
Confirm that `papers.json`, the HTML manifest and runtime, validators, audit script, test files, and non-maths resource paths have no slice diff.

## Validation And Link Audit

Run the following commands from the repository root after each group where practical and again for the complete maths publication.

```sh
node prototype.tests/gcse-science-f1-roadmap/tests/validate-manifest.mjs prototypes/gcse-science-f1-roadmap.html
npm run validate --prefix prototype.tests/gcse-science-f1-roadmap
node prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs prototypes/gcse-science-f1-roadmap
prototype.tests/gcse-science-f1-roadmap/tests/fetch-fixtures.sh
npm test --prefix prototype.tests/gcse-science-f1-roadmap
git diff --check
```

The manifest validator must continue to report 121 nodes, 189 prerequisite edges, and exactly 20 maths nodes.
The resource validator must report 20 published topics, zero papers, and zero paper edges at the end of this slice.
The link audit must emit three successful inspected destinations for each of the 20 maths topics, for a total of at least 60 audited URLs, with no unresolved likely gate or failed status.
The full test command must preserve the existing schema, scaffolder, runtime failure-containment, persistence, prerequisite, degraded-dependency, and responsive-layout coverage.

## Browser Representative Checks

Use the existing local HTTP Playwright server and locally routed Jelly UI, Open Props, and Marked fixtures rather than live CDN requests.
Open M1 first and confirm its summary text, exact three links, exact prompt copying, and empty-paper state still match the reviewed tracer.
Open M2 as the measurement representative, M3 as the algebra representative, M6 as the graph representative, M12 as the probability representative, M15 as the statistics representative, M17 as the geometry representative, and M20 as the uncertainty representative.
For every representative, confirm the support state reaches `ready`, the intended summary renders without raw Markdown, exactly the declared supporting and Wikipedia links appear, the copied clipboard text equals that topic's full `explain-prompt.md`, and the paper section remains independently empty.
At widths 320, 390, 768, and 1360 pixels, inspect one short topic and one content-dense topic and require zero page-level horizontal overflow, readable wrapped links, reachable copy controls, and a usable drawer.
Repeat one non-M1 representative with Marked blocked and confirm previews fail safely while exact prompt copying remains available.
Rapidly switch from a deliberately delayed new maths topic to another topic and confirm stale resource responses cannot replace the active drawer.
Capture page errors, failed local resource requests, incorrect support state, clipboard mismatch, overflow, or inaccessible controls as failures rather than visual caveats.

## Tradeoffs And Risks

Parallel research is bounded by coherent topic groups, but publication remains per-topic so one evidence gap cannot pressure reviewers to approve an entire batch.
Two links per topic is a minimum rather than a target for padding, because two excellent complementary resources are preferable to additional weak or repetitive links.
Provider concentration may make research faster but creates correlated link-rot risk, so use publisher diversity where quality is comparable without accepting inferior resources merely to diversify.
The audit script detects status and obvious gate patterns but cannot prove educational relevance, page quality, or hidden access barriers, so manual browser inspection remains a release gate.
Wikipedia coverage may be awkward for narrowly combined curriculum topics, so focused component articles must be chosen honestly and unresolved near-matches must block publication for review.
The existing browser suite hard-codes M1 as its full support tracer, so representative M2 through M20 checks provide acceptance evidence without weakening or rewriting the reviewed M1 assertions.
External sites can change between review and publication, so save final audit output and complete the manual spot-check as close to publication as practical.
