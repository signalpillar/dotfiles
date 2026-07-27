# GCSE Roadmap Support Resource Test Cases

| Case | v0 foundation | v1 subject batches | v2 integration |
|---|---|---|---|
| Embedded curriculum manifest still validates | Required | Required | Required |
| External catalog parses as strict JSON | Required | Required | Required |
| Published catalog topic resolves to known curriculum ID | Required | Required | Required |
| Topic JSON, summary, and prompt resolve | M1 exact | Every added subject topic | All 121 exact |
| Topic record ID agrees with catalog and directory | Required | Required | Required |
| Two free/open HTTPS supporting links exist | M1 exact | Every added subject topic | All 121 exact |
| Wikipedia HTTPS URL exists | M1 exact | Every added subject topic | All 121 exact |
| Raw HTML and unsafe Markdown links are rejected | Required | Required | Required |
| Empty paper graph validates | Required | Required | Required |
| Paper node and question edge fixtures validate | Required in validator tests | Required | Required |
| Paper edges never alter prerequisite results | Required | Required | Required |
| HTTP page loads resource summary with Marked | M1 exact | Representative topic | Four representative topics |
| Prompt copy produces exact Markdown source | M1 exact | Representative topic | Four representative topics |
| Supporting and Wikipedia links use declared URLs | M1 exact | Representative topic | Four representative topics |
| Empty paper state is explicit | Required | Required | Required |
| Rapid topic switching cannot show stale resources | Required | Required | Required |
| Missing or malformed topic resource is contained | Required | Required | Required |
| `file://` opening fails with clear guidance | Required | Required | Required |
| Jelly UI normal path works with local CDN fixtures | Required | Required | Required |
| Jelly UI and Open Props blocked path remains usable over HTTP | Required | Required | Required |
| Marked unavailable state is explicit and safe | Required | Required | Required |
| Progress persistence and import/export remain unchanged | Required | Required | Required |
| No horizontal overflow at required viewport widths | Required | Representative rerun | Required |
| External URL audit reports redirects, failures, and gates | M1 links | Subject links | Every published link |

## Acceptance Levels

`v0` proves the architecture with one topic and does not claim complete topic coverage.
`v1` makes each subject slice independently useful and validates every topic published by that slice.
`v2` rejects missing resources and is the completion gate for all 121 topics.
