# LLM Lifecycle Timelines Test Instructions

Run commands from the repository root.

- Validate the manifest and the YAML model with `node prototype.tests/llm-lifecycle-timelines/tests/validate-model.mjs prototypes/llm-lifecycle-timelines.html prototypes/llm-lifecycle-timelines.yaml`.
- Fetch browser fixtures once with `prototype.tests/llm-lifecycle-timelines/tests/fetch-fixtures.sh`.
- Install the isolated test dependency with `npm install --prefix prototype.tests/llm-lifecycle-timelines`.
- Run browser tests with `npm test --prefix prototype.tests/llm-lifecycle-timelines`.
- Assertions read expected stage names, summaries, and note counts from the YAML file.
  Keep them derived from the YAML rather than hard-coding copy in the spec.
- The note badge test covers a real regression: badges counted overlay notes that the detail panel never rendered.
  Do not weaken it to a spot check on one stage.
- Compact layout must expand detail under the selected stage.
  Wide layout must keep detail in `#detail-pane`.
  Do not regress to stacking detail under the whole timeline on phones.
