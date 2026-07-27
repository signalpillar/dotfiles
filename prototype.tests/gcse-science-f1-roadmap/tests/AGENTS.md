# GCSE Roadmap Test Instructions

Run commands from the repository root.

- Validate the manifest with `node prototype.tests/gcse-science-f1-roadmap/tests/validate-manifest.mjs prototypes/gcse-science-f1-roadmap.html`.
- Fetch browser fixtures once with `prototype.tests/gcse-science-f1-roadmap/tests/fetch-fixtures.sh`.
- Install the isolated test dependency with `npm install --prefix prototype.tests/gcse-science-f1-roadmap`.
- Run browser tests with `npm test --prefix prototype.tests/gcse-science-f1-roadmap`.
- The browser suite uses installed Google Chrome when available and otherwise uses Playwright's configured Chromium.
- Keep prerequisite expectations exact.
  Do not weaken them to counts or partial membership checks.
