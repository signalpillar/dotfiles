# Shared Shell Test Instructions

Run commands from the repository root.

- These tests need the `nostr-tools` fixture: run `prototype.tests/nostr/tests/fetch-fixtures.sh` first.
- Install with `npm install --prefix prototype.tests/shell`.
- Run with `npm test --prefix prototype.tests/shell`.
  The run validates the `about.html` manifest and parses its page script before any browser starts.
- The test server serves the whole `prototypes/` folder.
  A page URL with `?noshell` gets an empty `shell.js`, which gives the baseline for the same page without the bar.
- All third-party requests are blocked, except the `nostr-tools` fixture.
  The page-by-page test therefore compares the page with and without the bar; it does not judge a page's own CDN fallback.
- A new page in `prototypes/` is picked up automatically. If it fails here, check that it loads `shell.js`.
