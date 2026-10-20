# Fact Graph Test Instructions

Run commands from the repository root.

- Validate the manifest with `node prototype.tests/fact-graph/tests/validate-manifest.mjs prototypes/fact-graph.html`.
  It checks that every `C.*` copy key, colour, graph style, element id and storage key the code reads exists.
- Fetch browser fixtures once with `prototype.tests/fact-graph/tests/fetch-fixtures.sh`.
  The script pins each CDN file by SHA-256.
- Install the isolated test dependency with `npm install --prefix prototype.tests/fact-graph`.
- Run browser tests with `npm test --prefix prototype.tests/fact-graph`.
- Set `CHROME_PATH` if Chromium is not at `/opt/pw-browsers/chromium-1194/chrome-linux/chrome`.
- Node taps use real mouse clicks at the Cytoscape rendered position.
  Call `settle()` first, because fcose animates and an early click misses the node.
- Expected copy comes from the manifest.
  Do not hard-code user-facing strings in the spec.
- The phone tests cover a real regression: the bottom sheet covered the Add button.
  Do not remove the check that focusing the entry closes the sheet.
- CDN routes use `page.context().route()`, because the query worker loads DataScript with `importScripts()`.
- The example-query test runs every example from the manifest and expects rows.
  A new example must return rows on the seed data with `ews` selected.
- The timeout test loads `/fact-graph-timeout.html`, which the test server serves with `timeoutMs` set to 1.
- The Nostr sync tests use the shared kit in `prototype.tests/nostr/tests`: the fake relay and the `nostr-tools` fixture.
  Run `npm install --prefix prototype.tests/nostr` and `prototype.tests/nostr/tests/fetch-fixtures.sh` first.
- A sync test points the page at the fake relay with `open(page, { relayUrl })`. Tests never contact public relays.
