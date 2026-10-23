# Nostr Layer Test Instructions

Run commands from the repository root.

- Fetch the `nostr-tools` fixture once with `prototype.tests/nostr/tests/fetch-fixtures.sh`.
  The script pins the bundle by SHA-256.
- Install the test dependencies with `npm install --prefix prototype.tests/nostr`.
- Run the tests with `npm test --prefix prototype.tests/nostr`.
- `harness.html` is a minimal page with one record and one log collection.
  The test server serves it next to `prototypes/nostr/store.js` and `account.js`.
- `fake-relay.js` is an in-memory relay. Other prototype tests reuse it, for example `prototype.tests/fact-graph`.
  It verifies signatures and keeps one event per addressable slot, like a real relay.
- Each test opens a fresh browser context as one device.
  An init script sets `window.NOSTR_PROTOTYPES_CONFIG` to the fake relay and short retry times.
  Tests never contact public relays.
- When you change `store.js` or `account.js`, also run the tests of every prototype that uses them.
