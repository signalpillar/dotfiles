# Launcher (index.html) Test Instructions

Run commands from the repository root.

- These tests need the `nostr-tools` fixture: run `prototype.tests/nostr/tests/fetch-fixtures.sh` first.
- Install with `npm install --prefix prototype.tests/index`.
- Run with `npm test --prefix prototype.tests/index`.
- `validate-index.mjs` runs first and reads the page code of every prototype:
  - every `prototypes/*.html` page except `index.html` and `about.html` must have one manifest entry;
  - the group must match the code: `nostr/store.js` means `synced`, `localStorage` or `indexedDB` means `browser`, else `none`;
  - a page that calls `getUserMedia` needs `microphone`, and one that uses `navigator.gpu` needs `webgpu`.
- When you add a prototype, add its entry in the `index.html` manifest; the validator tells you what is missing.
