# Tab Dedupe

Brave/Chrome MV3 extension. Opening a URL that is already open in another tab focuses that tab and closes the new one.

## Install (dev mode)
1. `brave://extensions` -> enable **Developer mode**
2. **Load unpacked** -> select this folder
3. Reload the extension after edits. Settings: extension **Details -> Extension options**.

## Behavior
- New tabs only by default; "also dedupe on navigation" is an option.
- Ignores #fragment, `www.`, trailing slash and tracking params (configurable).
- Only http(s) tabs; never mixes incognito and regular tabs.
- Incognito: enable "Allow in Private" on the extension if wanted.

## Tests
`node --test proj/brave-tab-dedupe/url.test.mjs`
