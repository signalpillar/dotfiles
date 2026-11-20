# GitHub PR Viewed Tree

Brave/Chrome MV3 extension. Marks files you ticked as **Viewed** in the GitHub pull request file tree (dimmed row + green check). No API token, no extra requests.

## Install (dev mode)
1. `brave://extensions` / `chrome://extensions` -> enable **Developer mode**
2. **Load unpacked** -> select this folder
3. Reload the extension after edits.

## How it works
- `page-hook.js` (page world): reads viewed state from GitHub's embedded page JSON and the page's own fetch/XHR responses.
- `content.js`: merges that with Viewed toggles rendered in the diff list and `chrome.storage.local` (per PR, pruned after 90 days); newest observation wins.
- Tree rows and diffs are matched by GitHub's anchor id `diff-<sha256(path)>`.
- Folders dim when all their rendered files are viewed.

## Debug
`localStorage.prvDebug = 1` in the page console, reload, watch `[prv]` logs.
