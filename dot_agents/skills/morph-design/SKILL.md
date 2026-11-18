---
name: morph-design
description: Rules and principles for writing and restyling Empryo morphs (morph JSON surface, view modules, helper script). Use when creating a morph, changing how one looks, or adding actions to it.
license: personal
compatibility: all
metadata:
  audience: developer
---

## What I do

- Design and edit Empryo morphs so the user can read them at a glance and act from them.
- Keep the look editorial and calm. Every pixel should carry information or an action.

## Layout principles

- **Readable text beats clever shapes.** Show items as a list or as wrapping chips with their full names. Do not use clouds of dots, radial layouts, or labels cut off at N characters. If the user has to hover to read something, the layout is wrong.
- **Sort by what matters now.** Put flagged and recently active items first. Hide finished items behind a "done (N)" toggle instead of mixing them in.
- **Filter before scroll.** Any list with more than about 10 items gets a filter input and a "N shown" count.
- **Selection drives detail.** Clicking an item selects it. Show the selected item's name, short state, and its actions right under the list. Do not put them in a separate tooltip.
- **Cut noise.** Remove static legends and badge rows that look like data but never change. If a colour needs explaining, a single muted caption line is enough.
- **Dense sequences become lists.** A run like `✓a ✓b ○c ○d` gets its own section. Give it a header with a "N of M done" count, one item per row in an auto-fill grid, done items faded with a coloured ✓, and the current item in bold.

## Editorial style

- Body text uses a serif stack: `'Iowan Old Style','Charter','Source Serif Pro',Georgia,serif`, line-height about 1.45. Monospace only for paths and identifiers.
- Section headers use one shared class (e.g. `.hd`): about 17px, semibold, slight negative letter-spacing. Put headers inside the view module when the platform's text blocks can't be sized.
- Inputs: no box. Underline only, italic placeholder, inherited serif font.
- Shapes: hairline rules between rows instead of boxed cards. Near-square corners (2px) on chips, badges and SVG nodes. No pills.
- Colour only encodes state (ok / warn / error / done / active), always from theme tokens, never hard-coded. Done is muted, not hidden.
- Captions and secondary info are 11–12px with an opacity of 0.8 to 0.9, never lower. Faint grey text is hard to read, so use size, not fading, to set it apart. Reserve low opacity for done or inactive items.

## Action buttons

- Every action button starts with an emoji that names the action: 📂 open folder, 📄 open file, 📋 copy path, 💬 ask the agent, 🔍 audit, 🧭 check status, 💡 explain, ✕ clear.
- Buttons share one class (e.g. `.act`). Use small sans-serif uppercase text with letter-spacing, a 1px `currentColor` outline, 2px radius and a transparent background with a hover tint. The sans caps must stand out from the serif body.
- Labels are short verbs. Put a count in the label when it helps, e.g. `📂 Open references (4)`.
- Surface actions, pulse actions and intent labels get the same emoji treatment.
- Anything with a folder or file behind it gets an "open" action, so the user can navigate the material directly.

## Helper script rules

- The helper is read-only towards the data it shows. It reads and summarises. It never writes to the things it shows.
- Bind to `127.0.0.1` only.
- Validate every query parameter against a strict pattern (e.g. `[\w.\-]{1,80}`) before it touches the filesystem. Build paths from a fixed root plus the validated name, and check that they exist.
- Side-effect endpoints (open in Finder or an editor) are `POST`. They launch `open` / `xdg-open` detached with an argument list (no shell), and return `{ok, message}` JSON.
- Add `Access-Control-Allow-Origin: *` to `POST` replies so the view can read the result. The view calls `fetch` first and falls back to a declared intent with the same URL only if `fetch` fails, so the action never fires twice.
- Fall back gracefully, e.g. open the parent folder when a subfolder is missing, and say which path was opened.

## Editing mechanics

- View modules are single-line strings inside JSON. Edit them by parsing the JSON, changing the string, and writing it back with `JSON.stringify(obj, null, 2)` plus a trailing newline. Check first that this reproduces the original file byte for byte.
- Edit a module by replacing anchors that occur exactly once. Assert the anchor count before replacing.
- After editing, syntax-check each module: `new Function('return (' + module.replace(/^export default /, '') + ')')`.
- Keep per-view `css` additive. Shared classes (`.ed`, `.hd`, `.act`) are repeated in each view's css, because views may be isolated from each other. Use `!important` only to override inline styles you don't own.
- A changed morph definition may need the user to approve it again. Say so.
- After changing the helper script, restart the running helper. Test new endpoints on a spare port with an invalid input first.
- Only the view modules and the helper are yours to style. Platform-drawn parts (stats, key/value boxes, tabs, view titles) keep the default look. Tell the user that instead of fighting it.

## Answering about a morph

- When the user asks why something is on screen, explain what it encodes. Then offer to shrink it, add live counts to it, or remove it.
- Report changes as a short list grouped by what the user sees, not by file.
