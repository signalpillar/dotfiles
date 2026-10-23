# Prototype Instructions

This directory contains throwaway, one-file HTML prototypes published through GitHub Pages.

This file is a living, self-improving record of how to build well in this directory, not a
static spec. Whichever agent works here maintains it: when a session in `prototypes/` turns up a
real gotcha, a wrong assumption caught late, or a technique worth reusing, add it under
"Learned the Hard Way" (or a new dated subsection there) before finishing the session. Keep
entries concrete and specific to what actually went wrong, not generic advice. Read this whole
file, including "Learned the Hard Way", before starting new prototype work — it is cheaper than
re-discovering the same mistake.

## Principles

- Prefer a single `.html` file per prototype, with application CSS and JavaScript embedded in that file.
- Link `prototype-base.css` for the shared browser foundation, and `prototype-jelly.css` when the prototype uses Jelly UI.
  Keeping these relative dependencies is preferable to copying cross-prototype fixes.
- Do not add a bundler, package manager, or build step.
- Design mobile-first, then make the layout acceptable on desktop.
- Treat mobile-friendly layout as a hard requirement, not a stretch goal.
  Primary flows must work with thumb reach, one-handed scrolling, and a 320px viewport without horizontal page scroll.
- Prefer drawers over popups, dialogs, or centered modals for secondary detail and inspection surfaces.
  Use a bottom sheet on small screens; a side drawer is fine on wider layouts when it stays thumb-reachable.
- When a compact layout has a long list plus item detail, do not stack the detail under the whole list.
  That forces a long scroll after every selection.
  Prefer one of the patterns under Compact list + detail below.
- Keep prototypes easy to open directly from the filesystem and from GitHub Pages.
- Keep state local to the page unless persistence is part of the prototype.
- Use `localStorage` only for user settings or session history that should survive reloads.
- Optimize for fast iteration, clear interaction, and realistic feel over production architecture.
- Make primary actions large, touch-friendly, and visible without scrolling on mobile.
- Keep controls understandable without external documentation.
- Include enough inline labels and stats for the prototype to explain itself while being used.
- Avoid backend services, authentication, and analytics unless they are the thing being prototyped.
- Exception: a prototype may opt in to sync its data between devices through the shared Nostr layer in `nostr/` (see `nostr/DESIGN.md`).
  It declares a `storage` block in its manifest, loads `nostr/store.js` and `nostr/account.js`, and adds one `<nostr-account>` element.
  It must keep working on local data without a sign-in, and from `file://`.
- Keep filenames URL-friendly, lowercase, and hyphen-separated.
- If there are multiple prototypes, maintain `index.html` as the launcher app.
  Each prototype has one entry in its manifest: group (`synced`, `browser` or `none`, by where the app keeps data), data sources, needs, a summary, what it saves and where its data comes from.
  `prototype.tests/index` fails when a page is missing from the manifest, or when an entry's group, microphone or WebGPU need does not match the page code.
- Every page loads the shared top bar with `<script src="shell.js" defer></script>`, as the last script in `<head>`.
  The bar links to `index.html` and to `about.html` (terms, privacy, Nostr sync, delete device data), and hosts the Nostr account badge on pages that load `nostr/account.js`.
  Do not add a second "back to index" link or an own account badge to a prototype.
- `about.html` is the one place for terms and privacy text.
  When a prototype starts to load from a new third-party host, add the host to its privacy table.

## Compact list + detail

On phones, a list (timeline, stages, inbox) plus a detail panel is a common trap:
if the detail sits under the full list, selecting an item still leaves the detail below the fold.

Canonical options we considered (Android Adaptive / Material list-detail, SAP Fiori, Apple NavigationSplitView, NN/g sheets):

1. List → detail replace.
   Compact: tapping an item replaces the list with a full-screen detail; Back returns to the list.
   Wide: list and detail sit side by side.
   Best when the detail is the main read after selection.
2. Detail as bottom sheet.
   Compact: the list stays; detail slides up in the thumb zone (optionally with peek / half / full detents).
   Wide: side-by-side or sticky pane.
   Best when keeping place in the list matters and the detail is short enough for a sheet.
   Prefer sheets for secondary inspection; NN/g warns against sheets as the long primary happy path.
3. Inline expand under the selected item.
   Compact: the selected row expands and the detail appears immediately under that item.
   Wide: list and detail can still sit side by side.
   Best when users scan many items and need the detail without leaving the list context.

Default for new prototypes in this repo: option 3 (inline expand) on compact screens, with a side-by-side or sticky detail pane on wide screens.
Use drawers / bottom sheets for secondary surfaces (filters, overlays, short inspectors), not for the primary selected-item read unless the content is deliberately sheet-sized.
Keep Prev / Next / Back actions in the lower third of the detail when they are the primary controls.

## Compact controls

`fact-graph.html` is the reference for these patterns (2026-10).

- **Status messages:** call `PrototypeShell.toast(message)` from `shell.js`.
  Do not give a full line of the page to transient messages.
  Keep an inline status line only for lasting states, such as "a library did not load".
- **Exclusive accordion:** give related `<details>` the same `name` (for example `name="side"`), so opening one closes the others.
  A side column then never grows longer than one open section.
- **"More" menu:** move rare actions (Export, Import, Clear all, Re-layout) into a native `popover` opened by one "More" button with `popovertarget`.
  It closes on Escape and on an outside click with no JavaScript.
  On wide screens, place it under its button, or above when the space below is too small, from the `toggle` event; on phones, style it as a bottom sheet.
  An action closes the menu; a two-step confirm keeps it open for the second tap.
- **Floating primary action on phones:** a floating button in the thumb zone opens the composer in a bottom-sheet `<dialog>`.
  Move the one composer node between the side column and the sheet with `matchMedia`, as in Compact list + detail.
  Close the sheet after a clean submit; keep it open when lines with errors remain.
  Hide the floating button while another bottom sheet is open, and lift the toast above it with `prototype-shell { --prototype-toast-offset: 5.25rem; }`.

## Visual Direction

- Prefer light themes and an airy, light visual design by default.
- Use white or softly tinted surfaces, restrained shadows, clear borders, and high-contrast dark text.
- Use a dark theme only when the prototype's subject or an explicit request calls for it.
- Set Jelly UI to `mode="light"` for the default experience rather than inheriting the operating-system theme.
- Prefer soft sheet/drawer surfaces over floating popup cards that block the whole page.

## Use Libraries, Not Hand-Written UI

- Reach for an existing component library before writing a component by hand.
- The default component library is [Jelly UI](https://jelly-ui.com).
  It is dependency-free web components, loaded with one module script tag and no build step.
  Wrap the page in `<jelly-theme mode="light">` and use `<jelly-button>`, `<jelly-card>`, `<jelly-tabs>`, `<jelly-input>`, `<jelly-drawer>` and the rest.
- For data-viewer prototypes with no custom widgets, prefer [Pico CSS](https://picocss.com) over Jelly UI (2026-09: `sixth-form-open-events.html` is the reference).
  One stylesheet link, semantic HTML, zero JavaScript: no upgrade races, no shadow-DOM testing quirks, no canvas-overflow fixes.
  Hand-roll tabs (~20 lines of `role=tab` buttons) and detail surfaces (native `<dialog>` / `<details>`).
  Offline fallback is total, since every control is a native element that keeps working unstyled.
- Never fetch a sidecar file unconditionally: from `file://` the request fails CORS and spams the console.
  Gate sidecar loads on `http:`/`https:` and lead with an upload picker plus a visible empty state instead (2026-09: open-events viewer).
- Paired inputs (lat/lon) must tolerate partial entry: commit only when both fields are non-empty, and never rewrite inputs on invalid input, or tabbing between fields wipes what was typed (2026-09: open-events home entry).
- Keep user flags (interesting / not interesting) in a separate store map keyed by record name, not on the imported rows, so CSV re-imports preserve them (2026-09: open-events interest).
- The default token set is [Open Props](https://open-props.style), loaded as a plain stylesheet link.
  Take spacing, radii, shadows, easings, and type scale from its custom properties.
- Load third-party CSS and JS from a CDN with a plain `<link>` or `<script>` tag.
  A CDN dependency is not a build step and is preferred over reimplementing solved UI.
- Hand-written CSS is for page layout and for whatever the libraries genuinely do not cover.
  Do not restyle a component the library already styles.
- Do not hand-write a value that a token already names.
  Use `var(--size-3)`, not `0.75rem`.
- Degrade gracefully when a CDN cannot be reached.
  Unknown custom elements still render their light-DOM children, so give them a minimal `:not(:defined)` fallback and keep the prototype usable offline.

## Shared CSS boundary

- Every new prototype should link `prototype-base.css` after Open Props and before its own `<style>` block.
  Prototypes that use Jelly UI also link `prototype-jelly.css` after the base file.
- Split shared CSS by concern:
  - `prototype-base.css` owns browser foundations: box sizing, page overflow safety, inherited form typography, and `[hidden]`.
  - `prototype-jelly.css` owns Jelly host behavior, tab containment, minimal corner radius via `--jelly-prototype-radius`, and generic `:not(:defined)` display fallbacks.
- Keep layout, colours, typography choices, component composition, and prototype-specific fallbacks in the prototype HTML.
- Promote a rule into the matching shared file only after at least two prototypes need the same fix.
  Do not turn either file into a design system or a collection of utility classes.
- Use relative links so direct filesystem use and GitHub Pages both work:
  `<link rel="stylesheet" href="prototype-base.css">`
  and, when needed, `<link rel="stylesheet" href="prototype-jelly.css">`.
- A shared-file change affects every consumer.
  Run the tests for all prototypes that link it, not only the prototype being edited.

## Everything Configurable Lives in One Manifest

- Each prototype embeds exactly one declarative JSON manifest, as `<script type="application/json" id="...">`, parsed once at startup.
- The manifest holds everything that is not application logic.
  Layout constants, colours, thresholds, storage keys, timings, labels, user-facing copy, external links, and all domain knowledge.
- Application code reads the manifest and renders from it.
  It must not contain magic numbers, hard-coded domain facts, or embedded copy.
- Changing what the prototype knows or how it is tuned should mean editing JSON only, never JavaScript.
- Keep the manifest valid JSON, not a JavaScript object literal, so it can be extracted, validated, and swapped wholesale.
- Large lesson or catalog data lives in a neighbouring file, same pattern as `llm-lifecycle-timelines.yaml` and `gcse-science-f1-roadmap/catalog.json`.
  The HTML keeps a small boot JSON with `dataFile` and load labels.
  Domain facts stay in the neighbouring file.

## Learned the Hard Way

Notes from building against these rules.
Read them before writing the first line, not after debugging.

### Jelly UI

- Every Jelly component paints its soft body on a `<canvas>` that deliberately overhangs its own box so the membrane can bulge.
  Those canvases stick out past the viewport and give the page a horizontal scrollbar.
  Fix it once with `html, body { overflow-x: clip }`.
  Use `clip`, not `hidden`: `hidden` creates a scroll container and breaks `position: sticky` and viewport-anchored overlays.
- A multi-tab `<jelly-tabs>` bar sizes itself to its content and will widen the whole page.
  Add `jelly-tabs::part(tabs) { max-inline-size: 100% }` and the component compresses the pill track to fit.
  Past four tabs, also set `size="small"` or the labels collide.
- `<jelly-theme>` is `display: contents`, so it paints no page background.
  Set the body background yourself from the same token values, or the page stays white around a themed island.
- Build a custom element's whole subtree and append its children *before* inserting it into the document.
  The element then upgrades with its children already present, so you never depend on undocumented child-observation behaviour.
- Components differ in how they fail when the CDN is unreachable.
  `jelly-button`, `jelly-card`, `jelly-chip` and `jelly-badge` render their light-DOM children, so they degrade to plain clickable text and keep working.
  `jelly-input` and `jelly-textarea` are entirely shadow DOM and simply vanish.
  Put critical controls on the first group, and swap in native `<input>` / `<textarea>` for the second.
- Detect availability with `await Promise.race([customElements.whenDefined("jelly-button"), timeout])` at the top of a module script, then branch the component factory on the result once.
  Do not check per call site.
- Jelly boolean state can be property-driven rather than ordinary string attributes.
  When closing a drawer, set its `open` property to `false` and remove the `open` attribute so upgraded and fallback paths agree.
- Use `size="small"` for dense secondary controls, but preserve a practical 40-44px hit area on phones with local min-size rules.
  Visual density and touch-target size are separate concerns.
- A Jelly control stretched by its parent layout breaks in a way that looks like a broken component.
  As a grid or flex item the host stretches, but the shadow `<button>` keeps its intrinsic width, so the painted soft body sits far wider than the clickable target.
  Use the component's own `block` attribute for a full-width button instead of stretching the host, or keep the item from stretching with `justify-items: start`.
- Jelly defaults to pill / wide radii.
  `prototype-jelly.css` sets `--jelly-prototype-radius` to `var(--radius-2)` and maps the common component radius tokens to it.
  Override that custom property on a prototype only when it needs a different corner language.
- A `change` listener on `<jelly-tabs>` also receives bubbled `change` events from every input inside the panels.
  A file picker that clears its own value before the event bubbles reads as a tab switch to the default tab; a radius `<select>` reads as a switch to `"10"`.
  Guard the handler with `if (e.target !== tabs) return` (2026-09: the schools tab import silently rendered nothing for this reason).
  This guard also protects the pre-existing progress import, which clears its file input the same way.

### Open Props

- Override `--shadow-color` and `--shadow-strength` per theme scope and the derived `--shadow-strength-N` values still resolve correctly, because custom properties substitute lazily at use time.
- Check a token exists before using it.
  The scales are not continuous: sizes run 1 to 8 plus `-00`/`-000`, radii stop at 6 plus `--radius-round`.
- Inspect the resolved value before choosing a token by name.
  In Open Props, `--font-size-00` is 0.5rem and is too small for normal control labels or explanatory copy.

### Sibling data files

- `file://` cannot `fetch` a neighbouring JSON or YAML file.
  Show a file picker after a failed load, same as the timelines prototype.
  GitHub Pages and a local static server load the file automatically.

### Printable tutor slips

- Hide the live chrome with `@media print { .shell, #status { display: none } }` and print a dedicated `.print-sheet` built from the selected snippet.
  Do not print the on-screen lesson page. The tutor key, filters, and day list leak onto the paper.
  Build the sheet in the click handler, then call `window.print()`.
  A student slip gets the snippet, predict questions, and a blank trace table.
  A tutor card adds the cue and the key.

### Layout

- A CSS grid column sizes to the widest item by default, so one intrinsically wide child (a tab bar, a canvas, a table) widens the whole page.
  Set `grid-template-columns: minmax(0, 1fr)` on the container and `min-inline-size: 0` on the children.
- Give wide content its own `overflow-x: auto` wrapper rather than letting the page scroll sideways.
- For list + detail prototypes, relocate one detail node between an inline slot under the selected row (compact) and a side pane (wide) via `matchMedia`, rather than duplicating the detail markup.
  Re-run placement on resize so a phone rotated to landscape does not leave the detail stranded.

### Testing a prototype

- Reusable tests for individual prototypes live under `prototype.tests/<prototype-name>/tests`.
  Check that directory before creating scratch tests or assuming no harness exists.
- Drive the real page in Chromium with Playwright before calling it done.
  Check for page errors, failed requests, and `document.documentElement.scrollWidth - clientWidth` at 320, 390, 768 and 1360 px.
- The sandbox proxy blocks Chromium from reaching CDNs.
  Download the library once, then use Playwright's `route()` to fulfil the CDN URLs from those local copies.
  Never disable TLS verification to work around it.
- Exercise both paths: with the library routed in, and with it blocked, so the degraded fallback is actually verified.
- Re-measure a shadow-DOM click target immediately before each click.
  Switching a tab changes the page height and moves the bar, so coordinates captured earlier miss and look like a broken control.
- Upgraded `jelly-tabs` segments expose `role="tab"` with the exact label, not `role="button"`.
  Use `getByRole("tab", { name })` on the upgraded path and `getByRole("button", { name })` on the degraded path.
  Do not switch tabs in tests by setting `tabs.value` plus a synthetic `change` event: it desyncs the app's view variable from the tab bar and hides real update bugs (2026-09: this masked the bubbled-change bug above for several debug rounds).
- Parse and cross-check the manifest before opening a browser.
  Verify it is valid JSON, that every referenced id resolves, and that every section `type` has a renderer.
- A deliberately blocked or 404'd request logs a *generic* console error with no URL in its text
  (`Failed to load resource: net::ERR_FAILED`, `... 404 (Not Found)`).
  Filtering `requestfailed` events by URL substring does not catch these; filter the console-error
  text by pattern too, or the intentional block/404 in one test path reads as a real failure
  (2026-09: `event-trail` CDN-blocked and missing-sidecar test paths).
- Check external links resolve with `curl -o /dev/null -w "%{http_code}" -L` before shipping them.
- Test servers must serve relative shared assets such as `prototype-base.css` and `prototype-jelly.css`.
  A page that works from disk can otherwise appear correct while tests silently receive a 404 for its foundation.
- `playwright`'s browser path under `/opt/pw-browsers` is versioned (e.g. `chromium-1194/chrome-linux/chrome`, not `chromium/chrome-linux/chrome`).
  `find /opt/pw-browsers -maxdepth 3` once rather than guessing the path.
- No system-wide `playwright` package is installed; `npm init -y && npm install playwright@latest --no-save` in a scratchpad directory gets a throwaway copy for one test run. Delete the scratchpad dir afterward.
- The system Python `cryptography`/`pdfminer` stack in this sandbox is broken (a Rust extension mismatch panics on import), which breaks `pypdf`/`pdfplumber` too since they depend on it transitively.
  A fresh `python3 -m venv` and installing `pdfplumber` there avoids the broken system packages. `poppler-utils` (`pdftotext`) is not installable via apt here (404 on the security mirror), so don't rely on it either.

### Verifying domain facts (tax, finance, and other real-world rules)

- A prototype that encodes a real-world rule (tax bands, interest formulas, unit conversions) needs
  the rule checked against an authoritative source, not just re-derived from memory or "it sounds
  right." Reasoning about *why* a rule should work a certain way (e.g. the UK "60% tax trap"
  marginal-rate story) can sound fully consistent while still supporting the wrong formula — it
  only proves a marginal claim, not the total/integral one.
- Random calculator and comparison-site results found via search are often wrong or stale (mirrored
  clone sites, outdated thresholds, blended figures that secretly include a second tax/fee). Don't
  trust a single fetched number. Get at least two independently-run real examples that agree on the
  same total before trusting a formula, and prefer primary sources (the regulator/authority's own
  worksheet or published examples) over secondary summaries when the two disagree.
- Concretely, for UK Income Tax: HMRC's SA110 "Tax calculation summary notes" (search
  `assets.publishing.service.gov.uk ... SA110-Notes`) is the primary source with the literal
  worksheet formulas — worth fetching directly (see the PDF extraction workaround above) rather
  than trusting paraphrased summaries of it.
- A plausible-looking model can be wrong only in a narrow, easy-to-miss range. This prototype's
  first UK Income Tax implementation applied the basic/higher rate band edges (£50,270/£125,140) as
  fixed points on *gross* salary, which is correct for the standard Personal Allowance case but
  silently undercounts tax by £2,000+ once the £100k-£125,140 taper reduces the allowance — because
  the real bands (£37,700 then £87,440) are fixed *widths of taxable income* (salary minus the
  allowance), confirmed against HMRC's own worksheet and two independent real £120,000 examples.
  Test the boundary/edge-case inputs (here: just above, inside, and just above the taper zone), not
  only a mid-range happy path — a formula bug can hide entirely below the first interesting threshold.
- When a threshold-based model has more than one rule that can shift a boundary (here: the UK
  Personal Allowance taper), do not hand-derive the marginal rate at a given input by combining
  band labels. Compute it numerically instead: `(totalAt(x + 1) - totalAt(x)) / 1`. This gets the
  taper's own effect for free and stays correct if a threshold value changes later. Used this to
  add the combined Income Tax plus National Insurance marginal rate to the tax visualiser, and it
  reproduced the well-known 62% figure in the £100k-£125,140 band without a single special case.

### DataScript and Cytoscape (2026-10: `fact-graph`)

- Inline `<script defer>` is ignored: the inline code runs before the deferred CDN scripts and sees no library globals.
  Use an inline `<script type="module">`; modules join the same ordered queue as `defer` scripts and still work from `file://`.
- The DataScript JS API (`datascript@1.8.1`, global `datascript`) takes attribute names as plain strings without the leading colon (`"node/slug"`).
  Schema values and tx functions keep the colon: `{":db/unique": ":db.unique/identity"}`, `[":db.fn/retractEntity", ["edge/id", id]]`.
- A `:db.unique/identity` slug gives node merging for free: two facts that name `ews` upsert into one entity.
  This also holds inside one transaction: two tempids with the same new slug resolve to one entity, so a self-loop needs no special case.
- Do not load `datascript.js` into a Node `vm` context to test it: cross-realm objects fail with `No protocol method IKVReduce.-kv-reduce`.
  `require()` it directly instead.
- `cytoscape-fcose` needs `layout-base` then `cose-base` loaded before it, as plain script tags.
  Keep node positions per graph and pass the known ones as `fixedNodeConstraint`, so adding a fact does not reshuffle the graph.
- A recursive DataScript rule loops forever on a cyclic graph when its recursive body calls another rule (`(link ?a ?m) (reaches ?m ?b)`).
  It stops when the recursive body uses plain patterns (`[?e "edge/from" ?a] [?e "edge/to" ?m] (reaches ?m ?b)`).
  A non-recursive wrapper rule around the safe form, such as a slug lookup, also stops.
- `ds.q` is synchronous, so a query that never ends freezes the tab.
  Run user-typed queries in a Web Worker built from a Blob, `importScripts()` the library there, send the db as `ds.serializable(db)`, and `terminate()` the worker at a time limit.
  This also works from `file://`.
- DataScript has `clojure.string/includes?` and `str` but no `clojure.string/lower-case`.
  For case-insensitive search, store a lowercase search attribute when you transact.
- `page.route()` does not cover `importScripts()` inside a dedicated worker.
  Use `page.context().route()` to serve CDN fixtures to workers in tests.
- `Node.append(null)` inserts the text "null".
  Filter optional children before `append()` (an `appendAll` helper), not only in the element factory.
- A fixed bottom sheet can cover primary controls lower on the page, such as a composer's submit button.
  Close the sheet when the user focuses the composer on compact screens, and pad the page by the sheet height while it is open.

### Shared Nostr layer (2026-10: `nostr/`, first adopter `fact-graph`)

- Load the shared files as classic `defer` scripts, not ES modules: a module import of `nostr/store.js` fails from `file://`.
  The `nostr-tools` browser bundle (`lib/nostr.bundle.js`) also loads as a classic script and sets `window.NostrTools`.
- In `nostr-tools@2.25.2`, `SimplePool.subscribe`, `subscribeEose` and `querySync` take one filter object, not an array.
  `publish()` returns one promise per relay; wrap them in `Promise.any` with a time limit.
- Public relays return at most 500 events per query (NIP-11 `max_limit`), so a first pull must page back with `until`.
- An addressable event (kind 30000-39999) with the same `created_at` as the stored one can lose to it, because relays keep the lower id on a tie.
  Make `created_at` grow with every write of one `d` tag.
- A `<dialog>` that renders its content only when `dialog.open` is true must call `showModal()` before it renders, or it opens empty.
- The store switches owner (sign-in, sign-out) asynchronously, after the account badge already updates.
  Tests must poll the data after a sign-out, not read it once.
- A retry loop that sets "syncing" on every attempt makes the badge blink between "Offline" and "Syncing".
  Keep "offline" until an attempt succeeds.
- Do not run `pkill -f` with a pattern that also matches your own shell command line; it kills the shell that runs it.
- An open IndexedDB connection blocks `indexedDB.deleteDatabase()` from any page, for example "Delete all" on `about.html`.
  Set `db.onversionchange = () => db.close()` on every connection you open.

### Shared top bar (2026-10: `shell.js`)

- Inside a shadow root, an author rule such as `a { display: inline-flex }` beats the `hidden` attribute.
  Add `[hidden] { display: none !important }` to the shadow styles.
- Every prototype test server must also serve `/shell.js`, or the page logs a 404 that reads as a real failure.
- The bar scrolls away with the page and returns as a fixed bar when you scroll up.
  The host element keeps the bar's height in the page flow, so content never jumps when the inner bar becomes fixed.
  In tests and captures, measure the inner `nav` in the shadow root, not the host: the host stays where it was.
- Pico styles every `<dialog>` as a full-screen centered layer (`display: flex`, `min-height: 100%`).
  A bottom-sheet dialog needs its own `dialog.x[open]` rule that resets `display`, `inset`, the sizes and `backdrop-filter`.
- A dropdown placed under its button runs off the bottom of the window when the button is low on the page.
  Measure the space below and open upward when it does not fit.
- A page that boots asynchronously (fact-graph reads IndexedDB first) needs tests that wait for an explicit ready flag after `goto` and after `reload`, not for the first rendered text.
- `prototype.tests/shell` loads every page twice, with and without the bar, and fails only on new overflow or new errors.
  A page that fails on its own hides its errors from that comparison, so pages built with the bar (`about.html`, `index.html`) must have zero errors.

### Manifest-driven code

- The manifest is the schema, so sanitise anything read from `localStorage` or an imported file against it.
  Drop unknown ids and unknown enum values rather than trusting the payload.
- Split validation by what the field is *for*: reject the whole import over a missing or duplicate
  identity/traceability field (an id, a source URL), since the app cannot show that row honestly at
  all, but sanitise a bad soft/enum field (a rating outside its known set) to a neutral fallback on
  just that row instead of failing an otherwise-good file over one bad value
  (2026-09: `event-trail`, an unrecognised `rating` renders as "Unrated" rather than rejecting the
  whole import).
- Give each section or item a `type` and keep a lookup of renderers keyed by it.
  Adding a new kind of content then means adding one JSON block and one small function, and a missing renderer is a one-line check to catch.
- Templated copy beats string concatenation in code.
  Store `"{count} / {max} pts"` in the manifest and fill it, so wording changes never touch JavaScript.
- Counts shown in badges must use the same resolved collection that the detail renderer uses.
  If notes can be inherited from overlays or related records, count those resolved notes rather than only the item's direct array.

## GitHub Pages Compatibility

- Assume prototypes are served as static files from the repository root.
- Use relative links between files.
- Avoid absolute paths unless they intentionally include the repository name.
- Do not rely on server-side routing, rewrites, environment variables, or generated assets.
