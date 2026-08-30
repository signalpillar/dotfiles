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
- Keep filenames URL-friendly, lowercase, and hyphen-separated.
- If there are multiple prototypes, maintain `index.html` as a simple launcher page.

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

### Open Props

- Override `--shadow-color` and `--shadow-strength` per theme scope and the derived `--shadow-strength-N` values still resolve correctly, because custom properties substitute lazily at use time.
- Check a token exists before using it.
  The scales are not continuous: sizes run 1 to 8 plus `-00`/`-000`, radii stop at 6 plus `--radius-round`.
- Inspect the resolved value before choosing a token by name.
  In Open Props, `--font-size-00` is 0.5rem and is too small for normal control labels or explanatory copy.

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
- Parse and cross-check the manifest before opening a browser.
  Verify it is valid JSON, that every referenced id resolves, and that every section `type` has a renderer.
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

### Manifest-driven code

- The manifest is the schema, so sanitise anything read from `localStorage` or an imported file against it.
  Drop unknown ids and unknown enum values rather than trusting the payload.
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
