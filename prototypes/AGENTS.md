# Prototype Instructions

This directory contains throwaway, one-file HTML prototypes published through GitHub Pages.

## Principles

- Prefer a single self-contained `.html` file per prototype.
- Put CSS in a `<style>` tag and JavaScript in a `<script>` tag inside the same file.
- Do not add a bundler, package manager, or build step.
- Design mobile-first, then make the layout acceptable on desktop.
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

## Use Libraries, Not Hand-Written UI

- Reach for an existing component library before writing a component by hand.
- The default component library is [Jelly UI](https://jelly-ui.com).
  It is dependency-free web components, loaded with one module script tag and no build step.
  Wrap the page in `<jelly-theme mode="auto">` and use `<jelly-button>`, `<jelly-card>`, `<jelly-tabs>`, `<jelly-input>`, `<jelly-drawer>` and the rest.
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

### Open Props

- Override `--shadow-color` and `--shadow-strength` per theme scope and the derived `--shadow-strength-N` values still resolve correctly, because custom properties substitute lazily at use time.
- Check a token exists before using it.
  The scales are not continuous: sizes run 1 to 8 plus `-00`/`-000`, radii stop at 6 plus `--radius-round`.

### Layout

- A CSS grid column sizes to the widest item by default, so one intrinsically wide child (a tab bar, a canvas, a table) widens the whole page.
  Set `grid-template-columns: minmax(0, 1fr)` on the container and `min-inline-size: 0` on the children.
- Give wide content its own `overflow-x: auto` wrapper rather than letting the page scroll sideways.

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

### Manifest-driven code

- The manifest is the schema, so sanitise anything read from `localStorage` or an imported file against it.
  Drop unknown ids and unknown enum values rather than trusting the payload.
- Give each section or item a `type` and keep a lookup of renderers keyed by it.
  Adding a new kind of content then means adding one JSON block and one small function, and a missing renderer is a one-line check to catch.
- Templated copy beats string concatenation in code.
  Store `"{count} / {max} pts"` in the manifest and fill it, so wording changes never touch JavaScript.

## GitHub Pages Compatibility

- Assume prototypes are served as static files from the repository root.
- Use relative links between files.
- Avoid absolute paths unless they intentionally include the repository name.
- Do not rely on server-side routing, rewrites, environment variables, or generated assets.
