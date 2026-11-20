// Isolated-world content script. Merges viewed state from three sources,
// newest observation wins, and marks matching File Tree rows:
//   1. page JSON (embedded data + the page's own fetch/XHR), via page-hook.js
//   2. Viewed toggles currently rendered in the diff list
//   3. chrome.storage.local, so marks survive reloads / unloaded diffs
// Files are keyed by GitHub's anchor id: diff-<sha256(path)>.
(() => {
  const HEX64 = /^[0-9a-f]{64}$/;
  const DIFF_ID = /diff-([0-9a-f]{64})/;
  const TOGGLES = 'button[aria-label="Viewed"], button[aria-label="Not Viewed"], input.js-reviewed-checkbox';
  const MAX_AGE_MS = 90 * 24 * 3600 * 1000;

  let pr = prKey();
  let map = new Map(); // hash -> { v: boolean, t: number }
  const hashCache = new Map();

  const debug = (...a) => {
    try {
      if (localStorage.prvDebug) console.debug("[prv]", ...a);
    } catch {}
  };

  function prKey() {
    const m = location.pathname.match(/^\/([^/]+)\/([^/]+)\/pull\/(\d+)/);
    return m ? `${m[1]}/${m[2]}#${m[3]}` : null;
  }

  async function hashPath(path) {
    let h = hashCache.get(path);
    if (h) return h;
    const buf = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(path));
    h = [...new Uint8Array(buf)].map((b) => b.toString(16).padStart(2, "0")).join("");
    hashCache.set(path, h);
    return h;
  }

  function set(hash, v, t = Date.now()) {
    const cur = map.get(hash);
    if (cur && (cur.t > t || cur.v === v)) return false;
    map.set(hash, { v, t });
    return true;
  }

  let saveTimer = 0;
  function save() {
    clearTimeout(saveTimer);
    const key = pr;
    const snapshot = Object.fromEntries([...map].map(([h, { v, t }]) => [h, [v, t]]));
    saveTimer = setTimeout(() => {
      if (key) chrome.storage.local.set({ ["prv:" + key]: { u: Date.now(), e: snapshot } });
    }, 300);
  }

  async function load(key) {
    const k = "prv:" + key;
    const all = await chrome.storage.local.get(null);
    const stale = Object.keys(all).filter((x) => x.startsWith("prv:") && Date.now() - (all[x]?.u || 0) > MAX_AGE_MS);
    if (stale.length) chrome.storage.local.remove(stale);
    if (key !== pr) return;
    for (const [h, [v, t]] of Object.entries(all[k]?.e || {})) set(h, v, t);
    debug("loaded", map.size, "from storage");
    render();
  }

  function hashFromNode(el) {
    const own = el.id && el.id.match(DIFF_ID);
    if (own) return own[1];
    const a = el.querySelector('a[href*="#diff-"]');
    const m = a && a.getAttribute("href").match(DIFF_ID);
    return m ? m[1] : null;
  }

  async function rowHash(row) {
    const h = hashFromNode(row);
    if (h) return h;
    // React tree fallback: path carried on the row itself.
    const path = row.getAttribute("data-path") || row.getAttribute("data-file-path") || row.id;
    return path && /[./]/.test(path) && !path.startsWith("file-tree") ? hashPath(path) : null;
  }

  async function scanToggles() {
    let changed = false;
    for (const t of document.querySelectorAll(TOGGLES)) {
      const box = t.closest('[id^="diff-"]');
      let hash = box && box.id.match(DIFF_ID)?.[1];
      if (!hash) {
        const p = t.closest("[data-tagsearch-path], [data-file-path]");
        const path = p && (p.getAttribute("data-tagsearch-path") || p.getAttribute("data-file-path"));
        if (path) hash = await hashPath(path);
      }
      if (!hash) continue;
      const v = t instanceof HTMLInputElement ? t.checked : t.getAttribute("aria-pressed") === "true";
      changed = set(hash, v) || changed;
    }
    if (changed) save();
  }

  async function render() {
    if (!pr) return;
    const rows = document.querySelectorAll('[role="treeitem"]:not([aria-expanded])');
    let marked = 0;
    for (const row of rows) {
      const hash = await rowHash(row);
      const viewed = !!(hash && map.get(hash)?.v);
      if (viewed) marked++;
      if (viewed && row.getAttribute("data-prv") !== "viewed") row.setAttribute("data-prv", "viewed");
      else if (!viewed && row.hasAttribute("data-prv")) row.removeAttribute("data-prv");
    }
    debug("render", { rows: rows.length, marked, known: map.size });
  }

  let tick = 0;
  function schedule() {
    clearTimeout(tick);
    tick = setTimeout(async () => {
      const key = prKey();
      if (key !== pr) {
        pr = key;
        map = new Map();
        if (!pr) return;
        window.postMessage({ __prvScan: true }, location.origin);
        load(pr);
      }
      if (!pr) return;
      await scanToggles();
      await render();
    }, 150);
  }

  window.addEventListener("message", async (e) => {
    if (e.source !== window || e.origin !== location.origin || !e.data || !e.data.__prv || !Array.isArray(e.data.entries)) return;
    if (!pr) return;
    const t = Date.now();
    let changed = false;
    for (const { hash, path, viewed } of e.data.entries) {
      const h = typeof hash === "string" && HEX64.test(hash) ? hash : typeof path === "string" ? await hashPath(path) : null;
      if (h && typeof viewed === "boolean") changed = set(h, viewed, t) || changed;
    }
    debug(e.data.source, e.data.entries.length, "entries");
    if (changed) {
      save();
      render();
    }
  });

  if (pr) load(pr);
  document.addEventListener("click", schedule, true);
  document.addEventListener("change", schedule, true);

  function start() {
    new MutationObserver(schedule).observe(document.body, {
      childList: true,
      subtree: true,
      attributes: true,
      attributeFilter: ["aria-pressed", "checked"],
    });
    schedule();
  }
  if (document.body) start();
  else document.addEventListener("DOMContentLoaded", start);
})();
