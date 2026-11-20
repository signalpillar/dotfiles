// Runs in the page (MAIN world). Pulls per-file viewed state out of the JSON
// GitHub already ships to the page: embedded <script type="application/json">
// blocks and the page's own fetch/XHR responses. Posts {path|hash, viewed}
// entries to content.js. Never makes requests of its own.
(() => {
  if (window.__prvHook) return;
  window.__prvHook = true;

  const HEX64 = /^[0-9a-f]{64}$/;
  const STATE = /^(un)?viewed$|^dismissed$/i;

  function viewedOf(o) {
    for (const k in o) {
      if (!/viewed/i.test(k) || /count|total|num|filter/i.test(k)) continue;
      const v = o[k];
      if (typeof v === "boolean") return v;
      if (typeof v === "string" && STATE.test(v)) return v.toUpperCase() === "VIEWED";
    }
    return undefined;
  }

  function extract(root) {
    const out = [];
    const seen = new Set();
    const stack = [root];
    let budget = 2e6;
    while (stack.length && budget-- > 0) {
      const o = stack.pop();
      if (!o || typeof o !== "object" || seen.has(o)) continue;
      seen.add(o);
      if (!Array.isArray(o)) {
        const viewed = viewedOf(o);
        if (viewed !== undefined) {
          const path = o.path ?? o.filePath ?? o.newPath ?? o.headPath;
          const digest = o.pathDigest ?? o.digest ?? o.pathSha;
          if (typeof digest === "string" && HEX64.test(digest)) out.push({ hash: digest, viewed });
          else if (typeof path === "string" && path) out.push({ path, viewed });
        }
      }
      for (const k in o) {
        const v = o[k];
        if (v && typeof v === "object") stack.push(v);
      }
    }
    return out;
  }

  function post(entries, source) {
    if (entries.length) window.postMessage({ __prv: true, source, entries }, location.origin);
  }

  function scanText(text, source) {
    if (!text || text.length > 30e6) return;
    const c = text.trimStart()[0];
    if (c !== "{" && c !== "[") return;
    try {
      post(extract(JSON.parse(text)), source);
    } catch {}
  }

  function scanEmbedded() {
    for (const s of document.querySelectorAll('script[type="application/json"]')) {
      scanText(s.textContent, "embedded");
    }
  }

  const origFetch = window.fetch;
  window.fetch = async function (...args) {
    const res = await origFetch.apply(this, args);
    try {
      if ((res.headers.get("content-type") || "").includes("json")) {
        res.clone().text().then((t) => scanText(t, "fetch"), () => {});
      }
    } catch {}
    return res;
  };

  const origOpen = XMLHttpRequest.prototype.open;
  XMLHttpRequest.prototype.open = function (...args) {
    this.addEventListener("load", () => {
      try {
        if (this.responseType === "json") post(extract(this.response), "xhr");
        else if (this.responseType === "" || this.responseType === "text") scanText(this.responseText, "xhr");
      } catch {}
    });
    return origOpen.apply(this, args);
  };

  window.addEventListener("message", (e) => {
    if (e.source === window && e.origin === location.origin && e.data && e.data.__prvScan) scanEmbedded();
  });
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", scanEmbedded);
  else scanEmbedded();
})();
