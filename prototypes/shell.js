/*
 * <prototype-shell>: the shared top bar of every prototype page.
 *
 * One classic script tag adds it as the first child of <body>:
 *   <script src="shell.js" defer></script>
 * Load it after nostr/account.js when the page uses Nostr sync; the shell then hosts the
 * page's <nostr-account> element (or creates one), so the account badge sits in the bar.
 * A page that places its own <prototype-shell> keeps it; the script does not add a second.
 *
 * The bar scrolls away with the page and slides back in when you scroll up.
 * PrototypeShell.toast(message) shows a short status message near the bottom of the screen,
 * so a prototype needs no status line of its own. A page with its own bottom control (a floating
 * button) lifts the toast with: prototype-shell { --prototype-toast-offset: 5rem; }
 */
(function () {
  "use strict";

  const SHELL = {
    homeUrl: "index.html",
    aboutUrl: "about.html",
    toastMs: 4000,
    scrollShowPx: 12, // scroll up this far to bring the bar back
    scrollHidePx: 4,
    copy: {
      navLabel: "Prototypes",
      home: "All prototypes",
      homeShort: "All",
      about: "About & terms",
      aboutShort: "About"
    }
  };

  const STYLE = `
    :host { display: block; position: relative; z-index: 900; font: 500 0.9rem/1.2 Inter, ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif; color: #172033; }
    nav { display: flex; align-items: center; gap: 0.25rem; min-inline-size: 0; background: #ffffff; border-block-end: 1px solid rgba(23, 32, 51, 0.12);
      padding: 0.25rem max(0.5rem, env(safe-area-inset-right)) 0.25rem max(0.5rem, env(safe-area-inset-left)); }
    /* Past the top of the page the bar is fixed: tucked away while you scroll down, shown when you scroll up. */
    :host([data-scroll="tucked"]) nav, :host([data-scroll="shown"]) nav { position: fixed; inset: 0 0 auto 0; transition: transform 0.2s ease; }
    :host([data-scroll="tucked"]) nav { transform: translateY(-100%); }
    :host([data-scroll="shown"]) nav { transform: none; box-shadow: 0 0.25rem 1rem rgba(23, 32, 51, 0.12); }
    .toast { position: fixed; inset-inline: 0; inset-block-end: calc(var(--prototype-toast-offset, 1rem) + env(safe-area-inset-bottom)); margin-inline: auto; z-index: 1000;
      inline-size: max-content; max-inline-size: min(30rem, calc(100vw - 2rem)); padding: 0.65rem 1rem; border-radius: 0.6rem;
      background: #172033; color: #ffffff; font-weight: 500; line-height: 1.35; box-shadow: 0 0.5rem 1.5rem rgba(23, 32, 51, 0.25);
      opacity: 0; transform: translateY(0.5rem); transition: opacity 0.2s ease, transform 0.2s ease; pointer-events: none; }
    .toast.visible { opacity: 1; transform: none; }
    @media (prefers-reduced-motion: reduce) { nav, .toast { transition: none !important; } }
    a { display: inline-flex; align-items: center; gap: 0.3rem; min-block-size: 2.75rem; padding: 0 0.6rem; border-radius: 0.5rem;
      color: #1d4ed8; text-decoration: none; white-space: nowrap; }
    a:hover, a:focus-visible { background: rgba(37, 99, 235, 0.08); outline: none; }
    a:focus-visible { box-shadow: 0 0 0 2px #1d4ed8; }
    a[aria-current="page"] { color: #172033; }
    [hidden] { display: none !important; } /* the a { display } rule above would otherwise show a hidden link */
    .spacer { flex: 1 1 auto; }
    .short { display: none; }
    ::slotted(nostr-account) { min-inline-size: 0; max-inline-size: 55%; }
    @media (max-width: 26rem) {
      .long { display: none; }
      .short { display: inline; }
    }
  `;

  const fileName = () => {
    const last = window.location.pathname.split("/").pop();
    return last === "" ? SHELL.homeUrl : last;
  };

  class PrototypeShell extends HTMLElement {
    constructor() {
      super();
      const root = this.attachShadow({ mode: "open" });
      const c = SHELL.copy;
      root.innerHTML = `<style>${STYLE}</style>
        <nav part="nav">
          <a class="home" part="home"><span aria-hidden="true">&larr;</span><span class="long"></span><span class="short"></span></a>
          <span class="spacer"></span>
          <a class="about" part="about"><span class="long"></span><span class="short"></span></a>
          <slot name="account"></slot>
        </nav>
        <div class="toast" part="toast" role="status" aria-live="polite"></div>`;
      const nav = root.querySelector("nav");
      nav.setAttribute("aria-label", c.navLabel);
      const home = root.querySelector(".home");
      home.href = SHELL.homeUrl;
      home.querySelector(".long").textContent = c.home;
      home.querySelector(".short").textContent = c.homeShort;
      home.setAttribute("aria-label", c.home);
      const about = root.querySelector(".about");
      about.href = SHELL.aboutUrl;
      about.querySelector(".long").textContent = c.about;
      about.querySelector(".short").textContent = c.aboutShort;
      about.setAttribute("aria-label", c.about);
      const page = fileName();
      if (page === SHELL.homeUrl) home.hidden = true; // the launcher needs no link to itself
      if (page === SHELL.aboutUrl) about.setAttribute("aria-current", "page");
      this.nav = nav;
      this.toastEl = root.querySelector(".toast");
      this.toastTimer = null;
      this.lastY = 0;
      this.peakY = 0;
      this.troughY = 0;
      this.onScroll = this.onScroll.bind(this);
    }

    connectedCallback() {
      // The host keeps the bar's height in the page flow, so the content never jumps when the bar becomes fixed.
      this.resize = new ResizeObserver(() => { if (!this.dataset.scroll || this.dataset.scroll === "top") this.style.minBlockSize = `${this.nav.offsetHeight}px`; });
      this.resize.observe(this.nav);
      this.lastY = window.scrollY;
      window.addEventListener("scroll", this.onScroll, { passive: true });
      this.onScroll();
    }

    disconnectedCallback() {
      window.removeEventListener("scroll", this.onScroll);
      if (this.resize) this.resize.disconnect();
    }

    // States: "top" (in the page flow), "tucked" (fixed, out of view), "shown" (fixed, in view).
    onScroll() {
      const y = Math.max(0, window.scrollY);
      const prev = this.lastY;
      this.lastY = y;
      const top = this.offsetTop;
      const barBottom = top + this.nav.offsetHeight;
      let state = this.dataset.scroll || "top";
      if (y <= top) state = "top";
      else if (y > prev) {
        this.peakY = y;
        if (state === "shown" ? y - this.troughY > SHELL.scrollHidePx : y > barBottom) state = "tucked";
      } else if (y < prev) {
        this.troughY = y;
        if (state === "tucked" && this.peakY - y > SHELL.scrollShowPx) state = "shown";
      }
      if (state !== (this.dataset.scroll || "top")) this.dataset.scroll = state;
    }

    toast(message, { timeoutMs = SHELL.toastMs } = {}) {
      clearTimeout(this.toastTimer);
      this.toastEl.textContent = message;
      this.toastEl.classList.add("visible");
      this.toastTimer = setTimeout(() => {
        this.toastEl.classList.remove("visible");
        this.toastTimer = setTimeout(() => { this.toastEl.textContent = ""; }, 250);
      }, timeoutMs);
    }
  }

  if (!customElements.get("prototype-shell")) customElements.define("prototype-shell", PrototypeShell);

  function mount() {
    let shell = document.querySelector("prototype-shell");
    if (!shell) {
      shell = document.createElement("prototype-shell");
      document.body.prepend(shell);
    }
    if (!customElements.get("nostr-account")) return;
    // The account badge lives in the bar. A page's own <nostr-account> moves there, so its code still finds it.
    const account = document.querySelector("nostr-account") || document.createElement("nostr-account");
    account.slot = "account";
    if (account.parentElement !== shell) shell.append(account);
  }

  if (document.body) mount();
  else document.addEventListener("DOMContentLoaded", mount);

  window.PrototypeShell = {
    toast(message, options) {
      const shell = document.querySelector("prototype-shell");
      if (shell && shell.toast) shell.toast(message, options);
    }
  };
})();
