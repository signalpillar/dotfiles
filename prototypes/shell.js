/*
 * <prototype-shell>: the shared top bar of every prototype page.
 *
 * One classic script tag adds it as the first child of <body>:
 *   <script src="shell.js" defer></script>
 * Load it after nostr/account.js when the page uses Nostr sync; the shell then hosts the
 * page's <nostr-account> element (or creates one), so the account badge sits in the bar.
 * A page that places its own <prototype-shell> keeps it; the script does not add a second.
 */
(function () {
  "use strict";

  const SHELL = {
    homeUrl: "index.html",
    aboutUrl: "about.html",
    copy: {
      navLabel: "Prototypes",
      home: "All prototypes",
      homeShort: "All",
      about: "About & terms",
      aboutShort: "About"
    }
  };

  const STYLE = `
    :host { display: block; font: 500 0.9rem/1.2 Inter, ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif;
      color: #172033; background: #ffffff; border-block-end: 1px solid rgba(23, 32, 51, 0.12); }
    nav { display: flex; align-items: center; gap: 0.25rem; min-inline-size: 0;
      padding: 0.25rem max(0.5rem, env(safe-area-inset-right)) 0.25rem max(0.5rem, env(safe-area-inset-left)); }
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
        </nav>`;
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
})();
