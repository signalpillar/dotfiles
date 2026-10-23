/*
 * <nostr-account>: the shared sign-in, key backup, relay and sync drawer. See DESIGN.md.
 *
 * Load after store.js. Give it the prototype's store to show that store's sync state:
 *   document.querySelector("nostr-account").store = store;
 * Override any text with element.copy = { key: "text" }.
 */
(function () {
  "use strict";

  const COPY = {
    badgeUnavailable: "Local only",
    badgeLocal: "Sign in to sync",
    badgeSynced: "Synced",
    badgeSyncing: "Syncing",
    badgePending: "{count} to sync",
    badgeOffline: "Offline",
    badgeError: "Sync problem",
    title: "Nostr sync",
    close: "Close",
    unavailable: "The Nostr library did not load, so this page keeps data on this device only.",
    intro: "Sign in with a Nostr key to sync this prototype between your devices. Data is encrypted to your key before it leaves the browser. One sign-in works for every prototype on this site.",
    localNote: "Without a key, data stays on this device. At sign-in it moves to your key and uploads.",
    create: "Create a new key",
    extension: "Use browser extension",
    extensionMissing: "No NIP-07 browser extension found.",
    pasteLabel: "Or paste your secret key (nsec)",
    pastePlaceholder: "nsec1...",
    signIn: "Sign in",
    signedInAs: "Signed in as",
    methodLocal: "Key stored in this browser",
    methodExtension: "Key in your browser extension",
    copy: "Copy",
    copied: "Copied",
    backupTitle: "Back up your secret key",
    backupWarning: "Anyone with this key can read and change your synced data. Without it you lose access when this browser forgets it. Store it in a password manager.",
    backupNew: "Save this new key now.",
    showSecret: "Show secret key",
    hideSecret: "Hide secret key",
    syncTitle: "Sync",
    stateLocal: "Data stays on this device.",
    stateSyncing: "Syncing with relays.",
    stateSynced: "All changes are on the relays.",
    stateOffline: "Cannot reach the relays. Changes wait on this device and retry.",
    stateError: "Sync stopped: {error}",
    pending: "{count} change(s) wait to upload.",
    lastSync: "Last sync: {time}",
    syncNow: "Sync now",
    noStore: "This page has no synced data.",
    relaysTitle: "Relays",
    relaysDefault: "Library defaults. Your edits publish your NIP-65 relay list.",
    relaysNip65: "From your NIP-65 relay list. Other Nostr apps use this list too.",
    relaysPrototype: "This prototype sets its own relays.",
    remove: "Remove",
    addRelayLabel: "Add relay",
    addRelayPlaceholder: "wss://relay.example.com",
    add: "Add",
    badRelay: "A relay address starts with wss://",
    connected: "connected",
    notConnected: "not connected",
    signOut: "Sign out",
    signOutNote: "Signing out shows this device's local data. Your synced data stays on the relays."
  };

  const STYLE = `
    :host { display: inline-block; font: inherit; color: inherit;
      --na-accent: var(--pico-primary, #1971c2); --na-ink: var(--pico-color, #172033); --na-muted: var(--pico-muted-color, #5d6678);
      --na-line: var(--pico-muted-border-color, rgba(23,32,51,.16)); --na-bg: var(--pico-card-background-color, #fff);
      --na-ok: #2f9e44; --na-warn: #e8590c; --na-bad: #c92a2a; --na-radius: var(--pico-border-radius, .5rem); }
    * { box-sizing: border-box; }
    button { font: inherit; cursor: pointer; border-radius: var(--na-radius); min-block-size: 2.75rem; padding: .4rem .8rem;
      border: 1px solid var(--na-accent); background: var(--na-accent); color: #fff; }
    button.secondary { background: transparent; color: var(--na-accent); }
    button:disabled { opacity: .5; cursor: not-allowed; }
    .badge { display: inline-flex; align-items: center; gap: .45rem; background: transparent; color: var(--na-ink); border-color: var(--na-line);
      min-block-size: 2.5rem; padding: .3rem .7rem; font-size: .85rem; max-inline-size: 100%; }
    .badge .text { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .dot { inline-size: .6rem; block-size: .6rem; border-radius: 50%; background: var(--na-muted); flex: none; }
    .dot.synced { background: var(--na-ok); } .dot.syncing { background: var(--na-accent); }
    .dot.offline { background: var(--na-warn); } .dot.error { background: var(--na-bad); }
    dialog { border: 1px solid var(--na-line); background: var(--na-bg); color: var(--na-ink); padding: 0;
      inline-size: 100%; max-inline-size: 100%; max-block-size: 85vh; margin: auto 0 0; border-radius: var(--na-radius) var(--na-radius) 0 0;
      box-shadow: 0 -.5rem 1.5rem rgba(23,32,51,.18); }
    dialog::backdrop { background: rgba(23,32,51,.25); }
    @media (min-width: 48rem) {
      dialog { margin: 0 0 0 auto; inline-size: 26rem; max-block-size: 100vh; block-size: 100vh; border-radius: 0; }
    }
    .body { display: grid; gap: 1rem; padding: 1rem 1rem calc(1rem + env(safe-area-inset-bottom)); }
    .head { display: flex; justify-content: space-between; align-items: center; gap: .5rem; }
    h2 { margin: 0; font-size: 1.15rem; } h3 { margin: 0 0 .4rem; font-size: .8rem; text-transform: uppercase; letter-spacing: .08em; color: var(--na-muted); }
    p { margin: 0; line-height: 1.45; } .muted { color: var(--na-muted); font-size: .85rem; }
    .row { display: flex; flex-wrap: wrap; gap: .5rem; align-items: center; }
    .stack { display: grid; gap: .5rem; }
    code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: .8rem; overflow-wrap: anywhere; background: rgba(23,32,51,.05); padding: .15rem .3rem; border-radius: .25rem; }
    input { font: inherit; inline-size: 100%; min-block-size: 2.75rem; padding: .4rem .6rem; border: 1px solid var(--na-line); border-radius: var(--na-radius); background: #fff; color: var(--na-ink); }
    label { font-size: .9rem; }
    .error { color: var(--na-bad); font-size: .85rem; min-block-size: 1.2em; }
    .warning { border-inline-start: 3px solid var(--na-warn); padding-inline-start: .6rem; }
    ul { list-style: none; margin: 0; padding: 0; display: grid; gap: .35rem; }
    li { display: flex; gap: .5rem; align-items: center; flex-wrap: wrap; font-size: .85rem; }
    li code { flex: 1 1 10rem; }
    li button { min-block-size: 2.5rem; padding: .2rem .6rem; font-size: .8rem; }
    section { display: grid; gap: .5rem; border-block-start: 1px solid var(--na-line); padding-block-start: 1rem; }
  `;

  const fill = (tpl, vars) => tpl.replace(/\{(\w+)\}/g, (all, key) => (key in vars ? String(vars[key]) : all));
  const shortNpub = (npub) => (npub ? `${npub.slice(0, 10)}...${npub.slice(-4)}` : "");

  class NostrAccount extends HTMLElement {
    constructor() {
      super();
      this.copyText = { ...COPY };
      this._store = null;
      this.offStatus = null;
      this.offAccount = null;
      this.showSecret = false;
      this.freshKey = false;
      this.error = "";
      const root = this.attachShadow({ mode: "open" });
      root.innerHTML = `<style>${STYLE}</style>
        <button class="badge" part="badge" type="button" aria-haspopup="dialog"><span class="dot"></span><span class="text"></span></button>
        <dialog part="drawer" aria-labelledby="na-title"><div class="body"></div></dialog>`;
      this.badge = root.querySelector(".badge");
      this.dialog = root.querySelector("dialog");
      this.body = root.querySelector(".body");
      this.badge.addEventListener("click", () => this.open());
      this.dialog.addEventListener("click", (event) => { if (event.target === this.dialog) this.close(); });
      this.dialog.addEventListener("close", () => { this.showSecret = false; this.freshKey = false; this.error = ""; });
    }

    set copy(overrides) { this.copyText = { ...COPY, ...(overrides || {}) }; this.render(); }
    get copy() { return this.copyText; }

    set store(store) {
      if (this.offStatus) this.offStatus();
      this._store = store;
      this.offStatus = store ? store.subscribeStatus(() => this.render()) : null;
      this.render();
    }
    get store() { return this._store; }

    get api() { return window.NostrPrototypes ? window.NostrPrototypes.account : null; }

    connectedCallback() {
      if (this.api && !this.offAccount) this.offAccount = this.api.subscribe(() => this.render());
      this.render();
    }

    disconnectedCallback() {
      if (this.offAccount) { this.offAccount(); this.offAccount = null; }
    }

    open() {
      if (!this.dialog.open) this.dialog.showModal();
      this.render();
    }

    close() { if (this.dialog.open) this.dialog.close(); }

    t(key, vars) { return vars ? fill(this.copyText[key], vars) : this.copyText[key]; }

    statusOf() {
      const api = this.api;
      if (!api || !api.available()) return { cls: "", badge: this.t("badgeUnavailable") };
      if (!api.signedIn()) return { cls: "", badge: this.t("badgeLocal") };
      const s = this._store ? this._store.status : { state: "synced", pending: 0 };
      const who = shortNpub(api.npub());
      if (s.state === "error") return { cls: "error", badge: `${who} · ${this.t("badgeError")}` };
      if (s.state === "offline") return { cls: "offline", badge: `${who} · ${this.t("badgeOffline")}` };
      if (s.pending) return { cls: "syncing", badge: `${who} · ${this.t("badgePending", { count: s.pending })}` };
      if (s.state === "syncing") return { cls: "syncing", badge: `${who} · ${this.t("badgeSyncing")}` };
      return { cls: "synced", badge: `${who} · ${this.t("badgeSynced")}` };
    }

    render() {
      const status = this.statusOf();
      this.badge.querySelector(".dot").className = `dot ${status.cls}`;
      this.badge.querySelector(".text").textContent = status.badge;
      this.badge.title = status.badge;
      if (this.dialog.open) this.renderDrawer();
    }

    el(tag, props = {}, ...children) {
      const node = Object.assign(document.createElement(tag), props);
      node.append(...children.filter((c) => c !== null && c !== undefined && c !== false));
      return node;
    }

    button(text, onClick, cls = "") {
      const b = this.el("button", { type: "button", textContent: text, className: cls });
      b.addEventListener("click", onClick);
      return b;
    }

    async guard(action) {
      this.error = "";
      try { await action(); } catch (error) { this.error = String((error && error.message) || error); }
      this.render();
    }

    copyButton(text) {
      const b = this.button(this.t("copy"), async () => {
        try { await navigator.clipboard.writeText(text); b.textContent = this.t("copied"); } catch { /* clipboard blocked: the text stays visible */ }
      }, "secondary");
      return b;
    }

    renderDrawer() {
      const api = this.api;
      const head = this.el("div", { className: "head" },
        this.el("h2", { id: "na-title", textContent: this.t("title") }),
        this.button(this.t("close"), () => this.close(), "secondary"));
      const parts = [head];
      if (!api || !api.available()) {
        parts.push(this.el("p", { className: "warning", textContent: this.t("unavailable") }));
      } else if (!api.signedIn()) {
        parts.push(...this.signInParts(api));
      } else {
        parts.push(...this.accountParts(api));
      }
      this.body.replaceChildren(...parts);
    }

    signInParts(api) {
      const input = this.el("input", { type: "password", id: "na-secret", placeholder: this.t("pastePlaceholder"), autocomplete: "off", spellcheck: false });
      const form = this.el("form", { className: "stack" },
        this.el("label", { htmlFor: "na-secret", textContent: this.t("pasteLabel") }),
        input,
        this.el("div", { className: "row" }, this.el("button", { type: "submit", textContent: this.t("signIn") })));
      form.addEventListener("submit", (event) => {
        event.preventDefault();
        this.guard(() => api.signInWithSecret(input.value));
      });
      const ext = this.button(this.t("extension"), () => this.guard(() => api.signInWithExtension()), "secondary");
      ext.disabled = !api.extensionAvailable();
      return [
        this.el("p", { textContent: this.t("intro") }),
        this.el("p", { className: "muted", textContent: this.t("localNote") }),
        this.el("div", { className: "row" },
          this.button(this.t("create"), () => this.guard(async () => { await api.createKey(); this.freshKey = true; this.showSecret = true; })),
          ext),
        api.extensionAvailable() ? null : this.el("p", { className: "muted", textContent: this.t("extensionMissing") }),
        form,
        this.el("p", { className: "error", role: "alert", textContent: this.error })
      ];
    }

    accountParts(api) {
      const npub = api.npub();
      const parts = [
        this.el("div", { className: "stack" },
          this.el("p", { className: "muted", textContent: this.t("signedInAs") }),
          this.el("div", { className: "row" }, this.el("code", { className: "npub", textContent: npub }), this.copyButton(npub)),
          this.el("p", { className: "muted", textContent: api.method() === "local" ? this.t("methodLocal") : this.t("methodExtension") }))
      ];
      if (api.method() === "local") {
        const nsec = api.nsec();
        const backup = this.el("section", {},
          this.el("h3", { textContent: this.t("backupTitle") }),
          this.freshKey ? this.el("p", { className: "warning", textContent: this.t("backupNew") }) : null,
          this.el("p", { className: "muted", textContent: this.t("backupWarning") }),
          this.showSecret ? this.el("div", { className: "row" }, this.el("code", { className: "nsec", textContent: nsec }), this.copyButton(nsec)) : null,
          this.el("div", { className: "row" }, this.button(this.showSecret ? this.t("hideSecret") : this.t("showSecret"), () => { this.showSecret = !this.showSecret; this.render(); }, "secondary")));
        parts.push(backup);
      }
      parts.push(this.syncSection(), this.relaySection(api));
      parts.push(this.el("section", {},
        this.el("p", { className: "muted", textContent: this.t("signOutNote") }),
        this.el("div", { className: "row" }, this.button(this.t("signOut"), () => { api.signOut(); this.close(); }, "secondary"))));
      parts.push(this.el("p", { className: "error", role: "alert", textContent: this.error }));
      return parts;
    }

    syncSection() {
      const store = this._store;
      const section = this.el("section", { className: "sync" }, this.el("h3", { textContent: this.t("syncTitle") }));
      if (!store) {
        section.append(this.el("p", { className: "muted", textContent: this.t("noStore") }));
        return section;
      }
      const s = store.status;
      const stateText = {
        local: this.t("stateLocal"),
        syncing: this.t("stateSyncing"),
        synced: this.t("stateSynced"),
        offline: this.t("stateOffline"),
        error: this.t("stateError", { error: s.error || "" })
      }[s.state] || "";
      section.append(
        this.el("p", { className: "state", textContent: stateText }),
        s.pending ? this.el("p", { className: "muted pending", textContent: this.t("pending", { count: s.pending }) }) : null,
        s.lastSyncAt ? this.el("p", { className: "muted", textContent: this.t("lastSync", { time: new Date(s.lastSyncAt).toLocaleString() }) }) : null,
        this.el("div", { className: "row" }, this.button(this.t("syncNow"), () => { store.syncNow(); this.render(); }, "secondary")));
      return section;
    }

    relaySection(api) {
      const store = this._store;
      const fixed = !!(store && store.spec.relays);
      const source = fixed ? "prototype" : api.relays().source;
      const list = fixed ? store.relayList() : api.relays().list;
      const status = new Map(api.connectionStatus().map((r) => [r.url, r.connected]));
      const items = list.map((url) => {
        const li = this.el("li", {},
          this.el("span", { className: `dot ${status.get(url) ? "synced" : ""}`, title: status.get(url) ? this.t("connected") : this.t("notConnected") }),
          this.el("code", { textContent: url }));
        if (!fixed && list.length > 1) li.append(this.button(this.t("remove"), () => this.guard(() => api.setRelays(list.filter((u) => u !== url))), "secondary"));
        return li;
      });
      const section = this.el("section", { className: "relays" },
        this.el("h3", { textContent: this.t("relaysTitle") }),
        this.el("p", { className: "muted", textContent: { default: this.t("relaysDefault"), nip65: this.t("relaysNip65"), prototype: this.t("relaysPrototype") }[source] }),
        this.el("ul", {}, ...items));
      if (!fixed) {
        const input = this.el("input", { type: "url", id: "na-relay", placeholder: this.t("addRelayPlaceholder"), autocomplete: "off" });
        const form = this.el("form", { className: "stack" },
          this.el("label", { htmlFor: "na-relay", textContent: this.t("addRelayLabel") }),
          this.el("div", { className: "row" }, input, this.el("button", { type: "submit", textContent: this.t("add") })));
        form.addEventListener("submit", (event) => {
          event.preventDefault();
          const url = input.value.trim();
          if (!/^wss?:\/\/\S+/.test(url)) { this.error = this.t("badRelay"); this.render(); return; }
          this.guard(() => api.setRelays([...list, url]));
        });
        section.append(form);
      }
      return section;
    }
  }

  if (!customElements.get("nostr-account")) customElements.define("nostr-account", NostrAccount);
})();
