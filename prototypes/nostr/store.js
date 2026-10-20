/*
 * Shared Nostr storage for prototypes. See DESIGN.md next to this file.
 *
 * Classic script (not a module), so it also loads from file://.
 * Needs the nostr-tools browser bundle (global NostrTools) for keys, signing and relays.
 * Without it, stores still work on the local copy only.
 *
 * Exposes window.NostrPrototypes = { config, account, openStore, validateSpec }.
 */
(function () {
  "use strict";

  const config = Object.assign({
    defaultRelays: ["wss://relay.damus.io", "wss://nos.lol", "wss://relay.primal.net"],
    kind: 30078,
    relayListKind: 10002,
    labelNamespace: "signalpillar.prototypes",
    accountKey: "nostr-prototypes.account.v1",
    dbName: "nostr-prototypes",
    dbVersion: 1,
    anonOwner: "anon",
    pageLimit: 500,
    queryWaitMs: 8000,
    publishTimeoutMs: 8000,
    publishGapMs: 150,
    retryBaseMs: 2000,
    retryMaxMs: 60000,
    sinceSlackSec: 600,
    fieldTypes: ["string", "number", "boolean", "object", "array"],
    collectionTypes: ["record", "log"]
  }, window.NOSTR_PROTOTYPES_CONFIG || {});

  const NT = () => window.NostrTools || null;
  const nowSec = () => Math.floor(Date.now() / 1000);
  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
  const isRelayUrl = (url) => typeof url === "string" && /^wss?:\/\/[^\s/]+/.test(url);
  const normalizeRelay = (url) => url.trim().replace(/\/+$/, "");
  const uniq = (list) => [...new Set(list)];

  function withTimeout(promise, ms, message) {
    return Promise.race([promise, new Promise((_, reject) => setTimeout(() => reject(new Error(message)), ms))]);
  }

  // ---- Account: one key and one relay list for every prototype on this origin --------------

  const account = (() => {
    const listeners = new Set();
    let state = load();
    let signer = null;
    let pool = null;

    function load() {
      try {
        const raw = JSON.parse(localStorage.getItem(config.accountKey) || "null");
        if (!raw || typeof raw.pubkey !== "string" || !/^[0-9a-f]{64}$/.test(raw.pubkey)) return null;
        if (raw.method !== "local" && raw.method !== "nip07") return null;
        if (raw.method === "local" && typeof raw.nsec !== "string") return null;
        const relays = Array.isArray(raw.relays) ? raw.relays.filter(isRelayUrl).map(normalizeRelay) : null;
        return {
          method: raw.method,
          pubkey: raw.pubkey,
          nsec: raw.method === "local" ? raw.nsec : undefined,
          relays: relays && relays.length ? relays : null,
          relaysSource: relays && relays.length && raw.relaysSource === "nip65" ? "nip65" : "default",
          readOnlyRelays: Array.isArray(raw.readOnlyRelays) ? raw.readOnlyRelays.filter(isRelayUrl) : []
        };
      } catch {
        return null;
      }
    }

    function save() {
      try {
        if (state) localStorage.setItem(config.accountKey, JSON.stringify(state));
        else localStorage.removeItem(config.accountKey);
      } catch {
        /* storage blocked: the account lasts until reload */
      }
    }

    function emit() {
      signer = null;
      for (const fn of [...listeners]) {
        try { fn(); } catch (error) { console.error(error); }
      }
    }

    // Another prototype (another tab on the same origin) signed in or out.
    window.addEventListener("storage", (event) => {
      if (event.key !== config.accountKey) return;
      state = load();
      emit();
    });

    function getPool() {
      if (!NT()) return null;
      if (!pool) pool = new (NT().SimplePool)({ enableReconnect: true });
      return pool;
    }

    function getSigner() {
      if (!state) return null;
      if (signer) return signer;
      const nt = NT();
      if (state.method === "local") {
        if (!nt) return null;
        const decoded = nt.nip19.decode(state.nsec);
        const sk = decoded.data;
        const conversationKey = nt.nip44.v2.utils.getConversationKey(sk, state.pubkey);
        signer = {
          pubkey: state.pubkey,
          sign: async (template) => nt.finalizeEvent(template, sk),
          encrypt: async (text) => nt.nip44.v2.encrypt(text, conversationKey),
          decrypt: async (text) => nt.nip44.v2.decrypt(text, conversationKey)
        };
      } else {
        const ext = window.nostr;
        if (!ext || !ext.nip44) return null;
        signer = {
          pubkey: state.pubkey,
          sign: (template) => ext.signEvent(template),
          encrypt: (text) => ext.nip44.encrypt(state.pubkey, text),
          decrypt: (text) => ext.nip44.decrypt(state.pubkey, text)
        };
      }
      return signer;
    }

    function relays() {
      return state && state.relays
        ? { list: state.relays, source: state.relaysSource }
        : { list: config.defaultRelays.slice(), source: "default" };
    }

    function setState(next) {
      state = next;
      save();
      emit();
    }

    // Reads your NIP-65 relay list (kind 10002). Without one, the library defaults stay.
    async function refreshRelayList() {
      const p = getPool();
      if (!state || !p) return;
      const pubkey = state.pubkey;
      const where = uniq([...config.defaultRelays, ...relays().list]);
      const event = await p.get(where, { kinds: [config.relayListKind], authors: [pubkey] }, { maxWait: config.queryWaitMs });
      if (!event || !state || state.pubkey !== pubkey) return;
      const write = [];
      const readOnly = [];
      for (const tag of event.tags) {
        if (tag[0] !== "r" || !isRelayUrl(tag[1])) continue;
        (tag[2] === "read" ? readOnly : write).push(normalizeRelay(tag[1]));
      }
      if (!write.length) return;
      const same = JSON.stringify(write) === JSON.stringify(relays().list) && state.relaysSource === "nip65";
      if (same) return;
      setState({ ...state, relays: uniq(write), relaysSource: "nip65", readOnlyRelays: uniq(readOnly) });
    }

    async function signInLocal(sk) {
      const nt = NT();
      if (!nt) throw new Error("The Nostr library did not load.");
      const pubkey = nt.getPublicKey(sk);
      setState({ method: "local", pubkey, nsec: nt.nip19.nsecEncode(sk), relays: null, relaysSource: "default", readOnlyRelays: [] });
      refreshRelayList().catch(() => {});
    }

    return {
      available: () => !!NT(),
      extensionAvailable: () => !!(window.nostr && window.nostr.getPublicKey),
      signedIn: () => !!state,
      pubkey: () => (state ? state.pubkey : null),
      npub: () => (state && NT() ? NT().nip19.npubEncode(state.pubkey) : null),
      method: () => (state ? state.method : null),
      nsec: () => (state && state.method === "local" ? state.nsec : null),
      signer: getSigner,
      pool: getPool,
      relays,

      async createKey() {
        await signInLocal(NT().generateSecretKey());
        return state.nsec;
      },

      async signInWithSecret(text) {
        const nt = NT();
        if (!nt) throw new Error("The Nostr library did not load.");
        const value = String(text || "").trim();
        let sk;
        if (/^[0-9a-f]{64}$/i.test(value)) sk = nt.utils.hexToBytes(value.toLowerCase());
        else {
          let decoded;
          try { decoded = nt.nip19.decode(value); } catch { decoded = null; }
          if (!decoded || decoded.type !== "nsec") throw new Error("Paste a secret key that starts with nsec1.");
          sk = decoded.data;
        }
        await signInLocal(sk);
      },

      async signInWithExtension() {
        const ext = window.nostr;
        if (!ext || !ext.getPublicKey) throw new Error("No Nostr browser extension found.");
        if (!ext.nip44 || !ext.nip44.encrypt) throw new Error("This extension cannot encrypt with NIP-44.");
        const pubkey = await ext.getPublicKey();
        if (!/^[0-9a-f]{64}$/.test(pubkey || "")) throw new Error("The extension returned no public key.");
        setState({ method: "nip07", pubkey, relays: null, relaysSource: "default", readOnlyRelays: [] });
        refreshRelayList().catch(() => {});
      },

      signOut() {
        setState(null);
      },

      // Publishes the new list as your NIP-65 relay list, to the old and the new relays.
      async setRelays(list) {
        const clean = uniq(list.filter(isRelayUrl).map(normalizeRelay));
        if (!clean.length) throw new Error("Keep at least one relay.");
        const s = getSigner();
        const p = getPool();
        if (!state || !s || !p) throw new Error("Sign in first.");
        const tags = [...clean.map((url) => ["r", url]), ...state.readOnlyRelays.filter((u) => !clean.includes(u)).map((url) => ["r", url, "read"])];
        const event = await s.sign({ kind: config.relayListKind, created_at: nowSec(), tags, content: "" });
        const targets = uniq([...relays().list, ...clean]);
        await withTimeout(Promise.any(p.publish(targets, event)), config.publishTimeoutMs, "No relay accepted the relay list.");
        setState({ ...state, relays: clean, relaysSource: "nip65" });
      },

      refreshRelayList,

      connectionStatus() {
        const p = pool;
        const map = p ? p.listConnectionStatus() : new Map();
        return relays().list.map((url) => ({ url, connected: map.get(normalizeRelay(url) + "/") || map.get(normalizeRelay(url)) || false }));
      },

      subscribe(fn) {
        listeners.add(fn);
        return () => listeners.delete(fn);
      }
    };
  })();

  // ---- Manifest validation ------------------------------------------------------------------

  function validateSpec(spec) {
    const errors = [];
    if (!spec || typeof spec !== "object") return ["storage block is missing"];
    if (typeof spec.appId !== "string" || !/^[a-z0-9][a-z0-9-]*$/.test(spec.appId)) errors.push("appId must be lowercase letters, digits and hyphens");
    if (spec.relays !== undefined && (!Array.isArray(spec.relays) || !spec.relays.length || !spec.relays.every(isRelayUrl))) errors.push("relays must be a list of ws:// or wss:// URLs");
    const collections = spec.collections;
    if (!collections || typeof collections !== "object" || !Object.keys(collections).length) errors.push("collections must declare at least one collection");
    for (const [name, c] of Object.entries(collections || {})) {
      if (!/^[a-z0-9][a-z0-9-]*$/.test(name)) errors.push(`collection '${name}': name must be lowercase letters, digits and hyphens`);
      if (!c || !config.collectionTypes.includes(c.type)) { errors.push(`collection '${name}': type must be ${config.collectionTypes.join(" or ")}`); continue; }
      for (const [field, type] of Object.entries(c.fields || {})) {
        if (!config.fieldTypes.includes(type)) errors.push(`collection '${name}': field '${field}' has unknown type '${type}'`);
      }
      if (c.required !== undefined && (!Array.isArray(c.required) || !c.required.every((f) => typeof f === "string"))) errors.push(`collection '${name}': required must be a list of field names`);
      if (c.localOnlyIdPrefix !== undefined && typeof c.localOnlyIdPrefix !== "string") errors.push(`collection '${name}': localOnlyIdPrefix must be a string`);
      if (c.public !== undefined && typeof c.public !== "boolean") errors.push(`collection '${name}': public must be true or false`);
    }
    return errors;
  }

  function typeOf(value) {
    if (Array.isArray(value)) return "array";
    if (value === null) return "null";
    return typeof value;
  }

  // Returns an error message, or null when the value fits the collection.
  function checkValue(collection, value) {
    if (!value || typeOf(value) !== "object") return "the value must be an object";
    if (typeof value.id !== "string" || !value.id) return "the value needs a string id";
    for (const field of collection.required || []) {
      if (value[field] === undefined || value[field] === null) return `field '${field}' is required`;
    }
    for (const [field, type] of Object.entries(collection.fields || {})) {
      if (value[field] !== undefined && value[field] !== null && typeOf(value[field]) !== type) return `field '${field}' must be ${type}`;
    }
    return null;
  }

  // ---- IndexedDB ----------------------------------------------------------------------------

  const request = (req) => new Promise((resolve, reject) => {
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
  const done = (tx) => new Promise((resolve, reject) => {
    tx.oncomplete = () => resolve();
    tx.onerror = tx.onabort = () => reject(tx.error);
  });

  let dbPromise = null;
  function openDb() {
    if (!dbPromise) {
      dbPromise = new Promise((resolve, reject) => {
        const req = indexedDB.open(config.dbName, config.dbVersion);
        req.onupgradeneeded = () => {
          const db = req.result;
          const items = db.createObjectStore("items", { keyPath: ["owner", "app", "collection", "id"] });
          items.createIndex("byCollection", ["owner", "app", "collection"]);
          items.createIndex("byApp", ["owner", "app"]);
          const outbox = db.createObjectStore("outbox", { keyPath: ["owner", "app", "collection", "id"] });
          outbox.createIndex("byApp", ["owner", "app"]);
          db.createObjectStore("meta", { keyPath: "key" });
        };
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
      });
    }
    return dbPromise;
  }

  // ---- Store --------------------------------------------------------------------------------

  class Store {
    constructor(spec, db) {
      this.spec = spec;
      this.app = spec.appId;
      this.db = db;
      this.owner = null;
      this.cache = new Map();
      this.listeners = new Set();
      this.statusListeners = new Set();
      this.generation = 0;
      this.liveSub = null;
      this.publishing = false;
      this.retryMs = config.retryBaseMs;
      this.retryTimer = null;
      this.relayKey = "";
      this.pendingEmit = new Set();
      this.status = { state: "local", pending: 0, lastSyncAt: null, error: null };
      for (const name of Object.keys(spec.collections)) this.cache.set(name, new Map());
    }

    collection(name) {
      const c = this.spec.collections[name];
      if (!c) throw new Error(`Unknown collection '${name}'.`);
      return c;
    }

    label(name) { return `${this.app}/${name}`; }
    synced() { return this.owner !== config.anonOwner && account.available() && !!account.signer(); }
    relayList() { return this.spec.relays ? this.spec.relays.map(normalizeRelay) : account.relays().list; }
    relaySource() { return this.spec.relays ? "prototype" : account.relays().source; }
    localOnly(name, id) {
      const prefix = this.collection(name).localOnlyIdPrefix;
      return !!prefix && id.startsWith(prefix);
    }

    async init() {
      this.offAccount = account.subscribe(() => { this.switchOwner().catch((error) => this.fail(error)); });
      window.addEventListener("online", () => this.restartSync());
      await this.switchOwner();
    }

    // ---- reading ----

    list(name) {
      this.collection(name);
      return [...this.cache.get(name).values()]
        .filter((row) => !row.deleted)
        .sort((a, b) => a.updatedAt - b.updatedAt || (a.id < b.id ? -1 : 1))
        .map((row) => row.value);
    }

    get(name, id) {
      const row = this.cache.get(name) && this.cache.get(name).get(id);
      return row && !row.deleted ? row.value : null;
    }

    subscribe(fn) {
      this.listeners.add(fn);
      return () => this.listeners.delete(fn);
    }

    subscribeStatus(fn) {
      this.statusListeners.add(fn);
      fn(this.status);
      return () => this.statusListeners.delete(fn);
    }

    emit(change) {
      for (const fn of [...this.listeners]) {
        try { fn(change); } catch (error) { console.error(error); }
      }
    }

    setStatus(patch) {
      this.status = { ...this.status, ...patch };
      for (const fn of [...this.statusListeners]) {
        try { fn(this.status); } catch (error) { console.error(error); }
      }
    }

    fail(error) {
      console.error(error);
      this.setStatus({ state: "error", error: String((error && error.message) || error) });
    }

    // ---- writing ----

    async write(name, row) {
      const tx = this.db.transaction(["items", "outbox"], "readwrite");
      tx.objectStore("items").put(row);
      if (this.synced() && !this.localOnly(name, row.id)) {
        tx.objectStore("outbox").put({ owner: row.owner, app: row.app, collection: name, id: row.id, queuedAt: Date.now() });
      }
      await done(tx);
      this.cache.get(name).set(row.id, row);
      this.emit({ reason: "local", collection: name });
      this.kickPublisher();
    }

    makeRow(name, id, value, deleted) {
      return { owner: this.owner, app: this.app, collection: name, id, value, updatedAt: Date.now(), deleted, createdAt: 0 };
    }

    async append(name, value) {
      const c = this.collection(name);
      if (c.type !== "log") throw new Error(`'${name}' is a ${c.type} collection; use put().`);
      const problem = checkValue(c, value);
      if (problem) throw new Error(`${name}: ${problem}`);
      if (this.cache.get(name).has(value.id)) return false;
      await this.write(name, this.makeRow(name, value.id, value, false));
      return true;
    }

    async put(name, value) {
      const c = this.collection(name);
      if (c.type !== "record") throw new Error(`'${name}' is a ${c.type} collection; use append().`);
      const problem = checkValue(c, value);
      if (problem) throw new Error(`${name}: ${problem}`);
      const previous = this.cache.get(name).get(value.id);
      const row = this.makeRow(name, value.id, value, false);
      row.createdAt = previous ? previous.createdAt : 0;
      await this.write(name, row);
    }

    async remove(name, id) {
      const c = this.collection(name);
      if (c.type !== "record") throw new Error(`'${name}' is a log; logs never delete entries.`);
      const previous = this.cache.get(name).get(id);
      if (!previous || previous.deleted) return;
      const row = this.makeRow(name, id, null, true);
      row.createdAt = previous.createdAt;
      await this.write(name, row);
    }

    // Deletes the local copy of one collection for the current owner. Relays keep their copy.
    async clearLocal(name) {
      this.collection(name);
      const tx = this.db.transaction(["items", "outbox"], "readwrite");
      for (const id of this.cache.get(name).keys()) {
        tx.objectStore("items").delete([this.owner, this.app, name, id]);
        tx.objectStore("outbox").delete([this.owner, this.app, name, id]);
      }
      await done(tx);
      this.cache.set(name, new Map());
      this.emit({ reason: "local", collection: name });
    }

    // ---- owner switch ----

    async switchOwner() {
      const owner = account.pubkey() || config.anonOwner;
      const relayKey = this.relayList().join(" ");
      if (owner === this.owner) {
        if (relayKey !== this.relayKey) this.restartSync();
        return;
      }
      this.stopSync();
      this.generation += 1;
      if (owner !== config.anonOwner) await this.adoptAnonymous(owner);
      this.owner = owner;
      await this.loadCache();
      this.setStatus({ state: owner === config.anonOwner ? "local" : "syncing", pending: 0, error: null });
      this.emit({ reason: "owner" });
      if (owner !== config.anonOwner) this.startSync();
    }

    // At the first sign-in, anonymous items move to the key's copy and wait in the outbox.
    async adoptAnonymous(owner) {
      const anon = await request(this.db.transaction("items").objectStore("items").index("byApp").getAll([config.anonOwner, this.app]));
      if (!anon.length) return;
      const tx = this.db.transaction(["items", "outbox"], "readwrite");
      const items = tx.objectStore("items");
      const outbox = tx.objectStore("outbox");
      for (const row of anon) {
        if (!this.spec.collections[row.collection]) continue;
        items.delete([config.anonOwner, this.app, row.collection, row.id]);
        const existing = await request(items.get([owner, this.app, row.collection, row.id]));
        if (existing && existing.updatedAt >= row.updatedAt) continue;
        items.put({ ...row, owner });
        if (!this.localOnly(row.collection, row.id)) outbox.put({ owner, app: this.app, collection: row.collection, id: row.id, queuedAt: Date.now() });
      }
      await done(tx);
    }

    async loadCache() {
      const index = this.db.transaction("items").objectStore("items").index("byCollection");
      for (const name of Object.keys(this.spec.collections)) {
        const rows = await request(index.getAll([this.owner, this.app, name]));
        this.cache.set(name, new Map(rows.map((row) => [row.id, row])));
      }
    }

    // ---- sync ----

    stopSync() {
      if (this.liveSub) { this.liveSub.close(); this.liveSub = null; }
      clearTimeout(this.retryTimer);
      this.retryTimer = null;
    }

    restartSync() {
      if (!this.owner || this.owner === config.anonOwner) return;
      this.stopSync();
      this.generation += 1;
      this.startSync();
    }

    syncNow() {
      this.retryMs = config.retryBaseMs;
      this.restartSync();
    }

    startSync() {
      const gen = this.generation;
      this.relayKey = this.relayList().join(" ");
      if (!this.synced()) {
        this.setStatus({ state: "error", error: account.available() ? "This key cannot sign here. Sign in again." : "The Nostr library did not load." });
        return;
      }
      // A retry keeps "offline" until it succeeds, so the badge does not blink between states.
      if (this.status.state !== "offline") this.setStatus({ state: "syncing", error: null });
      this.pull(gen)
        .then(() => {
          if (gen !== this.generation) return;
          this.subscribeLive(gen);
          return this.publish(gen);
        })
        .catch((error) => {
          if (gen !== this.generation) return;
          this.setStatus({ state: "offline", error: String((error && error.message) || error) });
          this.scheduleRetry();
        });
    }

    scheduleRetry() {
      clearTimeout(this.retryTimer);
      this.retryTimer = setTimeout(() => this.restartSync(), this.retryMs);
      this.retryMs = Math.min(this.retryMs * 2, config.retryMaxMs);
    }

    async metaGet(key) {
      const row = await request(this.db.transaction("meta").objectStore("meta").get(key));
      return row ? row.value : null;
    }

    async metaSet(key, value) {
      const tx = this.db.transaction("meta", "readwrite");
      tx.objectStore("meta").put({ key, value });
      await done(tx);
    }

    // Fetches each collection since the last sync. A relay returns at most pageLimit events, so it pages back with until.
    async pull(gen) {
      const pool = account.pool();
      const relays = this.relayList();
      for (const name of Object.keys(this.spec.collections)) {
        const metaKey = `since:${this.owner}:${this.label(name)}`;
        const last = await this.metaGet(metaKey);
        const filter = { kinds: [config.kind], authors: [this.owner], "#l": [this.label(name)] };
        if (last) filter.since = Math.max(0, last - config.sinceSlackSec);
        const seen = new Set();
        let newest = last || 0;
        let until;
        let answered = false;
        for (;;) {
          if (gen !== this.generation) return;
          const page = { ...filter, limit: config.pageLimit };
          if (until !== undefined) page.until = until;
          let closed = [];
          const events = await new Promise((resolve) => {
            const got = [];
            pool.subscribeEose(relays, page, {
              maxWait: config.queryWaitMs,
              onevent: (event) => got.push(event),
              onclose: (reasons) => { closed = reasons || []; resolve(got); }
            });
          });
          if (closed.some((r) => /closed automatically on eose/.test(r.reason || ""))) answered = true;
          const fresh = events.filter((e) => !seen.has(e.id));
          for (const event of fresh) {
            seen.add(event.id);
            newest = Math.max(newest, event.created_at);
            await this.ingest(name, event);
          }
          if (events.length < config.pageLimit || !fresh.length) break;
          until = Math.min(...events.map((e) => e.created_at));
        }
        if (!answered && !seen.size) throw new Error("No relay answered.");
        if (newest) await this.metaSet(metaKey, newest);
      }
      this.flushEmit();
      this.setStatus({ lastSyncAt: Date.now() });
    }

    subscribeLive(gen) {
      const labels = Object.keys(this.spec.collections).map((name) => this.label(name));
      this.liveSub = account.pool().subscribe(this.relayList(), { kinds: [config.kind], authors: [this.owner], "#l": labels, since: nowSec() - config.sinceSlackSec }, {
        onevent: (event) => {
          if (gen !== this.generation) return;
          const label = (event.tags.find((t) => t[0] === "l") || [])[1] || "";
          const name = label.slice(this.app.length + 1);
          if (!this.spec.collections[name]) return;
          this.ingest(name, event).then(() => this.flushEmit()).catch((error) => console.error(error));
        }
      });
    }

    // Merges one relay event into the local copy. Returns true when the copy changed.
    async ingest(name, event) {
      if (event.kind !== config.kind || event.pubkey !== this.owner) return false;
      const d = (event.tags.find((t) => t[0] === "d") || [])[1] || "";
      const prefix = `${this.label(name)}/`;
      if (!d.startsWith(prefix)) return false;
      const id = d.slice(prefix.length);
      const c = this.collection(name);
      let body;
      try {
        const text = c.public ? event.content : await account.signer().decrypt(event.content);
        body = JSON.parse(text);
      } catch {
        return false; // not ours, or not readable with this key
      }
      if (!body || body.v !== 1 || typeof body.updatedAt !== "number") return false;
      const deleted = body.deleted === true;
      if (!deleted && (checkValue(c, body.value) || body.value.id !== id)) return false;
      const current = this.cache.get(name).get(id);
      if (c.type === "log" && current) return false;
      if (c.type === "record" && current && current.updatedAt >= body.updatedAt) return false;
      const row = { owner: this.owner, app: this.app, collection: name, id, value: deleted ? null : body.value, updatedAt: body.updatedAt, deleted, createdAt: event.created_at };
      const tx = this.db.transaction(["items", "outbox"], "readwrite");
      tx.objectStore("items").put(row);
      tx.objectStore("outbox").delete([this.owner, this.app, name, id]); // a newer remote write supersedes a pending one
      await done(tx);
      this.cache.get(name).set(id, row);
      this.pendingEmit.add(name);
      return true;
    }

    flushEmit() {
      for (const name of this.pendingEmit) this.emit({ reason: "remote", collection: name });
      this.pendingEmit.clear();
    }

    kickPublisher() {
      if (!this.synced() || this.status.state === "error") return;
      this.publish(this.generation).catch(() => {});
    }

    async toEvent(row) {
      const c = this.collection(row.collection);
      const body = JSON.stringify({ v: 1, value: row.value, updatedAt: row.updatedAt, deleted: row.deleted });
      const s = account.signer();
      const content = c.public ? body : await s.encrypt(body);
      // A relay keeps the newest addressable event, so created_at must grow with every write of one id.
      const createdAt = Math.max(Math.floor(row.updatedAt / 1000), (row.createdAt || 0) + 1);
      return s.sign({
        kind: config.kind,
        created_at: createdAt,
        tags: [["d", `${this.label(row.collection)}/${row.id}`], ["L", config.labelNamespace], ["l", this.label(row.collection), config.labelNamespace]],
        content
      });
    }

    async publish(gen) {
      if (this.publishing || !this.synced()) return;
      this.publishing = true;
      try {
        const relays = this.relayList();
        for (;;) {
          if (gen !== this.generation) return;
          const rows = await request(this.db.transaction("outbox").objectStore("outbox").index("byApp").getAll([this.owner, this.app]));
          this.setStatus({ pending: rows.length });
          if (!rows.length) break;
          const job = rows[0];
          const row = this.cache.get(job.collection) && this.cache.get(job.collection).get(job.id);
          if (row) {
            this.setStatus({ state: "syncing" });
            const event = await this.toEvent(row);
            await withTimeout(Promise.any(account.pool().publish(relays, event)), config.publishTimeoutMs, "No relay accepted the event.");
            row.createdAt = event.created_at;
            const tx = this.db.transaction(["items", "outbox"], "readwrite");
            tx.objectStore("items").put(row);
            const store = tx.objectStore("outbox");
            const latest = await request(store.get([job.owner, job.app, job.collection, job.id]));
            if (latest && latest.queuedAt === job.queuedAt) store.delete([job.owner, job.app, job.collection, job.id]);
            await done(tx);
          } else {
            const tx = this.db.transaction("outbox", "readwrite");
            tx.objectStore("outbox").delete([job.owner, job.app, job.collection, job.id]);
            await done(tx);
          }
          await sleep(config.publishGapMs);
        }
        this.retryMs = config.retryBaseMs;
        if (gen === this.generation) this.setStatus({ state: "synced", pending: 0, error: null, lastSyncAt: Date.now() });
      } catch (error) {
        if (gen === this.generation) {
          const message = error instanceof AggregateError ? "No relay accepted the event." : String((error && error.message) || error);
          this.setStatus({ state: "offline", error: message });
          this.scheduleRetry();
        }
      } finally {
        this.publishing = false;
      }
    }
  }

  async function openStore(spec) {
    const errors = validateSpec(spec);
    if (errors.length) throw new Error(`Invalid storage block: ${errors.join("; ")}`);
    if (!window.indexedDB) throw new Error("IndexedDB is not available.");
    const store = new Store(spec, await openDb());
    await store.init();
    return store;
  }

  window.NostrPrototypes = { config, account, openStore, validateSpec };
})();
