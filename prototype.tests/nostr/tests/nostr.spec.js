const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");
const { startFakeRelay, NostrTools } = require("./fake-relay");

const workspace = path.resolve(__dirname, "../../..");
const fixtures = path.join(__dirname, "fixtures");
const BUNDLE_URL = "https://cdn.jsdelivr.net/npm/nostr-tools@2.25.2/lib/nostr.bundle.js";
const chromeCandidates = [process.env.CHROME_PATH, "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"];
const chromePath = chromeCandidates.find((candidate) => candidate && fs.existsSync(candidate));
if (chromePath) test.use({ launchOptions: { executablePath: chromePath } });

const SK = NostrTools.generateSecretKey();
const PK = NostrTools.getPublicKey(SK);
const NSEC = NostrTools.nip19.nsecEncode(SK);
const conversationKey = NostrTools.nip44.v2.utils.getConversationKey(SK, PK);

let server;
let baseUrl;
let relay;

test.beforeAll(async () => {
  if (!fs.existsSync(path.join(fixtures, "nostr.bundle.js"))) throw new Error("Missing nostr.bundle.js. Run tests/fetch-fixtures.sh first.");
  server = http.createServer((request, response) => {
    const files = {
      "/harness.html": [path.join(__dirname, "harness.html"), "text/html"],
      "/nostr/store.js": [path.join(workspace, "prototypes/nostr/store.js"), "text/javascript"],
      "/nostr/account.js": [path.join(workspace, "prototypes/nostr/account.js"), "text/javascript"]
    };
    const entry = files[new URL(request.url, "http://localhost").pathname];
    if (!entry) { response.writeHead(404); response.end(); return; }
    response.writeHead(200, { "content-type": entry[1] });
    response.end(fs.readFileSync(entry[0]));
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  baseUrl = `http://127.0.0.1:${server.address().port}/harness.html`;
});

test.afterAll(async () => {
  if (server) await new Promise((resolve) => server.close(resolve));
});

test.beforeEach(async () => { relay = await startFakeRelay(); });
test.afterEach(async () => { await relay.close(); });

// Opens the harness as one "device" (a browser context). Public relays are never contacted.
async function device(browser, { bundle = true, width = 1280, extension = false } = {}) {
  const context = await browser.newContext({ viewport: { width, height: 900 } });
  const failures = [];
  await context.route(BUNDLE_URL, (route) => (bundle ? route.fulfill({ contentType: "text/javascript", path: path.join(fixtures, "nostr.bundle.js") }) : route.abort()));
  await context.addInitScript(({ relayUrl, skHex, extension }) => {
    window.NOSTR_PROTOTYPES_CONFIG = { defaultRelays: [relayUrl], retryBaseMs: 300, retryMaxMs: 1000, queryWaitMs: 2000, publishTimeoutMs: 2000, publishGapMs: 5 };
    if (extension) {
      // A NIP-07 extension stand-in that signs with the test key through the page's own nostr-tools.
      const sk = () => window.NostrTools.utils.hexToBytes(skHex);
      const ck = () => window.NostrTools.nip44.v2.utils.getConversationKey(sk(), window.NostrTools.getPublicKey(sk()));
      window.nostr = {
        getPublicKey: async () => window.NostrTools.getPublicKey(sk()),
        signEvent: async (template) => window.NostrTools.finalizeEvent(template, sk()),
        nip44: { encrypt: async (_pk, text) => window.NostrTools.nip44.v2.encrypt(text, ck()), decrypt: async (_pk, text) => window.NostrTools.nip44.v2.decrypt(text, ck()) }
      };
    }
  }, { relayUrl: relay.url, skHex: Buffer.from(SK).toString("hex"), extension });
  const page = await context.newPage();
  page.on("pageerror", (error) => failures.push(`pageerror: ${error.message}`));
  page.on("console", (message) => {
    if (message.type() === "error" && !(!bundle && /Failed to load resource/.test(message.text()))) failures.push(`console: ${message.text()}`);
  });
  await page.goto(baseUrl);
  await page.waitForFunction(() => window.harness && window.harness.ready);
  return { context, page, failures };
}

const call = (page, fn, ...args) => page.evaluate(([source, a]) => new Function(`return (${source})`)()(window.harness.store, ...a), [fn.toString(), args]);
const notes = (page) => call(page, (s) => s.list("notes"));
const events = (page) => call(page, (s) => s.list("events"));
const status = (page) => call(page, (s) => s.status);
const badge = (page) => page.locator("nostr-account .badge");

async function signInWithNsec(page, nsec = NSEC) {
  await badge(page).click();
  await page.locator("nostr-account #na-secret").fill(nsec);
  await page.locator("nostr-account form button[type=submit]").first().click();
  await expect(page.locator("nostr-account code.npub")).toHaveText(NostrTools.nip19.npubEncode(PK));
  await page.locator("nostr-account").getByRole("button", { name: "Close" }).click();
}

async function waitSynced(page) {
  await expect.poll(async () => { const s = await status(page); return `${s.state}/${s.pending}`; }, { timeout: 10000 }).toBe("synced/0");
}

const decrypt = (event) => JSON.parse(NostrTools.nip44.v2.decrypt(event.content, conversationKey));

test("validateSpec explains a bad storage block", async ({ browser }) => {
  const { page, context, failures } = await device(browser);
  const errors = await page.evaluate(() => window.NostrPrototypes.validateSpec({ appId: "Bad App", collections: { x: { type: "table", fields: {} }, y: { type: "log", fields: { a: "date" } } } }));
  expect(errors).toEqual([
    "appId must be lowercase letters, digits and hyphens",
    "collection 'x': type must be record or log",
    "collection 'y': field 'a' has unknown type 'date'"
  ]);
  const rejected = await page.evaluate(() => window.NostrPrototypes.openStore({ appId: "x" }).then(() => "opened", (e) => e.message));
  expect(rejected).toContain("Invalid storage block");
  const badWrite = await call(page, (s) => s.put("notes", { id: "n1", text: 5 }).then(() => "ok", (e) => e.message));
  expect(badWrite).toBe("notes: field 'text' must be string");
  expect(failures).toEqual([]);
  await context.close();
});

test("without a key, data stays local and survives a reload", async ({ browser }) => {
  const { page, context, failures } = await device(browser);
  await expect(badge(page)).toHaveText("Sign in to sync");
  await call(page, (s) => s.put("notes", { id: "n1", text: "hello" }));
  await call(page, (s) => s.append("events", { id: "e1", n: 1 }));
  expect(await call(page, (s) => s.append("events", { id: "e1", n: 99 }))).toBe(false); // a log never rewrites an id
  await page.reload();
  await page.waitForFunction(() => window.harness && window.harness.ready);
  expect(await notes(page)).toEqual([{ id: "n1", text: "hello" }]);
  expect(await events(page)).toEqual([{ id: "e1", n: 1 }]);
  expect(relay.events).toHaveLength(0);
  expect(failures).toEqual([]);
  await context.close();
});

test("sign-in uploads anonymous data encrypted, except local-only ids", async ({ browser }) => {
  const { page, context, failures } = await device(browser);
  await call(page, (s) => s.put("notes", { id: "n1", text: "secret plan" }));
  await call(page, (s) => s.append("events", { id: "e1", n: 1 }));
  await call(page, (s) => s.append("events", { id: "sample-1", n: 0 }));
  await signInWithNsec(page);
  await waitSynced(page);
  await expect(badge(page)).toContainText("Synced");

  const uploaded = relay.events.filter((e) => e.kind === 30078);
  expect(uploaded.map((e) => e.tags.find((t) => t[0] === "d")[1]).sort()).toEqual(["harness/events/e1", "harness/notes/n1"]);
  for (const event of uploaded) {
    expect(event.pubkey).toBe(PK);
    expect(event.content).not.toContain("secret");
    expect(event.tags).toContainEqual(["L", "signalpillar.prototypes"]);
  }
  const note = uploaded.find((e) => e.tags.some((t) => t[1] === "harness/notes"));
  expect(decrypt(note)).toMatchObject({ v: 1, value: { id: "n1", text: "secret plan" }, deleted: false });
  // The local-only sample stays on this device under the key.
  expect((await events(page)).map((e) => e.id)).toEqual(["e1", "sample-1"]);
  expect(failures).toEqual([]);
  await context.close();
});

test("a second device pulls the data and sees live changes; records follow the last write", async ({ browser }) => {
  const a = await device(browser);
  await call(a.page, (s) => s.put("notes", { id: "n1", text: "v1" }));
  await signInWithNsec(a.page);
  await waitSynced(a.page);

  const b = await device(browser);
  await signInWithNsec(b.page);
  await expect.poll(() => notes(b.page)).toEqual([{ id: "n1", text: "v1" }]);
  await waitSynced(b.page);

  // Live: a log entry and a record change on A reach B without a reload.
  await call(a.page, (s) => s.append("events", { id: "e2", n: 2 }));
  await call(a.page, (s) => s.put("notes", { id: "n1", text: "v2" }));
  await expect.poll(() => events(b.page)).toEqual([{ id: "e2", n: 2 }]);
  await expect.poll(() => notes(b.page)).toEqual([{ id: "n1", text: "v2" }]);

  // B removes the record; A sees the removal, and the relay holds one event for the id.
  await call(b.page, (s) => s.remove("notes", "n1"));
  await expect.poll(() => notes(a.page)).toEqual([]);
  const slots = relay.events.filter((e) => e.tags.some((t) => t[0] === "d" && t[1] === "harness/notes/n1"));
  expect(slots).toHaveLength(1);
  expect(decrypt(slots[0]).deleted).toBe(true);
  expect(b.failures).toEqual([]);
  expect(a.failures).toEqual([]);
  await a.context.close();
  await b.context.close();
});

test("sign-out shows the anonymous copy; sign-in again brings the key's copy back", async ({ browser }) => {
  const { page, context, failures } = await device(browser);
  await signInWithNsec(page);
  await call(page, (s) => s.put("notes", { id: "n1", text: "mine" }));
  await waitSynced(page);
  await badge(page).click();
  await page.locator("nostr-account").getByRole("button", { name: "Sign out" }).click();
  await expect(badge(page)).toHaveText("Sign in to sync");
  await expect.poll(() => notes(page)).toEqual([]); // the store switches owner asynchronously
  await call(page, (s) => s.put("notes", { id: "n2", text: "anonymous" }));
  await signInWithNsec(page);
  await expect.poll(async () => (await notes(page)).map((n) => n.id).sort()).toEqual(["n1", "n2"]);
  expect(failures).toEqual([]);
  await context.close();
});

test("create a new key shows the backup with a warning", async ({ browser }) => {
  const { page, context, failures } = await device(browser, { width: 320 });
  await badge(page).click();
  await page.locator("nostr-account").getByRole("button", { name: "Create a new key" }).click();
  const drawer = page.locator("nostr-account dialog");
  await expect(drawer).toContainText("Save this new key now.");
  await expect(drawer.locator("code.nsec")).toHaveText(/^nsec1/);
  await expect(drawer.locator("code.npub")).toHaveText(/^npub1/);
  expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
  const box = await drawer.boundingBox();
  expect(box.width).toBeLessThanOrEqual(320);
  expect(failures).toEqual([]);
  await context.close();
});

test("a relay edit publishes the NIP-65 list, and another device follows it", async ({ browser }) => {
  const second = await startFakeRelay();
  try {
    const a = await device(browser);
    await signInWithNsec(a.page);
    await waitSynced(a.page);
    await badge(a.page).click();
    await a.page.locator("nostr-account #na-relay").fill(second.url);
    await a.page.locator("nostr-account").getByRole("button", { name: "Add", exact: true }).click();
    await expect(a.page.locator("nostr-account .relays")).toContainText("From your NIP-65 relay list");
    const list = relay.events.find((e) => e.kind === 10002);
    expect(list.tags).toEqual([["r", relay.url], ["r", second.url]]);
    await a.page.locator("nostr-account").getByRole("button", { name: "Close" }).click();
    await call(a.page, (s) => s.put("notes", { id: "n1", text: "both relays" }));
    await waitSynced(a.page);
    await expect.poll(() => second.events.filter((e) => e.kind === 30078).length).toBe(1);

    const b = await device(browser);
    await signInWithNsec(b.page);
    await expect.poll(() => b.page.evaluate(() => window.NostrPrototypes.account.relays())).toEqual({ list: [relay.url, second.url], source: "nip65" });
    expect(a.failures).toEqual([]);
    expect(b.failures).toEqual([]);
    await a.context.close();
    await b.context.close();
  } finally {
    await second.close();
  }
});

test("offline writes wait in the outbox and upload when the relay returns", async ({ browser }) => {
  const { page, context, failures } = await device(browser);
  await signInWithNsec(page);
  await waitSynced(page);
  relay.goDown();
  await call(page, (s) => s.append("events", { id: "e1", n: 1 }));
  await expect.poll(async () => (await status(page)).state, { timeout: 10000 }).toBe("offline");
  expect((await status(page)).pending).toBe(1);
  await expect(badge(page)).toContainText("Offline");
  relay.comeUp();
  await waitSynced(page);
  expect(relay.eventsFor("harness/events")).toHaveLength(1);
  expect(failures.filter((f) => !/WebSocket/.test(f))).toEqual([]);
  await context.close();
});

test("a NIP-07 extension signs and encrypts instead of a stored key", async ({ browser }) => {
  const { page, context, failures } = await device(browser, { extension: true });
  await badge(page).click();
  await page.locator("nostr-account").getByRole("button", { name: "Use browser extension" }).click();
  await expect(page.locator("nostr-account dialog")).toContainText("Key in your browser extension");
  expect(await page.evaluate(() => localStorage.getItem("nostr-prototypes.account.v1"))).not.toContain("nsec");
  await call(page, (s) => s.put("notes", { id: "n1", text: "via extension" }));
  await waitSynced(page);
  expect(decrypt(relay.eventsFor("harness/notes")[0]).value.text).toBe("via extension");
  expect(failures).toEqual([]);
  await context.close();
});

test("the first pull pages past the relay limit of 500 events", async ({ browser }) => {
  const now = Math.floor(Date.now() / 1000);
  const injected = Array.from({ length: 520 }, (_, i) => NostrTools.finalizeEvent({
    kind: 30078,
    created_at: now - 1000 + Math.floor(i / 3),
    tags: [["d", `harness/events/x${i}`], ["L", "signalpillar.prototypes"], ["l", "harness/events", "signalpillar.prototypes"]],
    content: NostrTools.nip44.v2.encrypt(JSON.stringify({ v: 1, value: { id: `x${i}`, n: i }, updatedAt: (now - 1000 + i) * 1000, deleted: false }), conversationKey)
  }, SK));
  relay.inject(injected);
  const { page, context, failures } = await device(browser);
  await signInWithNsec(page);
  await expect.poll(async () => (await events(page)).length, { timeout: 15000 }).toBe(520);
  expect(failures).toEqual([]);
  await context.close();
});

test("without the nostr-tools bundle the store still works locally", async ({ browser }) => {
  const { page, context, failures } = await device(browser, { bundle: false, width: 390 });
  await expect(badge(page)).toHaveText("Local only");
  await call(page, (s) => s.put("notes", { id: "n1", text: "offline" }));
  expect(await notes(page)).toEqual([{ id: "n1", text: "offline" }]);
  await badge(page).click();
  await expect(page.locator("nostr-account dialog")).toContainText("did not load");
  expect(failures).toEqual([]);
  await context.close();
});
