const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const workspace = path.resolve(__dirname, "../../..");
const htmlPath = path.join(workspace, "prototypes/fact-graph.html");
const baseCssPath = path.join(workspace, "prototypes/prototype-base.css");
const fixtures = path.join(__dirname, "fixtures");
// The shared Nostr test kit: fake relay and the nostr-tools fixture (npm install + fetch-fixtures in prototype.tests/nostr).
const nostrKit = path.resolve(__dirname, "../../nostr/tests");
const { startFakeRelay, NostrTools } = require(path.join(nostrKit, "fake-relay.js"));
const html = fs.readFileSync(htmlPath, "utf8");
const manifest = JSON.parse(html.match(/<script type="application\/json" id="fact-graph-manifest">\s*([\s\S]*?)\s*<\/script>/)[1]);
const C = manifest.copy;
const fill = (tpl, vars) => tpl.replace(/\{(\w+)\}/g, (all, key) => (key in vars ? String(vars[key]) : all));
const viewports = [320, 390, 768, 1360];
const cdn = {
  "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css": ["pico.min.css", "text/css", "pico"],
  "https://cdn.jsdelivr.net/npm/datascript@1.8.1/datascript.js": ["datascript.js", "text/javascript", "datascript"],
  "https://cdn.jsdelivr.net/npm/cytoscape@3.30.4/dist/cytoscape.min.js": ["cytoscape.min.js", "text/javascript", "cytoscape"],
  "https://cdn.jsdelivr.net/npm/layout-base@2.0.1/layout-base.js": ["layout-base.js", "text/javascript", "cytoscape"],
  "https://cdn.jsdelivr.net/npm/cose-base@2.2.0/cose-base.js": ["cose-base.js", "text/javascript", "cytoscape"],
  "https://cdn.jsdelivr.net/npm/cytoscape-fcose@2.2.0/cytoscape-fcose.js": ["cytoscape-fcose.js", "text/javascript", "cytoscape"],
  "https://cdn.jsdelivr.net/npm/nostr-tools@2.25.2/lib/nostr.bundle.js": ["nostr.bundle.js", "text/javascript", "nostr", path.join(nostrKit, "fixtures")]
};
const chromeCandidates = [process.env.CHROME_PATH, "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"];
const chromePath = chromeCandidates.find((candidate) => candidate && fs.existsSync(candidate));
if (chromePath) test.use({ launchOptions: { executablePath: chromePath } });

let server;
let baseUrl;

test.beforeAll(async () => {
  for (const [name, , , dir] of Object.values(cdn)) {
    if (!fs.existsSync(path.join(dir || fixtures, name))) throw new Error(`Missing ${name}. Run tests/fetch-fixtures.sh here and in prototype.tests/nostr.`);
  }
  server = http.createServer((request, response) => {
    const pathname = new URL(request.url, "http://localhost").pathname;
    if (pathname === "/fact-graph-timeout.html") {
      // Same page with a 1 ms query limit, to exercise the stop path.
      response.writeHead(200, { "content-type": "text/html" });
      response.end(html.replace(`"timeoutMs": ${manifest.query.timeoutMs}`, '"timeoutMs": 1'));
      return;
    }
    const files = {
      "/fact-graph.html": [htmlPath, "text/html"],
      "/prototype-base.css": [baseCssPath, "text/css"],
      "/shell.js": [path.join(workspace, "prototypes/shell.js"), "text/javascript"],
      "/nostr/store.js": [path.join(workspace, "prototypes/nostr/store.js"), "text/javascript"],
      "/nostr/account.js": [path.join(workspace, "prototypes/nostr/account.js"), "text/javascript"]
    };
    const entry = files[pathname];
    if (!entry) { response.writeHead(404); response.end(); return; }
    response.writeHead(200, { "content-type": entry[1] });
    response.end(fs.readFileSync(entry[0]));
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  baseUrl = `http://127.0.0.1:${server.address().port}/fact-graph.html`;
});

test.afterAll(async () => {
  if (server) await new Promise((resolve) => server.close(resolve));
});

// blocked: a set of dependency groups ("pico", "datascript", "cytoscape", "nostr") to abort.
// aborted: dependency groups whose load the page may cancel on purpose (a stopped query worker).
// relayUrl: points the shared Nostr store at a fake relay; without it the page never signs in.
async function open(page, { blocked = [], aborted = [], width = 1360, file = "fact-graph.html", relayUrl = null } = {}) {
  const tolerated = [...blocked, ...aborted];
  const failures = [];
  page.on("pageerror", (error) => failures.push(`pageerror: ${error.message}`));
  page.on("console", (message) => {
    if (message.type() !== "error") return;
    if (tolerated.length && /Failed to load resource/.test(message.text())) return;
    failures.push(`console: ${message.text()}`);
  });
  page.on("requestfailed", (request) => {
    const dep = cdn[request.url()];
    if (dep && tolerated.includes(dep[2])) return;
    failures.push(`requestfailed: ${request.url()}`);
  });
  // Context-level routing also covers importScripts() inside the query worker.
  await page.context().route("https://cdn.jsdelivr.net/**", (route) => {
    const dep = cdn[route.request().url()];
    if (!dep || blocked.includes(dep[2])) return route.abort();
    return route.fulfill({ contentType: dep[1], path: path.join(dep[3] || fixtures, dep[0]) });
  });
  if (relayUrl) {
    await page.context().addInitScript((url) => {
      window.NOSTR_PROTOTYPES_CONFIG = { defaultRelays: [url], retryBaseMs: 300, retryMaxMs: 1000, queryWaitMs: 2000, publishTimeoutMs: 2000, publishGapMs: 5 };
    }, relayUrl);
  }
  await page.setViewportSize({ width, height: 900 });
  await page.goto(baseUrl.replace("fact-graph.html", file));
  await expect(page.locator("#stats")).not.toBeEmpty();
  return failures;
}

async function settle(page) {
  // Wait until no node moves between two samples (fcose animates).
  await expect.poll(async () => {
    const sample = () => page.evaluate(() => JSON.stringify(window.factGraph.cy.nodes().map((n) => [n.id(), Math.round(n.renderedPosition("x")), Math.round(n.renderedPosition("y"))])));
    const a = await sample();
    await page.waitForTimeout(150);
    return a === (await sample());
  }, { timeout: 10000 }).toBe(true);
}

async function tapNode(page, slug) {
  await settle(page);
  const point = await page.evaluate((id) => {
    const cy = window.factGraph.cy;
    const box = cy.container().getBoundingClientRect();
    const p = cy.$id(id).renderedPosition();
    return { x: box.left + p.x, y: box.top + p.y };
  }, slug);
  await page.mouse.click(point.x, point.y);
}

const classesOf = (page, id) => page.evaluate((x) => window.factGraph.cy.$id(x).classes(), id);
const nodeIds = (page) => page.evaluate(() => window.factGraph.cy.nodes().map((n) => n.id()).sort());

test("seed renders the news graph with merged nodes", async ({ page }) => {
  const failures = await open(page);
  await expect(page.getByRole("tab", { name: /^news/ })).toHaveAttribute("aria-selected", "true");
  expect(await nodeIds(page)).toEqual(["air-defence-aid", "ews", "ukraine", "usa.gov"]);
  // 'early warning system (EWS)' and 'EWS | Early warning system' are one node; the later explicit title wins.
  expect(await page.evaluate(() => window.factGraph.cy.$id("ews").data("label"))).toBe("Early warning system");
  await expect(page.locator("#stats")).toHaveText(fill(C.statsTemplate, { graph: "news", nodes: 4, edges: 4, entries: manifest.seed.length }));
  expect(failures).toEqual([]);
});

test("preview flags merges and new nodes, and Add merges into the graph", async ({ page }) => {
  const failures = await open(page);
  await page.locator("#entry").fill("EWS -> monitored by -> NORAD\nbroken line\nUkraine -> depends on -> EWS\nNORAD -> audits -> NORAD");
  const items = page.locator("#preview li");
  await expect(items).toHaveCount(4);
  await expect(items.nth(0)).toContainText(C.chipKnownNode);
  await expect(items.nth(0)).toContainText(C.chipNewNode);
  await expect(items.nth(0)).toContainText(C.chipNewLabel);
  await expect(items.nth(1)).toContainText(fill(C.lineError, { line: 2, message: fill(C.errArrows, { count: 0 }) }));
  await expect(items.nth(2)).toContainText(C.chipDuplicate);
  await expect(page.locator("#add")).toHaveText(fill(C.addButtonCount, { count: 2 }));
  await page.locator("#add").click();
  // The self-loop on a new node creates one entity, not two.
  expect(await nodeIds(page)).toEqual(["air-defence-aid", "ews", "norad", "ukraine", "usa.gov"]);
  expect(await page.evaluate(() => window.factGraph.cy.$id("norad").connectedEdges().length)).toBe(2);
  // The bad line stays in the field so it can be fixed in place.
  await expect(page.locator("#entry")).toHaveValue("broken line");
  await page.reload();
  expect(await nodeIds(page)).toContain("norad");
  expect(failures).toEqual([]);
});

test("tapping a node highlights upstream and downstream, and Also in switches graph", async ({ page }) => {
  const failures = await open(page);
  await tapNode(page, "ews");
  expect(await classesOf(page, "ews")).toContain("focus");
  expect(await classesOf(page, "ukraine")).toContain("up");
  expect(await classesOf(page, "usa.gov")).toContain("down");
  expect(await classesOf(page, "air-defence-aid")).toContain("faded");
  const detail = page.locator("#detail");
  await expect(detail).toBeVisible();
  await expect(detail.getByRole("heading", { level: 2 })).toHaveText("Early warning system");
  await expect(detail).toContainText(fill(C.detailDown, { count: 1 }));
  await expect(detail).toContainText(fill(C.detailUp, { count: 1 }));
  await detail.getByRole("button", { name: fill(C.detailAlsoInItem, { graph: "professional", edges: 1 }) }).click();
  await expect(page.getByRole("tab", { name: /^professional/ })).toHaveAttribute("aria-selected", "true");
  expect(await classesOf(page, "ews")).toContain("focus");
  expect(await classesOf(page, "usa.gov")).toContain("down");
  await expect(detail).toContainText(fill(C.detailAlsoInItem, { graph: "news", edges: 2 }));
  // Tapping the empty canvas clears the selection.
  const box = await page.locator("#canvas").boundingBox();
  await page.mouse.click(box.x + 5, box.y + 5);
  await expect(detail).toBeHidden();
  expect(failures).toEqual([]);
});

test("graph prefix creates a graph, retraction removes the edge, and replay rewinds", async ({ page }) => {
  const failures = await open(page);
  await page.locator("#entry").fill("#research EWS -> studied by -> RAND @2026-09-30");
  await expect(page.locator("#preview li").first()).toContainText(C.chipNewGraph);
  await page.locator("#entry").press("Control+Enter");
  await expect(page.getByRole("tab", { name: /^research/ })).toHaveAttribute("aria-selected", "true");
  expect(await nodeIds(page)).toEqual(["ews", "rand"]);

  await page.locator("#log summary").click();
  const row = page.locator("#log-list li", { hasText: "studied by" });
  await row.getByRole("button", { name: C.retract }).click();
  const factRow = page.locator("#log-list li.retracted", { hasText: "studied by" });
  await expect(factRow).toContainText(C.logRetractedTag);
  await expect(page.locator("#log-list")).not.toContainText("null");
  await expect(page.locator("#log-list li", { hasText: fill(C.logRetraction, { text: "" }).trim() })).toHaveCount(1);
  expect(await nodeIds(page)).toEqual([]);
  await expect(page.locator("#canvas-empty")).toBeVisible();

  await page.getByRole("tab", { name: /^news/ }).click();
  const slider = page.locator("#time");
  const max = Number(await slider.getAttribute("max"));
  expect(max).toBe(manifest.seed.length + 2);
  await slider.fill("3");
  await expect(page.locator("#time-label")).toContainText(fill(C.timeAsOf, { n: 3, count: max, at: "" }).trim());
  expect(await nodeIds(page)).toEqual(["ews", "ukraine"]);
  await slider.fill(String(max));
  await expect(page.locator("#time-label")).toHaveText(fill(C.timeLive, { count: max }));
  expect(await nodeIds(page)).toHaveLength(4);
  expect(failures).toEqual([]);
});

test("export, clear and import round-trip the log", async ({ page }) => {
  const failures = await open(page);
  await page.locator("#log summary").click();
  const download = page.waitForEvent("download");
  await page.locator("#export").click();
  const file = await (await download).path();
  const exported = JSON.parse(fs.readFileSync(file, "utf8"));
  expect(exported.format).toBe(manifest.exportFile.format);
  expect(exported.entries).toHaveLength(manifest.seed.length);

  await page.locator("#clear").click();
  await expect(page.locator("#clear")).toHaveText(C.clearConfirm);
  await page.locator("#clear").click();
  await expect(page.locator("#log-title")).toHaveText(fill(C.logTitle, { count: 0 }));
  await expect(page.locator("#load-sample")).toBeVisible();

  await page.locator("#import-file").setInputFiles(file);
  await expect(page.locator("#status")).toHaveText(fill(C.imported, { added: manifest.seed.length, skipped: 0 }));
  expect(await nodeIds(page)).toHaveLength(4);

  await page.locator("#import-file").setInputFiles({ name: "bad.json", mimeType: "application/json", buffer: Buffer.from('{"format":"fact-graph-log","entries":[{"kind":"fact"}]}') });
  await expect(page.locator("#status")).toHaveText(fill(C.importRejected, { reason: fill(C.importMissingIdentity, { index: 1 }) }));
  expect(failures).toEqual([]);
});

for (const width of viewports) {
  test(`no horizontal page scroll at ${width}px, detail reachable`, async ({ page }) => {
    const failures = await open(page, { width });
    const overflow = () => page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
    expect(await overflow()).toBe(0);
    await tapNode(page, "ukraine");
    const detail = page.locator("#detail");
    await expect(detail).toBeInViewport();
    expect(await overflow()).toBe(0);
    await expect(page.locator("#add")).toBeVisible();
    expect(failures).toEqual([]);
  });
}

test("graph library blocked: edges show as a list and detail still works", async ({ page }) => {
  const failures = await open(page, { blocked: ["cytoscape"], width: 390 });
  await expect(page.locator("#status")).toHaveText(C.cyMissing);
  const list = page.locator(".canvas-list li");
  await expect(list).toHaveCount(4);
  await list.first().getByRole("button").first().click();
  await expect(page.locator("#detail")).toBeVisible();
  await expect(page.locator("#detail")).toContainText(C.detailAlsoIn);
  // On a phone, focusing the entry closes the sheet so it never covers the composer.
  await page.locator("#entry").click();
  await expect(page.locator("#detail")).toBeHidden();
  await page.locator("#entry").fill("EWS -> monitored by -> NORAD");
  await page.locator("#add").click();
  await expect(list).toHaveCount(5);
  expect(failures).toEqual([]);
});

test("DataScript and Pico blocked: the page explains itself and keeps the log", async ({ page }) => {
  const failures = await open(page, { blocked: ["datascript", "pico"], width: 390 });
  await expect(page.locator("#status")).toHaveText(C.dsMissing);
  await expect(page.locator("#add")).toBeDisabled();
  await page.locator("#log summary").click();
  await expect(page.locator("#log-list li")).toHaveCount(manifest.seed.length);
  expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
  expect(failures).toEqual([]);
});

const examples = manifest.query.examples.flatMap((group) => group.items);

async function runQuery(page, text) {
  if (!(await page.locator("#query").evaluate((d) => d.open))) await page.locator("#query > summary").click();
  await page.locator("#query-input").fill(text);
  await page.locator("#query-run").click();
  await expect(page.locator("#query-status")).not.toHaveText(C.queryRunning);
  return page.locator("#query-status").textContent();
}

test("plain text search lists nodes, rings them and jumps to another graph", async ({ page }) => {
  const failures = await open(page);
  const status = await runQuery(page, "GOV");
  expect(status).toContain(fill(C.queryTextMode, { term: "GOV" }));
  const cells = page.locator("#query-results .node-cell");
  await expect(cells).toHaveCount(1);
  await expect(cells.first()).toHaveText("USA.gov");
  expect(await classesOf(page, "usa.gov")).toContain("match");
  expect(await classesOf(page, "ews")).not.toContain("match");

  // 'sensors' lives only in 'professional'; tapping it switches graph and selects it.
  await runQuery(page, "sensors");
  await page.locator("#query-results .node-cell", { hasText: "satellite sensors" }).click();
  await expect(page.getByRole("tab", { name: /^professional/ })).toHaveAttribute("aria-selected", "true");
  expect(await classesOf(page, "satellite-sensors")).toContain("focus");
  expect(failures).toEqual([]);
});

test("every example query runs without an error", async ({ page }) => {
  const failures = await open(page);
  await tapNode(page, "ews"); // the ?node examples need a selected node
  await page.locator("#query > summary").click();
  for (const example of examples) {
    await page.locator("#query-examples button", { hasText: example.title }).click();
    await expect(page.locator("#query-input")).toHaveValue(example.query.join("\n"));
    await expect(page.locator("#query-status")).not.toHaveText(C.queryRunning);
    const status = await page.locator("#query-status").textContent();
    expect(status, example.title).toMatch(/row\(s\)/);
    expect(await page.locator("#query-results tbody tr").count(), example.title).toBeGreaterThan(0);
  }
  expect(failures).toEqual([]);
});

test("reaches terminates on a cycle and results follow new facts", async ({ page }) => {
  const failures = await open(page);
  await page.locator("#entry").fill("USA.gov -> supports -> Ukraine");
  await page.locator("#add").click();
  await tapNode(page, "ukraine");
  await runQuery(page, '[:find ?slug :in $ % ?node :where (reaches-slug ?node ?slug)]');
  const slugs = await page.locator("#query-results .node-cell").evaluateAll((els) => els.map((e) => e.dataset.slug).sort());
  expect(slugs).toEqual(["air-defence-aid", "ews", "satellite-sensors", "ukraine", "usa.gov"]);

  await runQuery(page, "norad");
  await expect(page.locator("#query-results")).toHaveText(C.queryEmpty);
  await page.locator("#entry").fill("EWS -> monitored by -> NORAD");
  await page.locator("#add").click();
  await expect(page.locator("#query-results .node-cell")).toHaveText(["NORAD"]);
  expect(failures).toEqual([]);
});

test("query errors explain themselves", async ({ page }) => {
  const failures = await open(page);
  expect(await runQuery(page, '[:find ?s :in $ ?node :where [?n "node/slug" ?s]]')).toBe(C.queryNeedsNode);
  expect(await runQuery(page, '[:find ?s :in $ ?who :where [?n "node/slug" ?s]]')).toBe(fill(C.queryUnknownInput, { name: "?who" }));
  expect(await runQuery(page, '[:find ?s :where [?n "node/slug" ?s]')).toContain(fill(C.queryError, { message: "" }).trim());
  await expect(page.locator("#query-results")).toBeEmpty();
  await page.locator("#query-clear").click();
  await expect(page.locator("#query-input")).toHaveValue("");
  expect(failures).toEqual([]);
});

test("a query over the time limit is stopped", async ({ page }) => {
  // At 1 ms the worker is stopped while it still loads DataScript, so that request is cancelled.
  const failures = await open(page, { file: "fact-graph-timeout.html", aborted: ["datascript"] });
  expect(await runQuery(page, "[:find ?a ?b :where [?a _ _] [?b _ _]]")).toBe(fill(C.queryTimeout, { seconds: 0.001 }));
  expect(failures).toEqual([]);
});

// ---- Nostr sync (shared store in prototypes/nostr) ----------------------------------------

const SK = NostrTools.generateSecretKey();
const PK = NostrTools.getPublicKey(SK);
const NSEC = NostrTools.nip19.nsecEncode(SK);
const conversationKey = NostrTools.nip44.v2.utils.getConversationKey(SK, PK);
const decrypt = (event) => JSON.parse(NostrTools.nip44.v2.decrypt(event.content, conversationKey));

async function signIn(page) {
  await page.locator("nostr-account .badge").click();
  await page.locator("nostr-account #na-secret").fill(NSEC);
  await page.locator("nostr-account form button[type=submit]").first().click();
  await expect(page.locator("nostr-account code.npub")).toHaveText(NostrTools.nip19.npubEncode(PK));
  await page.locator("nostr-account").getByRole("button", { name: "Close" }).click();
}

const syncState = (page) => page.evaluate(() => `${window.factGraph.store.status.state}/${window.factGraph.store.status.pending}`);

test.describe("Nostr sync", () => {
  let relay;
  test.beforeEach(async () => { relay = await startFakeRelay(); });
  test.afterEach(async () => { await relay.close(); });

  test("facts sync between two devices; sample data stays local; retractions follow", async ({ browser }) => {
    const ctxA = await browser.newContext();
    const a = await ctxA.newPage();
    const failuresA = await open(a, { relayUrl: relay.url });
    await expect(a.locator("nostr-account .badge")).toHaveText("Sign in to sync");
    await signIn(a);
    await expect.poll(() => syncState(a), { timeout: 10000 }).toBe("synced/0");
    expect(relay.eventsFor("fact-graph/log")).toHaveLength(0); // seed-* ids never upload

    await a.locator("#entry").fill("EWS -> monitored by -> NORAD");
    await a.locator("#add").click();
    await expect.poll(() => relay.eventsFor("fact-graph/log").length).toBe(1);
    const uploaded = decrypt(relay.eventsFor("fact-graph/log")[0]).value;
    expect(uploaded).toMatchObject({ kind: "fact", label: "monitored by", from: { slug: "ews" }, to: { slug: "norad" } });

    const ctxB = await browser.newContext();
    const b = await ctxB.newPage();
    const failuresB = await open(b, { relayUrl: relay.url });
    await signIn(b);
    await expect.poll(() => nodeIds(b)).toContain("norad");

    // B retracts the synced fact; A drops it live, without a reload.
    await b.locator("#log summary").click();
    await b.locator("#log-list li", { hasText: "monitored by" }).getByRole("button", { name: C.retract }).click();
    await expect.poll(() => nodeIds(a), { timeout: 10000 }).not.toContain("norad");
    expect(failuresA).toEqual([]);
    expect(failuresB).toEqual([]);
    await ctxA.close();
    await ctxB.close();
  });

  test("a log from the localStorage version moves into the store once", async ({ page }) => {
    const legacy = [{ id: "old-1", kind: "fact", at: "2026-10-05T10:00:00Z", graph: "news", graphTitle: "news", from: { slug: "nato", title: null, name: "NATO" }, to: { slug: "ukraine", title: null, name: "Ukraine" }, label: "supports", date: null, text: "NATO -> supports -> Ukraine" }];
    await page.context().addInitScript((log) => { if (!sessionStorage.getItem("seeded-test")) { localStorage.setItem("fact-graph.log.v1", JSON.stringify(log)); sessionStorage.setItem("seeded-test", "1"); } }, legacy);
    const failures = await open(page);
    expect(await nodeIds(page)).toEqual(["nato", "ukraine"]);
    expect(await page.evaluate(() => localStorage.getItem("fact-graph.log.v1"))).toBeNull();
    await page.reload();
    await expect(page.locator("#stats")).not.toBeEmpty();
    expect(await nodeIds(page)).toEqual(["nato", "ukraine"]);
    expect(failures).toEqual([]);
  });

  test("Clear all while signed in retracts the facts instead of deleting the synced log", async ({ page }) => {
    const failures = await open(page, { relayUrl: relay.url });
    await signIn(page);
    await page.locator("#entry").fill("EWS -> monitored by -> NORAD");
    await page.locator("#add").click();
    await page.locator("#log summary").click();
    await page.locator("#clear").click();
    await page.locator("#clear").click();
    await expect(page.locator("#status")).toHaveText(fill(C.retractedAll, { count: manifest.seed.filter((e) => e.kind === "fact").length + 1 }));
    expect(await nodeIds(page)).toEqual([]);
    await expect.poll(() => syncState(page), { timeout: 10000 }).toBe("synced/0");
    const kinds = relay.eventsFor("fact-graph/log").map((e) => decrypt(e).value.kind).sort();
    expect(kinds.filter((k) => k === "retract")).toHaveLength(manifest.seed.filter((e) => e.kind === "fact").length + 1);
    expect(failures).toEqual([]);
  });
});
