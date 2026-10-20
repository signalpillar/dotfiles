const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const workspace = path.resolve(__dirname, "../../..");
const htmlPath = path.join(workspace, "prototypes/fact-graph.html");
const baseCssPath = path.join(workspace, "prototypes/prototype-base.css");
const fixtures = path.join(__dirname, "fixtures");
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
  "https://cdn.jsdelivr.net/npm/cytoscape-fcose@2.2.0/cytoscape-fcose.js": ["cytoscape-fcose.js", "text/javascript", "cytoscape"]
};
const chromeCandidates = [process.env.CHROME_PATH, "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"];
const chromePath = chromeCandidates.find((candidate) => candidate && fs.existsSync(candidate));
if (chromePath) test.use({ launchOptions: { executablePath: chromePath } });

let server;
let baseUrl;

test.beforeAll(async () => {
  for (const [name] of Object.values(cdn)) {
    if (!fs.existsSync(path.join(fixtures, name))) throw new Error(`Missing ${name}. Run tests/fetch-fixtures.sh first.`);
  }
  server = http.createServer((request, response) => {
    const files = { "/fact-graph.html": [htmlPath, "text/html"], "/prototype-base.css": [baseCssPath, "text/css"] };
    const entry = files[new URL(request.url, "http://localhost").pathname];
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

// blocked: a set of dependency groups ("pico", "datascript", "cytoscape") to abort.
async function open(page, { blocked = [], width = 1360 } = {}) {
  const failures = [];
  page.on("pageerror", (error) => failures.push(`pageerror: ${error.message}`));
  page.on("console", (message) => {
    if (message.type() !== "error") return;
    if (blocked.length && /Failed to load resource/.test(message.text())) return;
    failures.push(`console: ${message.text()}`);
  });
  page.on("requestfailed", (request) => {
    const dep = cdn[request.url()];
    if (dep && blocked.includes(dep[2])) return;
    failures.push(`requestfailed: ${request.url()}`);
  });
  await page.route("https://cdn.jsdelivr.net/**", (route) => {
    const dep = cdn[route.request().url()];
    if (!dep || blocked.includes(dep[2])) return route.abort();
    return route.fulfill({ contentType: dep[1], path: path.join(fixtures, dep[0]) });
  });
  await page.setViewportSize({ width, height: 900 });
  await page.goto(baseUrl);
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
