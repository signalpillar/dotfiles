const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const workspace = path.resolve(__dirname, "../../..");
const htmlPath = path.join(workspace, "prototypes/gcse-science-f1-roadmap.html");
const baseCssPath = path.join(workspace, "prototypes/prototype-base.css");
const jellyCssPath = path.join(workspace, "prototypes/prototype-jelly.css");
const resourceRoot = path.join(workspace, "prototypes/gcse-science-f1-roadmap");
const fixtures = path.join(__dirname, "fixtures");
const storageKey = "gcse-science-f1-roadmap-v1";
const chromePath = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
let server;
let baseUrl;

if (fs.existsSync(chromePath)) test.use({ launchOptions: { executablePath: chromePath } });

test.beforeAll(async () => {
  for (const name of ["jelly.js", "open-props.css", "marked.js"]) {
    if (!fs.existsSync(path.join(fixtures, name))) throw new Error(`Missing ${name}. Run tests/fetch-fixtures.sh first.`);
  }
  server = http.createServer((request, response) => {
    const pathname = new URL(request.url, "http://localhost").pathname;
    if (pathname === "/gcse-science-f1-roadmap.html") {
      response.writeHead(200, { "content-type": "text/html" });
      response.end(fs.readFileSync(htmlPath));
      return;
    }
    if (pathname === "/prototype-base.css") {
      response.writeHead(200, { "content-type": "text/css; charset=utf-8" });
      response.end(fs.readFileSync(baseCssPath));
      return;
    }
    if (pathname === "/prototype-jelly.css") {
      response.writeHead(200, { "content-type": "text/css; charset=utf-8" });
      response.end(fs.readFileSync(jellyCssPath));
      return;
    }
    if (pathname.startsWith("/gcse-science-f1-roadmap/")) {
      let relative;
      try { relative = decodeURIComponent(pathname.slice("/gcse-science-f1-roadmap/".length)); } catch { relative = ""; }
      const file = path.resolve(resourceRoot, relative);
      if (relative && file.startsWith(resourceRoot + path.sep) && fs.existsSync(file) && fs.statSync(file).isFile()) {
        const contentTypes = { ".json": "application/json", ".md": "text/markdown; charset=utf-8" };
        response.writeHead(200, { "content-type": contentTypes[path.extname(file)] || "application/octet-stream" });
        response.end(fs.readFileSync(file));
        return;
      }
    }
    response.writeHead(404);
    response.end();
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  baseUrl = `http://127.0.0.1:${server.address().port}/gcse-science-f1-roadmap.html`;
});

test.afterAll(async () => {
  if (server) await new Promise((resolve) => server.close(resolve));
});

async function routeDependencies(page, available) {
  const options = typeof available === "boolean"
    ? { jelly: available, props: available, marked: available }
    : { jelly: true, props: true, marked: true, ...available };
  if (!options.jelly) {
    await page.route("https://jelly-ui.com/**", (route) => route.abort());
  } else {
    await page.route("https://jelly-ui.com/package.js", (route) => route.fulfill({
      contentType: "text/javascript",
      body: "export * from 'https://jelly-ui.com/dist/jelly.js';"
    }));
    await page.route("https://jelly-ui.com/dist/jelly.js", (route) => route.fulfill({
      contentType: "text/javascript",
      path: path.join(fixtures, "jelly.js")
    }));
  }
  if (!options.props) {
    await page.route("https://unpkg.com/**", (route) => route.abort());
  } else {
    await page.route("https://unpkg.com/open-props", (route) => route.fulfill({
      contentType: "text/css",
      path: path.join(fixtures, "open-props.css")
    }));
  }
  await page.route("https://cdn.jsdelivr.net/npm/marked@15.0.12/marked.min.js", (route) => options.marked
    ? route.fulfill({ contentType: "text/javascript", path: path.join(fixtures, "marked.js") })
    : route.abort());
}

function collectFailures(page, allowRequestFailures = false) {
  const failures = [];
  page.on("pageerror", (error) => failures.push(`pageerror: ${error.message}`));
  page.on("console", (message) => {
    if (message.type() === "error") failures.push(`console: ${message.text()}`);
  });
  if (!allowRequestFailures) page.on("requestfailed", (request) => failures.push(`request: ${request.url()}`));
  return failures;
}

async function waitForRoadmap(page) {
  await page.goto(baseUrl);
  await page.waitForFunction(() => window.__ROADMAP_TEST__ && document.querySelectorAll(".node").length === 121);
}

async function openTopic(page, id) {
  await page.locator(`.node[data-node-id="${id}"]`).click();
  await expect(page.locator(".detail-body h2").first()).toBeVisible();
}

test("normal path exposes exact prerequisite logic and chain highlighting", async ({ page }) => {
  await routeDependencies(page, true);
  const failures = collectFailures(page);
  await page.addInitScript(({ key }) => {
    const met = ["M1", "M2", "M13", "M14", "P16", "C1", "C14", "M4", "M10", "M18", "M20", "C16", "C4"];
    const progress = Object.fromEntries(met.map((id) => [id, { status: "quali", notes: "", updatedAt: "2026-01-01T00:00:00.000Z" }]));
    localStorage.setItem(key, JSON.stringify({ version: 1, progress }));
  }, { key: storageKey });
  await page.goto(baseUrl);
  await page.waitForFunction(() => window.__ROADMAP_TEST__ && document.querySelectorAll(".node").length === 121);

  expect(await page.evaluate(() => window.__ROADMAP_TEST__.readiness("M1"))).toEqual({ ready: 0, total: 0, readyToStart: true, unmet: [] });
  expect(await page.evaluate(() => window.__ROADMAP_TEST__.readiness("C17"))).toEqual({ ready: 2, total: 3, readyToStart: false, unmet: ["C27"] });
  expect(await page.evaluate(() => window.__ROADMAP_TEST__.ancestors("C17"))).toEqual([
    "M1", "M2", "M13", "M14", "P16", "C1", "C14", "M4", "M10", "M18", "M20", "C16", "C4", "C27"
  ]);

  await page.locator('.node[data-node-id="C17"]').click();
  await expect(page.locator('[data-learning-sequence="C17"] .tagbtn')).toHaveText(["C27 Concentration and gas volumes"]);
  await expect(page.locator('[data-prerequisite-id="C27"]')).toContainText("Not run - Still blocking");
  await expect(page.locator('[data-prerequisite-id="C16"]')).toContainText("Green - Met");
  await page.evaluate(() => document.querySelector('.node[data-node-id="C16"]').click());
  expect(await page.locator(".node.upstream").count()).toBeGreaterThan(0);
  expect(await page.locator(".node.downstream").count()).toBeGreaterThan(0);
  expect(await page.locator(".node.unrelated").count()).toBeGreaterThan(0);
  for (const width of [320, 390, 768, 1360]) {
    await page.setViewportSize({ width, height: 900 });
    expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
  }
  expect(failures).toEqual([]);
});

test("all direct parents can be met while an earlier ancestor still blocks", async ({ page }) => {
  await routeDependencies(page, true);
  await page.addInitScript(({ key }) => {
    const met = ["C16", "C27", "M20"];
    const progress = Object.fromEntries(met.map((id) => [id, { status: "quali", notes: "", updatedAt: "2026-01-01T00:00:00.000Z" }]));
    localStorage.setItem(key, JSON.stringify({ version: 1, progress }));
  }, { key: storageKey });
  await page.goto(baseUrl);
  await page.waitForFunction(() => window.__ROADMAP_TEST__);
  expect(await page.evaluate(() => window.__ROADMAP_TEST__.readiness("C17"))).toEqual({
    ready: 3,
    total: 3,
    readyToStart: false,
    unmet: ["M1", "M2", "M13", "M14", "P16", "C1", "C14", "M4", "M10", "M18", "C4"]
  });
});

test("degraded path keeps tabs, drawer, persistence, and exact frontier working", async ({ page }) => {
  await routeDependencies(page, false);
  const failures = collectFailures(page, true);
  await page.goto(baseUrl);
  await page.waitForFunction(() => window.__ROADMAP_TEST__ && document.querySelectorAll(".node").length === 121, null, { timeout: 10000 });

  expect(await page.evaluate(() => window.__ROADMAP_TEST__.actionableIds())).toEqual(["M1", "M2", "M13", "P16", "C1", "B1"]);
  await page.getByRole("button", { name: "Roadmap", exact: true }).click();
  await expect(page.locator("[data-frontier] .tagbtn")).toHaveCount(6);
  await page.locator('.lcard[data-node-id="M1"]').click();
  await expect(page.locator('[data-learning-sequence="M1"]')).toContainText("Nothing is blocking this topic");
  await page.getByRole("button", { name: /Green - can do it/ }).click();
  expect(await page.evaluate((key) => JSON.parse(localStorage.getItem(key)).progress.M1.status, storageKey)).toBe("quali");
  await page.reload();
  await page.waitForFunction(() => window.__ROADMAP_TEST__);
  expect(await page.evaluate(() => window.__ROADMAP_TEST__.actionableIds())).toEqual(["M2", "M13", "P16", "C1", "B1", "M4", "M5", "M10"]);

  await page.getByRole("button", { name: "Garage", exact: true }).click();
  await expect(page.locator(".dcard")).toHaveCount(9);
  await page.getByRole("button", { name: "How to study", exact: true }).click();
  await expect(page.locator(".explainer:visible")).toContainText("Retrieval practice");
  await page.getByRole("button", { name: "F1 explained", exact: true }).click();
  await expect(page.locator(".explainer:visible")).toContainText("The 2026 grid");
  await page.getByRole("button", { name: "Track map", exact: true }).click();
  await expect(page.locator(".node")).toHaveCount(121);

  const downloadPromise = page.waitForEvent("download");
  await page.getByRole("button", { name: "Export progress", exact: true }).click();
  const download = await downloadPromise;
  const exportedPath = await download.path();
  page.once("dialog", (dialog) => dialog.accept());
  await page.getByRole("button", { name: "Reset", exact: true }).click();
  expect(await page.evaluate(() => window.__ROADMAP_TEST__.actionableIds())).toEqual(["M1", "M2", "M13", "P16", "C1", "B1"]);
  await page.locator('input[type="file"]').setInputFiles(exportedPath);
  await page.getByRole("button", { name: "Replace all", exact: true }).click();
  expect(await page.evaluate((key) => JSON.parse(localStorage.getItem(key)).progress.M1.status, storageKey)).toBe("quali");

  for (const width of [320, 390, 768, 1360]) {
    await page.setViewportSize({ width, height: 900 });
    expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
  }
  expect(failures.filter((failure) => !failure.includes("ERR_FAILED"))).toEqual([]);
});

test("M1 loads constrained support, exact links, prompt copy, and empty papers", async ({ page, context }) => {
  await routeDependencies(page, true);
  await page.route("**/gcse-science-f1-roadmap/topics/maths/m1/topic.json", async (route) => {
    await new Promise((resolve) => setTimeout(resolve, 100));
    await route.continue();
  });
  const failures = collectFailures(page);
  await context.grantPermissions(["clipboard-read", "clipboard-write"], { origin: new URL(baseUrl).origin });
  await waitForRoadmap(page);
  await openTopic(page, "M1");
  await expect(page.locator('[data-support-topic="M1"]')).toContainText("Loading topic support");
  await expect(page.locator('[data-support-topic="M1"]')).toContainText("Standard form");
  await expect(page.locator('[data-support-topic="M1"] a').first()).toHaveAttribute("href", "https://www.mathsgenie.co.uk/standard-form.html");
  await expect(page.locator('[data-support-topic="M1"] a').nth(1)).toHaveAttribute("href", "https://corbettmaths.com/contents/");
  await expect(page.locator('[data-support-topic="M1"] a').nth(2)).toHaveAttribute("href", "https://en.wikipedia.org/wiki/Scientific_notation");
  await page.getByRole("button", { name: "Copy tutor prompt", exact: true }).click();
  await expect(page.locator('[data-support-topic="M1"]')).toContainText("Prompt copied.");
  const expectedPrompt = fs.readFileSync(path.join(resourceRoot, "topics/maths/m1/explain-prompt.md"), "utf8");
  expect(await page.evaluate(() => navigator.clipboard.readText())).toBe(expectedPrompt);
  await expect(page.locator('[data-support-papers="M1"]')).toContainText("No past-paper questions have been mapped");
  expect(await page.evaluate(() => window.__ROADMAP_TEST__.supportState())).toEqual({ topicId: "M1", topic: "ready", papers: "empty" });
  for (const width of [320, 390, 768, 1360]) {
    await page.setViewportSize({ width, height: 900 });
    expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
  }
  expect(failures).toEqual([]);
});

test("a delayed M1 response cannot replace another topic drawer", async ({ page }) => {
  await routeDependencies(page, true);
  await page.route("**/gcse-science-f1-roadmap/topics/maths/m1/topic.json", async (route) => {
    await new Promise((resolve) => setTimeout(resolve, 300));
    await route.continue();
  });
  await waitForRoadmap(page);
  await openTopic(page, "M1");
  await page.evaluate(() => document.querySelector('.node[data-node-id="C1"]').click());
  await expect(page.locator(".detail-body h2").first()).toHaveText("Atomic structure and the periodic table");
  await expect(page.locator('[data-support-topic="C1"]')).toContainText("Support is not available");
  await page.waitForTimeout(400);
  await expect(page.locator(".detail-body")).not.toContainText("Standard form writes a number");
});

for (const resourceFailure of [
  {
    name: "missing topic JSON",
    route: "**/gcse-science-f1-roadmap/topics/maths/m1/topic.json",
    fulfill: (route) => route.fulfill({ status: 404, body: "missing" }),
    copy: "Support is not available"
  },
  {
    name: "malformed topic JSON",
    route: "**/gcse-science-f1-roadmap/topics/maths/m1/topic.json",
    fulfill: (route) => route.fulfill({ contentType: "application/json", body: "{" }),
    copy: "support files are invalid"
  },
  {
    name: "invalid Markdown",
    route: "**/gcse-science-f1-roadmap/topics/maths/m1/summary.md",
    fulfill: (route) => route.fulfill({ contentType: "text/markdown", body: "<script>alert(1)</script>" }),
    copy: "support files are invalid"
  },
  {
    name: "catalog failure",
    route: "**/gcse-science-f1-roadmap/catalog.json",
    fulfill: (route) => route.fulfill({ status: 503, body: "unavailable" }),
    copy: "Support is not available"
  }
]) {
  test(`${resourceFailure.name} is contained while progress controls remain usable`, async ({ page }) => {
    await routeDependencies(page, true);
    await page.route(resourceFailure.route, resourceFailure.fulfill);
    await waitForRoadmap(page);
    await openTopic(page, "M1");
    await expect(page.locator('[data-support-topic="M1"]')).toContainText(resourceFailure.copy);
    await page.getByRole("button", { name: /Green - can do it/ }).click();
    expect(await page.evaluate((key) => JSON.parse(localStorage.getItem(key)).progress.M1.status, storageKey)).toBe("quali");
    const notes = page.locator("textarea.plain, jelly-textarea");
    await expect(notes).toHaveCount(1);
  });
}

test("paper graph failure does not suppress valid topic resources", async ({ page }) => {
  await routeDependencies(page, true);
  await page.route("**/gcse-science-f1-roadmap/papers.json", (route) => route.fulfill({ contentType: "application/json", body: "{" }));
  await waitForRoadmap(page);
  await openTopic(page, "M1");
  await expect(page.locator('[data-support-topic="M1"]')).toContainText("Standard form");
  await expect(page.locator('[data-support-topic="M1"] a')).toHaveCount(3);
  await expect(page.getByRole("button", { name: "Copy tutor prompt" })).toBeVisible();
  await expect(page.locator('[data-support-papers="M1"]')).toContainText("Past-paper links are unavailable");
});

test("paper support graph cannot alter curriculum calculations", async ({ page }) => {
  await routeDependencies(page, true);
  const paperGraph = {
    schemaVersion: 1,
    nodes: [{
      id: "paper:test-m1", type: "past-paper", board: "AQA", qualification: "8463", series: "June 2025", component: "Paper 1", tier: "Higher",
      urls: { questionPaper: "https://example.org/questions.pdf", markScheme: "https://example.org/marks.pdf" }
    }],
    edges: [{ from: "paper:test-m1", to: "topic:M1", questions: ["01.1"], pages: [2], note: "Standard-form conversion." }]
  };
  await page.route("**/gcse-science-f1-roadmap/papers.json", (route) => route.fulfill({ contentType: "application/json", body: JSON.stringify(paperGraph) }));
  await waitForRoadmap(page);
  const before = await page.evaluate(() => ({
    topologicalIds: window.__ROADMAP_TEST__.topologicalIds(),
    ancestors: window.__ROADMAP_TEST__.ancestors("C17"),
    descendants: window.__ROADMAP_TEST__.descendants("M1"),
    readiness: window.__ROADMAP_TEST__.readiness("C17"),
    actionableIds: window.__ROADMAP_TEST__.actionableIds()
  }));
  await openTopic(page, "M1");
  await expect(page.locator('[data-support-papers="M1"]')).toContainText("questions 01.1");
  const after = await page.evaluate(() => ({
    topologicalIds: window.__ROADMAP_TEST__.topologicalIds(),
    ancestors: window.__ROADMAP_TEST__.ancestors("C17"),
    descendants: window.__ROADMAP_TEST__.descendants("M1"),
    readiness: window.__ROADMAP_TEST__.readiness("C17"),
    actionableIds: window.__ROADMAP_TEST__.actionableIds()
  }));
  expect(after).toEqual(before);
});

test("blocked Marked hides previews safely but preserves exact prompt copy", async ({ page, context }) => {
  await routeDependencies(page, { marked: false });
  await context.grantPermissions(["clipboard-read", "clipboard-write"], { origin: new URL(baseUrl).origin });
  await waitForRoadmap(page);
  await openTopic(page, "M1");
  await expect(page.locator('[data-support-topic="M1"]')).toContainText("Markdown renderer is unavailable");
  await expect(page.locator('[data-support-topic="M1"]')).not.toContainText("## Standard form");
  await page.getByRole("button", { name: "Copy tutor prompt" }).click();
  const expectedPrompt = fs.readFileSync(path.join(resourceRoot, "topics/maths/m1/explain-prompt.md"), "utf8");
  expect(await page.evaluate(() => navigator.clipboard.readText())).toBe(expectedPrompt);
});

test("file protocol shows HTTP guidance and never boots the roadmap", async ({ page }) => {
  await routeDependencies(page, false);
  await page.goto(`file://${htmlPath}`);
  await expect(page.getByRole("heading", { name: "Open this roadmap through HTTP" })).toBeVisible();
  await expect(page.locator("#shell")).toContainText("python3 -m http.server 8000");
  await expect(page.locator(".node")).toHaveCount(0);
  expect(await page.evaluate(() => window.__ROADMAP_TEST__)).toBeUndefined();
});
