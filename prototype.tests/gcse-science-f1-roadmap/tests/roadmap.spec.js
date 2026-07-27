const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const workspace = path.resolve(__dirname, "../../..");
const htmlPath = path.join(workspace, "prototypes/gcse-science-f1-roadmap.html");
const fixtures = path.join(__dirname, "fixtures");
const storageKey = "gcse-science-f1-roadmap-v1";
const chromePath = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
let server;
let baseUrl;

if (fs.existsSync(chromePath)) test.use({ launchOptions: { executablePath: chromePath } });

test.beforeAll(async () => {
  for (const name of ["jelly.js", "open-props.css"]) {
    if (!fs.existsSync(path.join(fixtures, name))) throw new Error(`Missing ${name}. Run tests/fetch-fixtures.sh first.`);
  }
  server = http.createServer((request, response) => {
    if (request.url === "/gcse-science-f1-roadmap.html") {
      response.writeHead(200, { "content-type": "text/html" });
      response.end(fs.readFileSync(htmlPath));
      return;
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
  if (!available) {
    await page.route("https://jelly-ui.com/**", (route) => route.abort());
    await page.route("https://unpkg.com/**", (route) => route.abort());
    return;
  }
  await page.route("https://jelly-ui.com/package.js", (route) => route.fulfill({
    contentType: "text/javascript",
    body: "export * from 'https://jelly-ui.com/dist/jelly.js';"
  }));
  await page.route("https://jelly-ui.com/dist/jelly.js", (route) => route.fulfill({
    contentType: "text/javascript",
    path: path.join(fixtures, "jelly.js")
  }));
  await page.route("https://unpkg.com/open-props", (route) => route.fulfill({
    contentType: "text/css",
    path: path.join(fixtures, "open-props.css")
  }));
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
