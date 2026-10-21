const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const root = path.resolve(__dirname, "../../../prototypes");
const nostrBundle = path.resolve(__dirname, "../../nostr/tests/fixtures/nostr.bundle.js");
const BUNDLE_URL = "https://cdn.jsdelivr.net/npm/nostr-tools@2.25.2/lib/nostr.bundle.js";
const html = fs.readFileSync(path.join(root, "index.html"), "utf8");
const M = JSON.parse(html.match(/<script type="application\/json" id="index-manifest">\s*([\s\S]*?)\s*<\/script>/)[1]);
const C = M.copy;
const fill = (tpl, vars) => tpl.replace(/\{(\w+)\}/g, (all, key) => (key in vars ? String(vars[key]) : all));
const chromeCandidates = [process.env.CHROME_PATH, "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"];
const chromePath = chromeCandidates.find((candidate) => candidate && fs.existsSync(candidate));
if (chromePath) test.use({ launchOptions: { executablePath: chromePath } });

let server;
let base;

test.beforeAll(async () => {
  if (!fs.existsSync(nostrBundle)) throw new Error("Missing the nostr-tools fixture. Run prototype.tests/nostr/tests/fetch-fixtures.sh first.");
  server = http.createServer((request, response) => {
    const file = path.resolve(root, decodeURIComponent(new URL(request.url, "http://localhost").pathname.slice(1)) || "index.html");
    if (!file.startsWith(root + path.sep) || !fs.existsSync(file) || !fs.statSync(file).isFile()) { response.writeHead(404); response.end(); return; }
    const types = { ".html": "text/html", ".css": "text/css", ".js": "text/javascript" };
    response.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
    response.end(fs.readFileSync(file));
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  base = `http://127.0.0.1:${server.address().port}`;
});

test.afterAll(async () => {
  if (server) await new Promise((resolve) => server.close(resolve));
});

async function open(page, width = 1360) {
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => { if (message.type() === "error") errors.push(message.text()); });
  await page.context().route((url) => !url.href.startsWith(base), (route) => (route.request().url() === BUNDLE_URL
    ? route.fulfill({ contentType: "text/javascript", path: nostrBundle })
    : route.abort()));
  await page.setViewportSize({ width, height: 900 });
  await page.goto(`${base}/index.html`);
  await expect(page.locator("#result-line")).not.toBeEmpty();
  return errors;
}

const visibleCards = (page) => page.locator(".app:visible");
const groupCount = (page, id) => page.locator(`#group-${id} .count`);

test("groups apps by where their data lives, with counts from the manifest", async ({ page }) => {
  const errors = await open(page);
  const counts = Object.fromEntries(M.groups.map((g) => [g.id, M.apps.filter((a) => a.group === g.id).length]));
  await expect(page.locator("#stats")).toHaveText(fill(C.stats, { apps: M.apps.length, ...counts }));
  for (const group of M.groups) {
    await expect(page.locator(`#group-${group.id} h2`)).toContainText(group.title);
    await expect(groupCount(page, group.id)).toHaveText(String(counts[group.id]));
    const titles = await page.locator(`#group-${group.id} .app-head a`).allTextContents();
    expect(titles).toEqual(M.apps.filter((a) => a.group === group.id).map((a) => a.title));
  }
  await expect(page.locator("#result-line")).toHaveText(fill(C.resultAll, { total: M.apps.length }));
  // Every card links to a page that exists.
  for (const href of await page.locator(".app-head a").evaluateAll((links) => links.map((a) => a.getAttribute("href")))) {
    expect(fs.existsSync(path.join(root, href)), href).toBe(true);
  }
  expect(errors).toEqual([]);
});

test("a card shows its data tags and opens details inline", async ({ page }) => {
  const errors = await open(page, 390);
  const app = M.apps.find((a) => a.id === "voice-kit");
  const card = page.locator(`#app-${app.id}`);
  await expect(card.locator(".tag")).toHaveText([...app.sources.map((s) => M.sources.find((x) => x.id === s).label), ...app.needs.map((n) => M.needs.find((x) => x.id === n).label)]);
  await card.locator("summary").click();
  await expect(card.locator("dl.facts")).toContainText(app.saves);
  await expect(card.locator("dl.facts")).toContainText(app.data);
  await expect(card.locator("dl.facts dd").nth(2)).toHaveText(app.needs.map((n) => M.needs.find((x) => x.id === n).label).join(", "));
  expect(errors).toEqual([]);
});

test("search and the data-source filter narrow the list, hide empty groups and survive a reload", async ({ page }) => {
  const errors = await open(page);
  await page.locator("#search").fill("microphone");
  await expect(visibleCards(page)).toHaveCount(M.apps.filter((a) => a.needs.includes("microphone")).length);
  await expect(page.locator("#group-browser")).toBeHidden();
  await expect(page.locator("#result-line")).toHaveText(fill(C.resultSome, { shown: 2, total: M.apps.length }));

  await page.locator("#search").fill("");
  await page.locator("#source-filter").getByRole("button", { name: "Your file" }).click();
  const withFile = M.apps.filter((a) => a.sources.includes("file"));
  await expect(visibleCards(page)).toHaveCount(withFile.length);
  await expect(groupCount(page, "browser")).toHaveText(String(withFile.filter((a) => a.group === "browser").length));
  await expect(page.locator("#source-filter").getByRole("button", { name: "Your file" })).toHaveAttribute("aria-pressed", "true");

  await page.reload();
  await expect(page.locator("#source-filter").getByRole("button", { name: "Your file" })).toHaveAttribute("aria-pressed", "true");
  await expect(visibleCards(page)).toHaveCount(withFile.length);
  expect(errors).toEqual([]);
});

test("no match shows an empty state that resets the view", async ({ page }) => {
  const errors = await open(page);
  await page.locator("#search").fill("zzzz no such app");
  await expect(visibleCards(page)).toHaveCount(0);
  await expect(page.locator("#empty")).toBeVisible();
  await page.locator("#empty-reset").click();
  await expect(visibleCards(page)).toHaveCount(M.apps.length);
  await expect(page.locator("#search")).toBeFocused();
  await expect(page.locator("#empty")).toBeHidden();
  expect(errors).toEqual([]);
});

test("the / key focuses search, but not while you type in another field", async ({ page }) => {
  const errors = await open(page);
  await page.locator("body").click({ position: { x: 5, y: 300 } });
  await page.keyboard.press("/");
  await expect(page.locator("#search")).toBeFocused();
  await expect(page.locator("#search")).toHaveValue("");
  // The Nostr key field in the account drawer (shadow DOM) must receive "/" as text.
  await page.locator("prototype-shell nostr-account .badge").click();
  await page.locator("nostr-account #na-secret").click();
  await page.keyboard.type("a/b");
  await expect(page.locator("nostr-account #na-secret")).toHaveValue("a/b");
  expect(errors).toEqual([]);
});

for (const width of [320, 390, 768, 1360]) {
  test(`no horizontal page scroll at ${width}px`, async ({ page }) => {
    const errors = await open(page, width);
    await page.locator(".app details summary").first().click();
    expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
    expect(errors).toEqual([]);
  });
}
