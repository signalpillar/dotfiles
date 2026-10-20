const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const root = path.resolve(__dirname, "../../../prototypes");
const nostrBundle = path.resolve(__dirname, "../../nostr/tests/fixtures/nostr.bundle.js");
const BUNDLE_URL = "https://cdn.jsdelivr.net/npm/nostr-tools@2.25.2/lib/nostr.bundle.js";
const pages = fs.readdirSync(root).filter((f) => f.endsWith(".html")).sort();
const about = JSON.parse(fs.readFileSync(path.join(root, "about.html"), "utf8").match(/<script type="application\/json" id="about-manifest">\s*([\s\S]*?)\s*<\/script>/)[1]);
const chromeCandidates = [process.env.CHROME_PATH, "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"];
const chromePath = chromeCandidates.find((candidate) => candidate && fs.existsSync(candidate));
if (chromePath) test.use({ launchOptions: { executablePath: chromePath } });

const types = { ".html": "text/html", ".css": "text/css", ".js": "text/javascript", ".json": "application/json", ".yaml": "text/yaml", ".md": "text/markdown", ".csv": "text/csv" };
let server;
let base;

test.beforeAll(async () => {
  if (!fs.existsSync(nostrBundle)) throw new Error("Missing the nostr-tools fixture. Run prototype.tests/nostr/tests/fetch-fixtures.sh first.");
  // Serves prototypes/ as static files. "?noshell" on the page URL serves an empty shell.js (the baseline).
  server = http.createServer((request, response) => {
    const url = new URL(request.url, "http://localhost");
    const file = path.resolve(root, decodeURIComponent(url.pathname.slice(1)) || "index.html");
    if (!file.startsWith(root + path.sep) || !fs.existsSync(file) || !fs.statSync(file).isFile()) { response.writeHead(404); response.end(); return; }
    const noShell = url.pathname === "/shell.js" && (request.headers.referer || "").includes("noshell");
    response.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
    response.end(noShell ? "" : fs.readFileSync(file));
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  base = `http://127.0.0.1:${server.address().port}`;
});

test.afterAll(async () => {
  if (server) await new Promise((resolve) => server.close(resolve));
});

// Loads one page with every third-party request blocked except the nostr-tools fixture.
async function load(page, file, { width, shell }) {
  const errors = [];
  page.on("pageerror", (error) => errors.push(`pageerror: ${error.message}`));
  page.on("console", (message) => {
    if (message.type() === "error" && !/Failed to load resource|net::ERR_FAILED/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  await page.context().route((url) => !url.href.startsWith(base), (route) => (route.request().url() === BUNDLE_URL
    ? route.fulfill({ contentType: "text/javascript", path: nostrBundle })
    : route.abort()));
  await page.setViewportSize({ width, height: 900 });
  await page.goto(`${base}/${file}${shell ? "" : "?noshell"}`, { waitUntil: "load" });
  await page.waitForTimeout(400);
  const overflow = await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
  return { errors, overflow };
}

for (const file of pages) {
  for (const width of [320, 1360]) {
    test(`${file} at ${width}px: the shell bar is first, links home and to About, and adds no overflow or errors`, async ({ browser }) => {
      const without = await browser.newContext().then(async (ctx) => { const p = await ctx.newPage(); const r = await load(p, file, { width, shell: false }); await ctx.close(); return r; });
      const ctx = await browser.newContext();
      const page = await ctx.newPage();
      const withShell = await load(page, file, { width, shell: true });

      expect(await page.evaluate(() => document.body.firstElementChild.tagName)).toBe("PROTOTYPE-SHELL");
      expect(await page.locator("prototype-shell").count()).toBe(1);
      const home = page.locator("prototype-shell a.home");
      const aboutLink = page.locator("prototype-shell a.about");
      await expect(aboutLink).toHaveAttribute("href", "about.html");
      if (file === "index.html") await expect(home).toBeHidden();
      else {
        await expect(home).toBeVisible();
        await expect(home).toHaveAttribute("href", "index.html");
      }
      if (file === "about.html") await expect(aboutLink).toHaveAttribute("aria-current", "page");
      const bar = await page.locator("prototype-shell").boundingBox();
      expect(bar.height).toBeLessThanOrEqual(56);
      expect(await page.evaluate(() => { const nav = document.querySelector("prototype-shell").shadowRoot.querySelector("nav"); return nav.scrollWidth - nav.clientWidth; })).toBeLessThanOrEqual(0);

      // The shell must not make anything worse than the page already is.
      expect(withShell.overflow).toBeLessThanOrEqual(Math.max(0, without.overflow));
      // Pages built with the shell must be clean on their own, not only "no worse than before".
      if (["about.html", "index.html"].includes(file)) expect(withShell.errors).toEqual([]);
      const known = new Set(without.errors);
      expect(withShell.errors.filter((e) => !known.has(e))).toEqual([]);
      await ctx.close();
    });
  }
}

test("pages that load the Nostr scripts show the account badge in the bar", async ({ page }) => {
  for (const file of pages) {
    const usesNostr = fs.readFileSync(path.join(root, file), "utf8").includes('src="nostr/account.js"');
    await load(page, file, { width: 1360, shell: true });
    const badge = page.locator("prototype-shell > nostr-account[slot=account]");
    expect(await badge.count(), file).toBe(usesNostr ? 1 : 0);
    if (usesNostr) await expect(badge.locator(".badge"), file).toHaveText("Sign in to sync");
  }
});

test("the About page renders every section and deletes all prototype data on request", async ({ page }) => {
  await load(page, "about.html", { width: 390, shell: true });
  await expect(page.locator("h1")).toHaveText(about.title);
  for (const section of about.sections) {
    await expect(page.locator(`#${section.id} h2`)).toHaveText(section.title);
    await expect(page.locator(`nav.toc a[href="#${section.id}"]`)).toHaveText(section.title);
  }
  const hosts = about.sections.find((s) => s.hosts).hosts;
  await expect(page.locator("#privacy tbody tr")).toHaveCount(hosts.length);

  // Put data where prototypes keep it: localStorage, IndexedDB, a stored Nostr key.
  await page.evaluate(async () => {
    localStorage.setItem("some-prototype.v1", "1");
    await window.NostrPrototypes.account.createKey();
    await window.NostrPrototypes.openStore({ appId: "probe", collections: { log: { type: "log" } } }).then((s) => s.append("log", { id: "x" }));
  });
  await expect(page.locator("prototype-shell nostr-account .badge")).not.toHaveText("Sign in to sync");

  const action = about.sections.find((s) => s.type === "action");
  await page.locator("#delete-all").click();
  await expect(page.locator("#delete-all")).toHaveText(action.confirm);
  await page.locator("#delete-all").click();
  await expect(page.locator("#delete-status")).toContainText("Deleted:");
  expect(await page.evaluate(() => localStorage.length)).toBe(0);
  expect(await page.evaluate(async () => (await indexedDB.databases()).length)).toBe(0);
  await expect(page.locator("prototype-shell nostr-account .badge")).toHaveText("Sign in to sync");
});

test("the bar is reachable by keyboard and goes back to the launcher", async ({ page }) => {
  await load(page, "meeting-cost-meter.html", { width: 1360, shell: true });
  await page.keyboard.press("Tab");
  expect(await page.evaluate(() => document.activeElement.tagName)).toBe("PROTOTYPE-SHELL");
  await page.keyboard.press("Enter");
  await page.waitForURL(/index\.html$/);
});
