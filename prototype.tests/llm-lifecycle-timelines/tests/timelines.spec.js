const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");
const yaml = require("js-yaml");

const workspace = path.resolve(__dirname, "../../..");
const htmlPath = path.join(workspace, "prototypes/llm-lifecycle-timelines.html");
const yamlPath = path.join(workspace, "prototypes/llm-lifecycle-timelines.yaml");
const baseCssPath = path.join(workspace, "prototypes/prototype-base.css");
const jellyCssPath = path.join(workspace, "prototypes/prototype-jelly.css");
const fixtures = path.join(__dirname, "fixtures");
const chromePath = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const model = yaml.load(fs.readFileSync(yamlPath, "utf8"));
const viewports = [320, 390, 768, 1360];
let server;
let baseUrl;

if (fs.existsSync(chromePath)) test.use({ launchOptions: { executablePath: chromePath } });

test.beforeAll(async () => {
  for (const name of ["jelly.js", "open-props.css", "js-yaml.js"]) {
    if (!fs.existsSync(path.join(fixtures, name))) throw new Error(`Missing ${name}. Run tests/fetch-fixtures.sh first.`);
  }
  server = http.createServer((request, response) => {
    const pathname = new URL(request.url, "http://localhost").pathname;
    const files = {
      "/llm-lifecycle-timelines.html": [htmlPath, "text/html"],
      "/llm-lifecycle-timelines.yaml": [yamlPath, "text/yaml; charset=utf-8"],
      "/prototype-base.css": [baseCssPath, "text/css; charset=utf-8"],
      "/prototype-jelly.css": [jellyCssPath, "text/css; charset=utf-8"]
    };
    const entry = files[pathname];
    if (!entry) {
      response.writeHead(404);
      response.end();
      return;
    }
    response.writeHead(200, { "content-type": entry[1] });
    response.end(fs.readFileSync(entry[0]));
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  baseUrl = `http://127.0.0.1:${server.address().port}/llm-lifecycle-timelines.html`;
});

test.afterAll(async () => {
  if (server) await new Promise((resolve) => server.close(resolve));
});

async function routeDependencies(page, available) {
  const options = typeof available === "boolean"
    ? { jelly: available, props: available, yaml: available }
    : { jelly: true, props: true, yaml: true, ...available };
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
  await page.route("https://cdn.jsdelivr.net/npm/js-yaml@4.1.0/dist/js-yaml.min.js", (route) => options.yaml
    ? route.fulfill({ contentType: "text/javascript", path: path.join(fixtures, "js-yaml.js") })
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

async function openTimeline(page, url = baseUrl) {
  await page.goto(url);
  await expect(page.locator(".stage-button").first()).toBeVisible();
}

async function selectTimeline(page, timelineId) {
  const index = ["development", "serving"].indexOf(timelineId);
  await page.locator(".timeline-tab").nth(index).click();
  await expect(page.locator("#timeline-title")).toHaveText(model[timelineId].title);
}

function noteCountFor(timelineId, stage) {
  const inherited = stage.related_overlays.reduce((total, overlayId) => {
    const overlay = model.overlays.find((candidate) => candidate.id === overlayId);
    return total + (overlay.notes || []).length;
  }, 0);
  return (stage.notes || []).length + inherited;
}

async function expectNoHorizontalOverflow(page) {
  for (const width of viewports) {
    await page.setViewportSize({ width, height: 900 });
    expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
  }
}

test("both timelines render every YAML stage in order as a vertical rail", async ({ page }) => {
  await routeDependencies(page, true);
  const failures = collectFailures(page);
  await openTimeline(page);

  expect(await page.locator("#page-title").textContent()).toBe(model.meta.title);
  expect(await page.evaluate(() => document.querySelector("jelly-theme").getAttribute("mode"))).toBe("light");
  expect(await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme)).toBe("light");

  for (const timelineId of ["development", "serving"]) {
    await selectTimeline(page, timelineId);
    const expected = model[timelineId].stages;
    await expect(page.locator(".stage-card h3")).toHaveText(expected.map((stage) => stage.name));
    await expect(page.locator(".stage-card > p")).toHaveText(expected.map((stage) => stage.summary));
    const tops = await page.locator(".stage-button").evaluateAll((nodes) => nodes.map((node) => node.getBoundingClientRect().top));
    const lefts = await page.locator(".stage-button").evaluateAll((nodes) => nodes.map((node) => node.getBoundingClientRect().left));
    expect(tops.every((top, index) => index === 0 || top > tops[index - 1])).toBe(true);
    expect(lefts.every((left) => Math.abs(left - lefts[0]) < 2)).toBe(true);
  }

  await expectNoHorizontalOverflow(page);
  expect(failures).toEqual([]);
});

test("the stage note badge always matches the notes the detail panel renders", async ({ page }) => {
  await routeDependencies(page, true);
  const failures = collectFailures(page);
  await openTimeline(page);

  for (const timelineId of ["development", "serving"]) {
    await selectTimeline(page, timelineId);
    const stages = model[timelineId].stages;
    for (let index = 0; index < stages.length; index += 1) {
      const expectedNotes = noteCountFor(timelineId, stages[index]);
      await page.locator(".stage-button").nth(index).click();
      await expect(page.locator("#detail h2")).toHaveText(stages[index].name);

      const badge = page.locator(".stage-button").nth(index).locator("jelly-badge", { hasText: /note/ });
      await expect(badge).toHaveCount(expectedNotes ? 1 : 0);
      if (expectedNotes) {
        await expect(badge).toHaveText(`${expectedNotes} ${expectedNotes === 1 ? "note" : "notes"}`);
      }

      const notesSection = page.locator("#detail section").filter({ has: page.getByRole("heading", { name: "Notes", exact: true }) });
      await expect(notesSection.locator("details.note")).toHaveCount(expectedNotes);
      if (!expectedNotes) await expect(notesSection.locator(".empty")).toHaveText("No notes attached yet.");
    }
  }
  expect(failures).toEqual([]);
});

test("compact screens expand detail under the selected stage instead of below the whole list", async ({ page }) => {
  await routeDependencies(page, true);
  await page.setViewportSize({ width: 390, height: 844 });
  await openTimeline(page, `${baseUrl}#serving/prefill`);

  const placement = await page.evaluate(() => {
    const detail = document.querySelector("#detail");
    const selected = document.querySelector('.stage-row[data-stage-id="prefill"]');
    const selectedButton = selected.querySelector(".stage-button");
    const detailTop = detail.getBoundingClientRect().top;
    const selectedBottom = selectedButton.getBoundingClientRect().bottom;
    const followingTops = [];
    let next = selected.nextElementSibling;
    while (next) {
      followingTops.push(next.querySelector(".stage-button").getBoundingClientRect().top);
      next = next.nextElementSibling;
    }
    return {
      parentClass: detail.parentElement.className,
      underSelected: detail.closest(".stage-row")?.dataset.stageId === "prefill",
      immediatelyBelow: detailTop >= selectedBottom - 1 && detailTop <= selectedBottom + 48,
      followingStagesBelowDetail: followingTops.every((top) => top > detailTop)
    };
  });

  expect(placement).toEqual({
    parentClass: "stage-detail-slot",
    underSelected: true,
    immediatelyBelow: true,
    followingStagesBelowDetail: true
  });
  await expect(page.locator("#detail h2")).toHaveText("Prefill (prompt forward pass)");
  expect(await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth)).toBe(0);
});

test("wide screens keep the detail in a sticky side pane", async ({ page }) => {
  await routeDependencies(page, true);
  await page.setViewportSize({ width: 1360, height: 900 });
  await openTimeline(page, `${baseUrl}#serving/prefill`);

  const placement = await page.evaluate(() => {
    const detail = document.querySelector("#detail");
    return {
      parentId: detail.parentElement.id,
      sticky: getComputedStyle(detail).position === "sticky",
      inlineSlotsEmpty: [...document.querySelectorAll(".stage-detail-slot")].every((slot) => !slot.contains(detail))
    };
  });
  expect(placement).toEqual({ parentId: "detail-pane", sticky: true, inlineSlotsEmpty: true });
});

test("primary controls use the compact size without losing touch targets", async ({ page }) => {
  await routeDependencies(page, true);
  await page.setViewportSize({ width: 390, height: 844 });
  await openTimeline(page, `${baseUrl}#serving/prefill`);

  const navigationControls = page.locator(".nav-actions jelly-button");
  await expect(navigationControls).toHaveCount(2);
  expect(await navigationControls.evaluateAll((nodes) => nodes.map((node) => node.getAttribute("size"))))
    .toEqual(["small", "small"]);
  await expect(page.locator(".stage-card jelly-badge").first()).toHaveAttribute("size", "small");
  const measurements = await page.evaluate(() => ({
    tab: document.querySelector(".timeline-tab").getBoundingClientRect().height,
    navigation: [...document.querySelectorAll(".nav-actions jelly-button")].map((node) => node.getBoundingClientRect().height),
    overlay: document.querySelector(".overlay-button").getBoundingClientRect().height
  }));
  expect(measurements.tab).toBeGreaterThanOrEqual(40);
  expect(measurements.tab).toBeLessThanOrEqual(46);
  expect(measurements.navigation.every((height) => height >= 40 && height <= 44)).toBe(true);
  expect(measurements.overlay).toBeGreaterThanOrEqual(40);
  expect(measurements.overlay).toBeLessThanOrEqual(44);

  await page.locator(".overlay-button").first().click();
  const close = page.locator("#overlay-drawer jelly-button", { hasText: "Close" });
  await expect(close).toHaveAttribute("size", "small");
  await expect(page.locator("#overlay-drawer jelly-chip").first()).toHaveAttribute("size", "small");
  const closeHeight = await close.evaluate((node) => node.getBoundingClientRect().height);
  expect(closeHeight).toBeGreaterThanOrEqual(40);
  expect(closeHeight).toBeLessThanOrEqual(44);
});

test("a note held by an overlay reaches its stages attributed and expandable", async ({ page }) => {
  await routeDependencies(page, true);
  await openTimeline(page, `${baseUrl}#serving/prefill`);
  await expect(page.locator("#detail h2")).toHaveText("Prefill (prompt forward pass)");

  const overlay = model.overlays.find((candidate) => candidate.id === "kernel_compute");
  const note = overlay.notes[0];
  const stageNote = page.locator("#detail details.note").first();
  await expect(stageNote.locator("summary")).toContainText(`Via ${overlay.name}`);
  await expect(stageNote.locator("summary")).toContainText(note.source);

  await stageNote.locator("summary").click();
  await expect(stageNote.locator(".note-body p").first()).toContainText("rewrote production");
  expect(await stageNote.evaluate((node) => node.open)).toBe(true);

  // The same note is unattributed inside its own overlay, where the name is the title.
  await page.locator(".overlay-button", { hasText: overlay.name }).click();
  const drawerNote = page.locator("#overlay-drawer details.note").first();
  await expect(drawerNote.locator("summary")).not.toContainText("Via ");
  await expect(drawerNote.locator("summary")).toContainText(note.source);
});

test("overlay detail opens in a bottom drawer rather than a popup", async ({ page }) => {
  await routeDependencies(page, true);
  const failures = collectFailures(page);
  await page.setViewportSize({ width: 390, height: 844 });
  await openTimeline(page, `${baseUrl}#serving/decode`);

  expect(await page.locator("dialog").count()).toBe(0);
  const drawer = page.locator("#overlay-drawer");
  await expect(drawer).not.toHaveAttribute("open", "");

  const overlay = model.overlays.find((candidate) => candidate.id === "kv_cache");
  await page.locator(".overlay-button", { hasText: overlay.name }).click();
  await expect(drawer).toHaveAttribute("open", "");
  await expect(drawer).toHaveAttribute("side", "bottom");
  await expect(drawer).toHaveAttribute("label", overlay.name);
  await expect(drawer.locator("h2")).toHaveText(overlay.name);
  await expect(drawer.locator("jelly-chip")).toHaveText(overlay.applies_to);

  // A stretched host with an intrinsic inner button paints its body past the real target.
  const closeBox = await page.evaluate(() => {
    const host = document.querySelector(".drawer-actions jelly-button");
    const inner = host.shadowRoot.querySelector("button");
    return { host: host.getBoundingClientRect().width, inner: inner.getBoundingClientRect().width };
  });
  expect(closeBox.inner).toBeCloseTo(closeBox.host, 0);

  await drawer.getByText("Close", { exact: true }).click();
  await expect(drawer).not.toHaveAttribute("open", "");

  // Closing must hand the page back: Jelly's teardown releases inert and the scroll lock.
  await expect(page.locator("jelly-theme")).not.toHaveAttribute("inert", "");
  expect(await page.evaluate(() => getComputedStyle(document.body).overflow)).not.toBe("hidden");

  await page.locator(".overlay-button", { hasText: overlay.name }).click();
  await expect(drawer).toHaveAttribute("open", "");
  await page.keyboard.press("Escape");
  await expect(drawer).not.toHaveAttribute("open", "");
  await page.locator(".stage-button").first().click();
  await expect(page.locator("#detail h2")).toHaveText(model.serving.stages[0].name);
  expect(failures).toEqual([]);
});

test("stage selection is shareable through the location hash", async ({ page }) => {
  await routeDependencies(page, true);
  await openTimeline(page, `${baseUrl}#development/sft`);
  await expect(page.locator("#detail h2")).toHaveText("Supervised fine-tuning (SFT)");

  await page.locator(".stage-button").nth(0).click();
  await expect(page).toHaveURL(/#development\/problem_and_spec$/);

  await selectTimeline(page, "serving");
  await expect(page).toHaveURL(/#serving\/ingress$/);
  await page.reload();
  await expect(page.locator("#detail h2")).toHaveText("Ingress & routing");
});

test("keyboard steps through stages and the detail panel follows", async ({ page }) => {
  await routeDependencies(page, true);
  await openTimeline(page);
  const stages = model.development.stages;

  await page.locator("#timeline-scroll").focus();
  await page.keyboard.press("ArrowDown");
  await expect(page.locator("#detail h2")).toHaveText(stages[1].name);
  await page.keyboard.press("ArrowUp");
  await expect(page.locator("#detail h2")).toHaveText(stages[0].name);

  await page.getByText("Next", { exact: true }).click();
  await expect(page.locator("#detail h2")).toHaveText(stages[1].name);
  await page.getByText("Previous", { exact: true }).click();
  await expect(page.locator("#detail h2")).toHaveText(stages[0].name);
});

test("a blocked component library still leaves both timelines and their notes usable", async ({ page }) => {
  await routeDependencies(page, { jelly: false, props: false });
  const failures = collectFailures(page, true);
  await openTimeline(page);

  await expect(page.locator(".stage-card h3")).toHaveText(model.development.stages.map((stage) => stage.name));
  await selectTimeline(page, "serving");
  await page.locator(".stage-button").nth(model.serving.stages.findIndex((stage) => stage.id === "prefill")).click();

  const stageNote = page.locator("#detail details.note").first();
  await stageNote.locator("summary").click();
  await expect(stageNote.locator(".note-body p").first()).toContainText("rewrote production");

  await page.locator(".overlay-button", { hasText: "Kernels" }).click();
  const drawer = page.locator("#overlay-drawer");
  await expect(drawer).toHaveAttribute("open", "");
  await expect(drawer.locator("h2")).toBeVisible();
  await drawer.getByText("Close", { exact: true }).click();
  await expect(drawer).not.toHaveAttribute("open", "");

  await expectNoHorizontalOverflow(page);
  expect(failures.filter((failure) => !failure.includes("ERR_FAILED"))).toEqual([]);
});

test("a blocked YAML parser reports itself rather than failing silently", async ({ page }) => {
  await routeDependencies(page, { yaml: false });
  await page.goto(baseUrl);

  await expect(page.locator("#status")).toContainText("YAML parser did not load");
  await expect(page.locator("#app")).toBeHidden();
});

test("an unreachable YAML file explains itself instead of rendering an empty page", async ({ page }) => {
  await routeDependencies(page, true);
  await page.route("**/llm-lifecycle-timelines.yaml", (route) => route.fulfill({ status: 404, body: "missing" }));
  await page.goto(baseUrl);

  await expect(page.locator("#status")).toContainText("could not read the neighbouring YAML file");
  await expect(page.locator("#status input[type=file]")).toHaveCount(1);
  await expect(page.locator("#app")).toBeHidden();
});
