// Checks the launcher manifest against the prototypes folder, so the index cannot drift from the apps.
import fs from "node:fs";
import path from "node:path";

const [dirArg] = process.argv.slice(2);
if (!dirArg) throw new Error("Usage: node validate-index.mjs <prototypes-dir>");
const dir = path.resolve(dirArg);
const html = fs.readFileSync(path.join(dir, "index.html"), "utf8");
const match = html.match(/<script type="application\/json" id="index-manifest">\s*([\s\S]*?)\s*<\/script>/);
if (!match) throw new Error("Manifest script not found");
const M = JSON.parse(match[1]);
const code = html.slice(html.indexOf('<script type="module">'));
const errors = [];

try { new Function(code.slice(code.indexOf(">") + 1, code.indexOf("</script>"))); } catch (error) { errors.push(`Page script does not parse: ${error.message}`); }
for (const [, key] of code.matchAll(/\bC\.(\w+)/g)) if (typeof M.copy[key] !== "string") errors.push(`Missing copy: ${key}`);

const groups = new Set(M.groups.map((g) => g.id));
const sources = new Set(M.sources.map((s) => s.id));
const needs = new Set(M.needs.map((n) => n.id));
const pages = fs.readdirSync(dir).filter((f) => f.endsWith(".html") && !["index.html", "about.html"].includes(f));
const listed = M.apps.map((a) => a.url);
for (const page of pages) if (!listed.includes(page)) errors.push(`Prototype not listed in the index: ${page}`);
const seen = new Set();
for (const app of M.apps) {
  const where = `App ${app.id}`;
  if (seen.has(app.id)) errors.push(`${where}: duplicate id`);
  seen.add(app.id);
  for (const field of ["title", "url", "summary", "saves", "data"]) if (typeof app[field] !== "string" || !app[field].trim()) errors.push(`${where}: missing ${field}`);
  if (!fs.existsSync(path.join(dir, app.url))) { errors.push(`${where}: ${app.url} does not exist`); continue; }
  if (!groups.has(app.group)) errors.push(`${where}: unknown group ${app.group}`);
  if (!app.sources.length || !app.sources.every((s) => sources.has(s))) errors.push(`${where}: sources must be known and not empty`);
  if (!app.needs.every((n) => needs.has(n))) errors.push(`${where}: unknown need`);
  // The group must match what the page really does with data.
  const source = fs.readFileSync(path.join(dir, app.url), "utf8");
  const syncs = source.includes('src="nostr/store.js"');
  const saves = /localStorage|indexedDB/.test(source);
  const actual = syncs ? "synced" : saves ? "browser" : "none";
  if (actual !== app.group) errors.push(`${where}: listed as '${app.group}', but the page code is '${actual}'`);
  if (app.needs.includes("microphone") && !source.includes("getUserMedia")) errors.push(`${where}: needs a microphone, but the page never asks for one`);
  if (source.includes("getUserMedia") && !app.needs.includes("microphone")) errors.push(`${where}: the page asks for the microphone; add the need`);
  if (source.includes("navigator.gpu") !== app.needs.includes("webgpu")) errors.push(`${where}: WebGPU need does not match the page`);
}
if (html.includes("\u2014")) errors.push("Em dash found; use a plain dash");

if (errors.length) { console.error(errors.join("\n")); process.exit(1); }
console.log(`Index manifest OK: ${M.apps.length} apps in ${M.groups.length} groups, all ${pages.length} prototype pages listed.`);
