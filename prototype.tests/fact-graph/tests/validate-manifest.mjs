import fs from "node:fs";
import path from "node:path";

const [htmlArg] = process.argv.slice(2);
if (!htmlArg) throw new Error("Usage: node validate-manifest.mjs <html-file>");

const html = fs.readFileSync(path.resolve(htmlArg), "utf8");
const manifestMatch = html.match(/<script type="application\/json" id="fact-graph-manifest">\s*([\s\S]*?)\s*<\/script>/);
if (!manifestMatch) throw new Error("Manifest script not found");
const manifest = JSON.parse(manifestMatch[1]);
const code = html.slice(html.indexOf('<script type="module">'));
const styles = html.slice(html.indexOf("<style>"), html.indexOf("</style>"));
const errors = [];

// Every copy key the code reads exists and is non-empty.
for (const [, key] of code.matchAll(/\bC\.(\w+)/g)) {
  const value = manifest.copy[key];
  if (!(typeof value === "string" && value.trim()) && !(Array.isArray(value) && value.length)) errors.push(`Missing copy: ${key}`);
}
// Every colour the code or CSS reads exists.
for (const [, key] of code.matchAll(/\bK\.(\w+)/g)) if (!manifest.colors[key]) errors.push(`Missing colour: ${key}`);
for (const [, key] of styles.matchAll(/var\(--fg-([\w-]+)\)/g)) if (!manifest.colors[key]) errors.push(`Missing CSS colour: ${key}`);
for (const [, key] of code.matchAll(/\bS\.(\w+)/g)) if (typeof manifest.graphStyle[key] !== "number") errors.push(`Missing graph style: ${key}`);
// Every element id the code looks up exists in the markup.
for (const [, id] of code.matchAll(/\$\("([\w-]+)"\)/g)) if (!html.includes(`id="${id}"`)) errors.push(`Missing element id: ${id}`);
// Syntax patterns compile with the flags the code uses.
for (const [key, source] of Object.entries(manifest.syntax)) {
  try { new RegExp(source, key === "slugStrip" ? "gu" : ""); } catch (error) { errors.push(`Bad regex ${key}: ${error.message}`); }
}
// Seed entries have the fields the boot code reads.
const graphs = new Set(manifest.seed.filter((e) => e.kind === "graph").map((e) => e.slug));
manifest.seed.forEach((entry, index) => {
  if (!["graph", "fact"].includes(entry.kind)) errors.push(`Seed ${index + 1}: unknown kind ${entry.kind}`);
  if (Number.isNaN(Date.parse(entry.at))) errors.push(`Seed ${index + 1}: bad time`);
  if (entry.kind === "fact" && !graphs.has(entry.graph)) errors.push(`Seed ${index + 1}: graph ${entry.graph} has no graph entry`);
  if (entry.kind === "fact" && entry.text.split(new RegExp(manifest.syntax.arrow)).length !== 3) errors.push(`Seed ${index + 1}: not 'A -> r -> B'`);
});
if (!graphs.has(manifest.defaultGraph.slug)) errors.push(`Default graph ${manifest.defaultGraph.slug} is not seeded`);
for (const [, key] of html.matchAll(/M\.storage\.(\w+)/g)) if (!manifest.storage[key]) errors.push(`Missing storage key: ${key}`);
for (const [, key] of code.matchAll(/\bQM\.(\w+)/g)) if (manifest.query[key] === undefined) errors.push(`Missing query setting: ${key}`);
manifest.query.examples.forEach((group) => group.items.forEach((item) => {
  const text = item.query.join("\n");
  if (!item.title || !text.trim()) errors.push(`Empty example in ${group.group}`);
  const depth = [...text].reduce((d, ch) => (d < 0 ? d : d + (ch === "[" || ch === "(" ? 1 : ch === "]" || ch === ")" ? -1 : 0)), 0);
  if (depth !== 0) errors.push(`Unbalanced brackets in example: ${item.title}`);
}));
if (html.includes("\u2014")) errors.push("Em dash found; use a plain dash");

if (errors.length) {
  console.error(errors.join("\n"));
  process.exit(1);
}
console.log(`Manifest OK: ${manifest.seed.length} seed entries, ${Object.keys(manifest.copy).length} copy keys.`);
