import fs from "node:fs";
import path from "node:path";

const [htmlArg] = process.argv.slice(2);
if (!htmlArg) throw new Error("Usage: node validate-about.mjs <about.html>");
const html = fs.readFileSync(path.resolve(htmlArg), "utf8");
const match = html.match(/<script type="application\/json" id="about-manifest">\s*([\s\S]*?)\s*<\/script>/);
if (!match) throw new Error("Manifest script not found");
const manifest = JSON.parse(match[1]);
const code = html.slice(html.indexOf('<script type="module">'));
const renderers = [...code.matchAll(/^\s{8}(\w+): \(s\) =>/gm)].map((m) => m[1]);
const errors = [];
// Parse the page script, so a syntax error fails here instead of as a blank page.
const moduleSource = code.slice(code.indexOf(">") + 1, code.indexOf("</script>"));
try { new Function(moduleSource); } catch (error) { errors.push(`Page script does not parse: ${error.message}`); }
const ids = new Set();
for (const s of manifest.sections) {
  if (!renderers.includes(s.type)) errors.push(`Section ${s.id}: no renderer for type '${s.type}'`);
  if (ids.has(s.id)) errors.push(`Duplicate section id ${s.id}`);
  ids.add(s.id);
  if (s.type === "list" && !(Array.isArray(s.items) && s.items.length)) errors.push(`Section ${s.id}: list needs items`);
  if (s.type === "action" && !(s.button && s.confirm && s.done && s.failed)) errors.push(`Section ${s.id}: action needs button, confirm, done and failed copy`);
  if (s.link && !/^https:\/\//.test(s.link.url)) errors.push(`Section ${s.id}: link must be https`);
}
if (html.includes("\u2014")) errors.push("Em dash found; use a plain dash");
if (errors.length) { console.error(errors.join("\n")); process.exit(1); }
console.log(`About manifest OK: ${manifest.sections.length} sections, renderers: ${renderers.join(", ")}.`);
