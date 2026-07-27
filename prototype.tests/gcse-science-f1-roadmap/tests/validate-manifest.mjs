import fs from "node:fs";

const file = process.argv[2];
if (!file) throw new Error("Usage: node validate-manifest.mjs <html-file>");

const html = fs.readFileSync(file, "utf8");
const manifestMatch = html.match(/<script type="application\/json" id="manifest">\s*([\s\S]*?)\s*<\/script>/);
if (!manifestMatch) throw new Error("Manifest script not found");

const manifest = JSON.parse(manifestMatch[1]);
const errors = [];
const ids = new Set();
const byId = new Map();
const subjects = new Set(Object.keys(manifest.subjects));
const drivers = new Set(manifest.drivers.map((driver) => driver.id));
const stages = new Set(manifest.stages.map((stage) => stage.id));
const requiredText = ["title", "spec", "f1", "diy", "check"];

for (const node of manifest.nodes) {
  if (ids.has(node.id)) errors.push(`Duplicate node id: ${node.id}`);
  ids.add(node.id);
  byId.set(node.id, node);
  if (!subjects.has(node.subject)) errors.push(`${node.id}: unknown subject ${node.subject}`);
  if (!drivers.has(node.driver)) errors.push(`${node.id}: unknown driver ${node.driver}`);
  if (!stages.has(node.stage)) errors.push(`${node.id}: unknown stage ${node.stage}`);
  if (!Array.isArray(node.requires)) errors.push(`${node.id}: requires is not an array`);
  for (const field of requiredText) {
    if (typeof node[field] !== "string" || !node[field].trim()) errors.push(`${node.id}: empty ${field}`);
  }
}

for (const node of manifest.nodes) {
  for (const requiredId of node.requires || []) {
    const prerequisite = byId.get(requiredId);
    if (!prerequisite) errors.push(`${node.id}: missing prerequisite ${requiredId}`);
    else if (prerequisite.stage > node.stage) errors.push(`${node.id}: stage ${node.stage} requires later-stage ${requiredId} at ${prerequisite.stage}`);
  }
}

const indegree = new Map(manifest.nodes.map((node) => [node.id, node.requires.length]));
const unlocks = new Map(manifest.nodes.map((node) => [node.id, []]));
for (const node of manifest.nodes) {
  for (const requiredId of node.requires) if (unlocks.has(requiredId)) unlocks.get(requiredId).push(node.id);
}
const queue = manifest.nodes.filter((node) => indegree.get(node.id) === 0).map((node) => node.id);
const ordered = [];
while (queue.length) {
  const id = queue.shift();
  ordered.push(id);
  for (const next of unlocks.get(id)) {
    indegree.set(next, indegree.get(next) - 1);
    if (indegree.get(next) === 0) queue.push(next);
  }
}
if (ordered.length !== manifest.nodes.length) {
  const cyclic = manifest.nodes.filter((node) => indegree.get(node.id) > 0).map((node) => node.id);
  errors.push(`Cycle detected among: ${cyclic.join(", ")}`);
}

const pageNodeRefs = [];
for (const page of Object.values(manifest.pages)) {
  for (const section of page.sections) {
    for (const item of section.items || []) {
      for (const id of item.science || []) pageNodeRefs.push(id);
    }
  }
}
for (const id of pageNodeRefs) if (!byId.has(id)) errors.push(`Page references unknown node: ${id}`);

const rendererMatch = html.match(/const SECTION_RENDERERS = \{([\s\S]*?)\n\s*\};/);
if (!rendererMatch) errors.push("SECTION_RENDERERS map not found");
else {
  const rendererTypes = new Set([...rendererMatch[1].matchAll(/^\s{8}(\w+):/gm)].map((match) => match[1]));
  const sectionTypes = new Set(Object.values(manifest.pages).flatMap((page) => page.sections.map((section) => section.type)));
  for (const type of sectionTypes) if (!rendererTypes.has(type)) errors.push(`No renderer for section type: ${type}`);
}

if (html.includes("\u2014")) errors.push("File contains an em dash");

if (errors.length) {
  console.error(errors.join("\n"));
  process.exitCode = 1;
} else {
  const countBy = (field) => Object.fromEntries(
    [...new Set(manifest.nodes.map((node) => node[field]))]
      .sort()
      .map((value) => [value, manifest.nodes.filter((node) => node[field] === value).length])
  );
  console.log(JSON.stringify({
    nodes: manifest.nodes.length,
    edges: manifest.nodes.reduce((total, node) => total + node.requires.length, 0),
    subjects: countBy("subject"),
    stages: countBy("stage"),
    drivers: countBy("driver"),
    pageNodeReferences: pageNodeRefs.length,
    sectionTypes: [...new Set(Object.values(manifest.pages).flatMap((page) => page.sections.map((section) => section.type)))].sort()
  }, null, 2));
}
