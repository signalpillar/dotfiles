import fs from "node:fs";
import path from "node:path";
import yaml from "js-yaml";

const [htmlArg, yamlArg] = process.argv.slice(2);
if (!htmlArg || !yamlArg) throw new Error("Usage: node validate-model.mjs <html-file> <yaml-file>");

const htmlFile = path.resolve(htmlArg);
const yamlFile = path.resolve(yamlArg);

const html = fs.readFileSync(htmlFile, "utf8");
const manifestMatch = html.match(/<script type="application\/json" id="app-manifest">\s*([\s\S]*?)\s*<\/script>/);
if (!manifestMatch) throw new Error("Manifest script not found");
const manifest = JSON.parse(manifestMatch[1]);

const model = yaml.load(fs.readFileSync(yamlFile, "utf8"));
const errors = [];

const requiredLabels = [
  "eyebrow", "loading", "loadError", "chooseFile", "chooseHint", "stage", "previous", "next",
  "when", "artifacts", "overlays", "notes", "noNotes", "viaOverlay", "appliesTo", "close", "note", "notesPlural"
];
for (const label of requiredLabels) {
  if (typeof manifest.labels?.[label] !== "string" || !manifest.labels[label].trim()) {
    errors.push(`Manifest label missing or empty: ${label}`);
  }
}
if (!manifest.labels?.viaOverlay?.includes("{overlay}")) errors.push("viaOverlay label must contain the {overlay} placeholder");
if (manifest.dataFile !== path.basename(yamlFile)) errors.push(`Manifest dataFile ${manifest.dataFile} does not match ${path.basename(yamlFile)}`);
if (!["bottom", "start", "end", "left", "right"].includes(manifest.drawer?.side)) errors.push(`Unsupported drawer side: ${manifest.drawer?.side}`);
if (!manifest.timelineOrder?.includes(manifest.initialTimeline)) errors.push("initialTimeline is not part of timelineOrder");
if (typeof manifest.layout?.wideMinWidth !== "number" || manifest.layout.wideMinWidth < 320) {
  errors.push("layout.wideMinWidth must be a number >= 320");
}
if (manifest.layout?.compactDetail !== "inline-expand") {
  errors.push("layout.compactDetail must be inline-expand for this prototype");
}

const overlayIds = new Set((model.overlays || []).map((overlay) => overlay.id));
const stageKeys = new Set();
let directNotes = 0;

for (const timelineId of manifest.timelineOrder) {
  const timeline = model[timelineId];
  if (!timeline || !Array.isArray(timeline.stages) || !timeline.stages.length) {
    errors.push(`Timeline missing or empty: ${timelineId}`);
    continue;
  }
  if (!manifest.theme?.[timelineId]) errors.push(`No accent colour for timeline: ${timelineId}`);
  for (const stage of timeline.stages) {
    const key = `${timelineId}.${stage.id}`;
    if (stageKeys.has(key)) errors.push(`Duplicate stage: ${key}`);
    stageKeys.add(key);
    for (const field of ["id", "name", "summary", "happens_when"]) {
      if (typeof stage[field] !== "string" || !stage[field].trim()) errors.push(`${key}: empty ${field}`);
    }
    if (!Array.isArray(stage.key_artifacts) || !stage.key_artifacts.length) errors.push(`${key}: no key_artifacts`);
    if (!Array.isArray(stage.related_overlays)) errors.push(`${key}: related_overlays is not an array`);
    if (!Array.isArray(stage.notes)) errors.push(`${key}: notes is not an array`);
    for (const overlayId of stage.related_overlays || []) {
      if (!overlayIds.has(overlayId)) errors.push(`${key}: unknown overlay ${overlayId}`);
    }
    directNotes += (stage.notes || []).length;
  }
}

let overlayNotes = 0;
for (const overlay of model.overlays || []) {
  for (const field of ["id", "name", "summary"]) {
    if (typeof overlay[field] !== "string" || !overlay[field].trim()) errors.push(`overlay ${overlay.id}: empty ${field}`);
  }
  for (const target of overlay.applies_to || []) {
    if (!stageKeys.has(target)) errors.push(`overlay ${overlay.id}: applies_to does not resolve: ${target}`);
  }
  for (const note of overlay.notes || []) {
    if (typeof note.text !== "string" || !note.text.trim()) errors.push(`overlay ${overlay.id}: note without text`);
    overlayNotes += 1;
  }
}

// Every note must be reachable from a stage, otherwise the badge count and the
// detail panel can drift apart the way they did before the note-merge fix.
const reachableOverlayIds = new Set();
for (const timelineId of manifest.timelineOrder) {
  for (const stage of model[timelineId]?.stages || []) {
    for (const overlayId of stage.related_overlays || []) reachableOverlayIds.add(overlayId);
  }
}
for (const overlay of model.overlays || []) {
  if ((overlay.notes || []).length && !reachableOverlayIds.has(overlay.id)) {
    errors.push(`overlay ${overlay.id} carries notes but no stage links to it`);
  }
}

for (const [name, contents] of [[path.basename(htmlFile), html], [path.basename(yamlFile), fs.readFileSync(yamlFile, "utf8")]]) {
  if (contents.includes("\u2014")) errors.push(`${name} contains an em dash`);
}

if (errors.length) {
  console.error(errors.join("\n"));
  process.exitCode = 1;
} else {
  console.log(JSON.stringify({
    timelines: Object.fromEntries(manifest.timelineOrder.map((id) => [id, model[id].stages.length])),
    overlays: overlayIds.size,
    directNotes,
    overlayNotes
  }, null, 2));
}
