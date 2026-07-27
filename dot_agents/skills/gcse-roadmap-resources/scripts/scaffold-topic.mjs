import fs from "node:fs";
import path from "node:path";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";

const REQUIRED_OPTIONS = ["html", "resources", "topic", "output"];

function parseJsonFile(file, label) {
  let value;
  try {
    value = JSON.parse(fs.readFileSync(file, "utf8"));
  } catch (error) {
    throw new Error(`${label}: ${error.message}`);
  }
  return value;
}

export function findManifestTopic(html, topicId) {
  const match = html.match(/<script type="application\/json" id="manifest">\s*([\s\S]*?)\s*<\/script>/);
  if (!match) throw new Error("Manifest: script not found");
  let manifest;
  try {
    manifest = JSON.parse(match[1]);
  } catch (error) {
    throw new Error(`Manifest: ${error.message}`);
  }
  if (!manifest || !Array.isArray(manifest.nodes)) throw new Error("Manifest: nodes must be an array");
  const matches = manifest.nodes.filter((node) => node && node.id === topicId);
  if (matches.length !== 1) throw new Error(`Manifest: expected exactly one topic ${topicId}, found ${matches.length}`);
  const topic = matches[0];
  if (typeof topic.subject !== "string" || !/^[A-Za-z0-9-]+$/.test(topic.subject)) throw new Error(`Manifest topic ${topicId}: invalid subject`);
  return { topicId, subject: topic.subject.toLowerCase(), directory: topicId.toLowerCase() };
}

export function renderTopicDraft(topicId) {
  const record = {
    draft: true,
    schemaVersion: 1,
    topicId,
    summary: "summary.md",
    explainPrompt: "explain-prompt.md",
    links: [
      {
        label: "EVIDENCE_REQUIRED_RESOURCE_LABEL_1",
        url: "EVIDENCE_REQUIRED_HTTPS_URL_1",
        publisher: "EVIDENCE_REQUIRED_PUBLISHER_1",
        kind: "EVIDENCE_REQUIRED_ALLOWED_LINK_KIND_1",
        note: "EVIDENCE_REQUIRED_RELEVANCE_AND_ACCESS_NOTE_1"
      },
      {
        label: "EVIDENCE_REQUIRED_RESOURCE_LABEL_2",
        url: "EVIDENCE_REQUIRED_HTTPS_URL_2",
        publisher: "EVIDENCE_REQUIRED_PUBLISHER_2",
        kind: "EVIDENCE_REQUIRED_ALLOWED_LINK_KIND_2",
        note: "EVIDENCE_REQUIRED_RELEVANCE_AND_ACCESS_NOTE_2"
      }
    ],
    wikipedia: {
      label: "EVIDENCE_REQUIRED_ENGLISH_WIKIPEDIA_LABEL",
      url: "EVIDENCE_REQUIRED_ENGLISH_WIKIPEDIA_HTTPS_URL"
    }
  };
  return {
    "topic.json": `${JSON.stringify(record, null, 2)}\n`,
    "summary.md": "# EVIDENCE_REQUIRED_TOPIC_TITLE\n\nEVIDENCE_REQUIRED_REVIEWED_GCSE_SUMMARY\n",
    "explain-prompt.md": "# Tutor me on EVIDENCE_REQUIRED_TOPIC_TITLE\n\nEVIDENCE_REQUIRED_INTERACTIVE_TUTOR_PROMPT_USING_ONLY_MANIFEST_EXAMPLES\n"
  };
}

function parseArgs(argv) {
  const options = {};
  for (let index = 0; index < argv.length; index += 2) {
    const key = argv[index];
    const value = argv[index + 1];
    if (!key?.startsWith("--") || !value) throw new Error("Arguments must be --name value pairs");
    options[key.slice(2)] = value;
  }
  for (const key of REQUIRED_OPTIONS) if (!options[key]) throw new Error(`Missing required argument --${key}`);
  return options;
}

function readCatalog(resourceRoot) {
  const catalog = parseJsonFile(path.join(resourceRoot, "catalog.json"), "Catalog");
  if (!catalog || Object.keys(catalog).sort().join(",") !== "schemaVersion,topics" || catalog.schemaVersion !== 1 || !catalog.topics || typeof catalog.topics !== "object" || Array.isArray(catalog.topics)) {
    throw new Error("Catalog: expected schemaVersion 1 and a topics object");
  }
  for (const [topicId, recordPath] of Object.entries(catalog.topics)) {
    if (typeof topicId !== "string" || !topicId || typeof recordPath !== "string" || !recordPath) throw new Error("Catalog: topic mappings must be non-empty strings");
  }
  return catalog;
}

export function scaffoldTopic({ htmlFile, resourceRoot, topicId, output }) {
  const html = fs.readFileSync(htmlFile, "utf8");
  const topic = findManifestTopic(html, topicId);
  const catalog = readCatalog(resourceRoot);
  const recordPath = `topics/${topic.subject}/${topic.directory}/topic.json`;
  const destination = path.join(resourceRoot, "topics", topic.subject, topic.directory);
  const outputPath = path.resolve(output);
  if (Object.hasOwn(catalog.topics, topicId)) throw new Error(`Topic ${topicId} is already cataloged`);
  if (fs.existsSync(destination)) throw new Error(`Published destination already exists: ${destination}`);
  if (fs.existsSync(outputPath)) throw new Error(`Output already exists: ${outputPath}`);

  const files = renderTopicDraft(topicId);
  const parent = path.dirname(outputPath);
  fs.mkdirSync(parent, { recursive: true });
  const temporary = path.join(parent, `.${path.basename(outputPath)}.tmp-${process.pid}-${randomUUID()}`);
  try {
    fs.mkdirSync(temporary);
    for (const [name, contents] of Object.entries(files)) fs.writeFileSync(path.join(temporary, name), contents, { flag: "wx" });
    fs.renameSync(temporary, outputPath);
  } catch (error) {
    fs.rmSync(temporary, { recursive: true, force: true });
    throw error;
  }
  return { ...topic, output: outputPath, recordPath, destination };
}

export function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  const result = scaffoldTopic({
    htmlFile: path.resolve(options.html),
    resourceRoot: path.resolve(options.resources),
    topicId: options.topic,
    output: options.output
  });
  console.log(`Draft created: ${result.output}`);
  console.log(`Catalog mapping: "${result.topicId}": "${result.recordPath}"`);
  console.log(`Published destination: ${result.destination}`);
  return result;
}

const isCli = process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isCli) main();
