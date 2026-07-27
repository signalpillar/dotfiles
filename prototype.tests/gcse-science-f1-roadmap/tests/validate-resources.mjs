import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const LINK_KINDS = new Set(["official-specification", "lesson", "video", "interactive", "practice", "reference"]);
const TOPIC_KEYS = ["explainPrompt", "links", "schemaVersion", "summary", "topicId", "wikipedia"];
const LINK_KEYS = ["kind", "label", "note", "publisher", "url"];
const PAPER_KEYS = ["board", "component", "id", "qualification", "series", "tier", "type", "urls"];
const EDGE_KEYS = ["from", "note", "pages", "questions", "to"];
const MAX_MARKDOWN_BYTES = 100000;

function exactKeys(value, keys, label) {
  const actual = Object.keys(value || {}).sort();
  const expected = [...keys].sort();
  if (actual.join("\0") !== expected.join("\0")) throw new Error(`${label}: expected keys ${expected.join(", ")}`);
}

function requiredText(value, label) {
  if (typeof value !== "string" || !value.trim()) throw new Error(`${label}: must be a non-empty string`);
}

function httpsUrl(value, label) {
  requiredText(value, label);
  let parsed;
  try { parsed = new URL(value); } catch { throw new Error(`${label}: must be an absolute URL`); }
  if (parsed.protocol !== "https:") throw new Error(`${label}: must use HTTPS`);
}

export function safeRelativePath(value, label) {
  requiredText(value, label);
  if (value.includes("\\") || value.startsWith("/") || value.includes("?") || value.includes("#")) throw new Error(`${label}: unsafe relative path`);
  const parts = value.split("/");
  if (parts.some((part) => !part || part === "." || part === "..")) throw new Error(`${label}: unsafe relative path`);
  return value;
}

export function validateMarkdownSource(source, label = "Markdown") {
  requiredText(source, label);
  if (Buffer.byteLength(source) > MAX_MARKDOWN_BYTES) throw new Error(`${label}: exceeds ${MAX_MARKDOWN_BYTES} bytes`);
  if (/<\/?[A-Za-z][^>]*>/.test(source)) throw new Error(`${label}: raw HTML is not allowed`);
  if (/^\s*>/m.test(source) || /^\s*\|.*\|\s*$/m.test(source) || /!\[[^\]]*\]\(/.test(source) || /^\s*- \[[ xX]\]/m.test(source) || /^#{4,}\s/m.test(source) || /^\s*(?:-{3,}|_{3,}|\*{3,})\s*$/m.test(source) || /~~/.test(source)) {
    throw new Error(`${label}: unsupported Markdown construct`);
  }
  for (const match of source.matchAll(/\[[^\]]+\]\(([^)\s]+)(?:\s+"[^"]*")?\)/g)) httpsUrl(match[1], `${label} link`);
  return source;
}

export function validateCatalogData(catalog, knownTopics) {
  exactKeys(catalog, ["schemaVersion", "topics"], "Catalog");
  if (catalog.schemaVersion !== 1) throw new Error("Catalog: schemaVersion must be 1");
  if (!catalog.topics || typeof catalog.topics !== "object" || Array.isArray(catalog.topics)) throw new Error("Catalog: topics must be an object");
  for (const [topicId, recordPath] of Object.entries(catalog.topics)) {
    const subject = knownTopics.get(topicId);
    if (!subject) throw new Error(`Catalog: unknown topic ${topicId}`);
    safeRelativePath(recordPath, `Catalog ${topicId}`);
    const expectedPath = `topics/${subject}/${topicId.toLowerCase()}/topic.json`;
    if (recordPath !== expectedPath) throw new Error(`Catalog ${topicId}: path must be ${expectedPath}`);
  }
  return catalog;
}

export function validateTopicData(topic, expectedTopicId) {
  exactKeys(topic, TOPIC_KEYS, `Topic ${expectedTopicId}`);
  if (topic.schemaVersion !== 1) throw new Error(`Topic ${expectedTopicId}: schemaVersion must be 1`);
  requiredText(topic.topicId, `Topic ${expectedTopicId} topicId`);
  if (topic.topicId !== expectedTopicId) throw new Error(`Topic ${expectedTopicId}: topicId mismatch`);
  safeRelativePath(topic.summary, `Topic ${expectedTopicId} summary`);
  safeRelativePath(topic.explainPrompt, `Topic ${expectedTopicId} explainPrompt`);
  if (topic.summary.includes("/") || topic.explainPrompt.includes("/")) throw new Error(`Topic ${expectedTopicId}: Markdown files must be beside topic.json`);
  if (!Array.isArray(topic.links) || topic.links.length < 2) throw new Error(`Topic ${expectedTopicId}: at least two supporting links are required`);
  const urls = new Set();
  topic.links.forEach((link, index) => {
    exactKeys(link, LINK_KEYS, `Topic ${expectedTopicId} link ${index}`);
    for (const field of ["label", "publisher", "note"]) requiredText(link[field], `Topic ${expectedTopicId} link ${index} ${field}`);
    if (!LINK_KINDS.has(link.kind)) throw new Error(`Topic ${expectedTopicId} link ${index}: unsupported kind ${link.kind}`);
    httpsUrl(link.url, `Topic ${expectedTopicId} link ${index} URL`);
    if (urls.has(link.url)) throw new Error(`Topic ${expectedTopicId}: duplicate URL ${link.url}`);
    urls.add(link.url);
  });
  exactKeys(topic.wikipedia, ["label", "url"], `Topic ${expectedTopicId} Wikipedia`);
  requiredText(topic.wikipedia.label, `Topic ${expectedTopicId} Wikipedia label`);
  httpsUrl(topic.wikipedia.url, `Topic ${expectedTopicId} Wikipedia URL`);
  if (new URL(topic.wikipedia.url).hostname !== "en.wikipedia.org") throw new Error(`Topic ${expectedTopicId}: Wikipedia URL must use en.wikipedia.org`);
  if (urls.has(topic.wikipedia.url)) throw new Error(`Topic ${expectedTopicId}: Wikipedia must be separate from supporting links`);
  return topic;
}

export function validatePaperGraphData(graph, knownTopicIds) {
  exactKeys(graph, ["edges", "nodes", "schemaVersion"], "Paper graph");
  if (graph.schemaVersion !== 1) throw new Error("Paper graph: schemaVersion must be 1");
  if (!Array.isArray(graph.nodes) || !Array.isArray(graph.edges)) throw new Error("Paper graph: nodes and edges must be arrays");
  const nodeIds = new Set();
  for (const node of graph.nodes) {
    exactKeys(node, PAPER_KEYS, `Paper ${node.id || "unknown"}`);
    for (const field of ["id", "board", "qualification", "series", "component", "tier"]) requiredText(node[field], `Paper ${field}`);
    if (!node.id.startsWith("paper:")) throw new Error(`Paper ${node.id}: id must start with paper:`);
    if (node.type !== "past-paper") throw new Error(`Paper ${node.id}: type must be past-paper`);
    if (nodeIds.has(node.id)) throw new Error(`Paper graph: duplicate node ${node.id}`);
    nodeIds.add(node.id);
    exactKeys(node.urls, ["markScheme", "questionPaper"], `Paper ${node.id} URLs`);
    httpsUrl(node.urls.questionPaper, `Paper ${node.id} question paper`);
    httpsUrl(node.urls.markScheme, `Paper ${node.id} mark scheme`);
  }
  const edgeKeys = new Set();
  for (const edge of graph.edges) {
    const expectedKeys = edge.pages === undefined ? EDGE_KEYS.filter((key) => key !== "pages") : EDGE_KEYS;
    exactKeys(edge, expectedKeys, "Paper edge");
    if (!nodeIds.has(edge.from)) throw new Error(`Paper edge: unknown source ${edge.from}`);
    if (typeof edge.to !== "string" || !edge.to.startsWith("topic:") || !knownTopicIds.has(edge.to.slice(6))) throw new Error(`Paper edge: unknown topic endpoint ${edge.to}`);
    if (!Array.isArray(edge.questions) || !edge.questions.length || edge.questions.some((question) => typeof question !== "string" || !question.trim())) throw new Error("Paper edge: non-empty question numbers are required");
    if (new Set(edge.questions).size !== edge.questions.length) throw new Error("Paper edge: question numbers must be unique");
    if (edge.pages !== undefined && (!Array.isArray(edge.pages) || !edge.pages.length || edge.pages.some((page) => !Number.isInteger(page) || page < 1))) throw new Error("Paper edge: pages must be positive integers");
    requiredText(edge.note, "Paper edge note");
    if (edge.note.length > 240) throw new Error("Paper edge: note is too long");
    const key = `${edge.from}\0${edge.to}`;
    if (edgeKeys.has(key)) throw new Error("Paper graph: duplicate edge mapping");
    edgeKeys.add(key);
  }
  return graph;
}

function readJson(file, label) {
  try { return JSON.parse(fs.readFileSync(file, "utf8")); } catch (error) { throw new Error(`${label}: ${error.message}`); }
}

function manifestTopics(htmlFile) {
  const html = fs.readFileSync(htmlFile, "utf8");
  const match = html.match(/<script type="application\/json" id="manifest">\s*([\s\S]*?)\s*<\/script>/);
  if (!match) throw new Error("Manifest script not found");
  return new Map(JSON.parse(match[1]).nodes.map((node) => [node.id, node.subject]));
}

function filesBelow(root) {
  if (!fs.existsSync(root)) return [];
  return fs.readdirSync(root, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(root, entry.name);
    return entry.isDirectory() ? filesBelow(full) : [full];
  });
}

export function validateResourceLibrary(htmlFile, resourceRoot) {
  resourceRoot = path.resolve(resourceRoot);
  const knownTopics = manifestTopics(htmlFile);
  const knownTopicIds = new Set(knownTopics.keys());
  const catalog = validateCatalogData(readJson(path.join(resourceRoot, "catalog.json"), "Catalog"), knownTopics);
  const expectedFiles = new Set();
  for (const [topicId, recordPath] of Object.entries(catalog.topics)) {
    const recordFile = path.resolve(resourceRoot, recordPath);
    if (!recordFile.startsWith(path.resolve(resourceRoot) + path.sep) || !fs.existsSync(recordFile)) throw new Error(`Topic ${topicId}: missing ${recordPath}`);
    const topic = validateTopicData(readJson(recordFile, `Topic ${topicId}`), topicId);
    expectedFiles.add(recordFile);
    for (const [field, relative] of [["summary", topic.summary], ["explainPrompt", topic.explainPrompt]]) {
      const markdownFile = path.resolve(path.dirname(recordFile), relative);
      if (!markdownFile.startsWith(path.resolve(resourceRoot) + path.sep) || !fs.existsSync(markdownFile)) throw new Error(`Topic ${topicId}: missing ${field} file`);
      validateMarkdownSource(fs.readFileSync(markdownFile, "utf8"), `Topic ${topicId} ${field}`);
      expectedFiles.add(markdownFile);
    }
  }
  const publishedFiles = filesBelow(path.join(resourceRoot, "topics"));
  for (const file of publishedFiles) if (!expectedFiles.has(file)) throw new Error(`Orphan topic resource: ${path.relative(resourceRoot, file)}`);
  const papers = validatePaperGraphData(readJson(path.join(resourceRoot, "papers.json"), "Paper graph"), knownTopicIds);
  return { topics: Object.keys(catalog.topics).length, papers: papers.nodes.length, paperEdges: papers.edges.length };
}

const isCli = process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isCli) {
  const [htmlFile, resourceRoot] = process.argv.slice(2);
  if (!htmlFile || !resourceRoot) throw new Error("Usage: node validate-resources.mjs <html-file> <resource-root>");
  console.log(JSON.stringify(validateResourceLibrary(htmlFile, resourceRoot), null, 2));
}
