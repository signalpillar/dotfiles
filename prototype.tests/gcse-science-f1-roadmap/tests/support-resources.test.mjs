import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import {
  safeRelativePath,
  validateCatalogData,
  validateMarkdownSource,
  validatePaperGraphData,
  validateResourceLibrary,
  validateTopicData
} from "./validate-resources.mjs";

const workspace = path.resolve(import.meta.dirname, "../../..");
const htmlFile = path.join(workspace, "prototypes/gcse-science-f1-roadmap.html");
const resourceRoot = path.join(workspace, "prototypes/gcse-science-f1-roadmap");
const known = new Map([["M1", "maths"], ["P14", "physics"]]);
const knownIds = new Set(known.keys());
const validTopic = {
  schemaVersion: 1,
  topicId: "M1",
  summary: "summary.md",
  explainPrompt: "explain-prompt.md",
  links: [
    { label: "One", url: "https://example.com/one", publisher: "Example", kind: "lesson", note: "Useful lesson." },
    { label: "Two", url: "https://example.com/two", publisher: "Example", kind: "practice", note: "Useful practice." }
  ],
  wikipedia: { label: "Scientific notation", url: "https://en.wikipedia.org/wiki/Scientific_notation" }
};
const validPaper = {
  schemaVersion: 1,
  nodes: [{
    id: "paper:aqa-8463-2025-june-p1h", type: "past-paper", board: "AQA", qualification: "8463",
    series: "June 2025", component: "Paper 1", tier: "Higher",
    urls: { questionPaper: "https://example.org/questions.pdf", markScheme: "https://example.org/marks.pdf" }
  }],
  edges: [{ from: "paper:aqa-8463-2025-june-p1h", to: "topic:P14", questions: ["03.1", "03.2"], pages: [12, 13], note: "Calculation and method evaluation." }]
};

const clone = (value) => structuredClone(value);

test("the shipped resource library validates", () => {
  const catalog = JSON.parse(fs.readFileSync(path.join(resourceRoot, "catalog.json"), "utf8"));
  assert.deepEqual(validateResourceLibrary(htmlFile, resourceRoot), { topics: Object.keys(catalog.topics).length, papers: 0, paperEdges: 0 });
});

test("local paths reject traversal, absolute paths, queries, and fragments", () => {
  for (const value of ["../topic.json", "/topic.json", "topic.json?x=1", "topic.json#x", "topics\\m1.json"]) {
    assert.throws(() => safeRelativePath(value, "path"), /unsafe relative path/);
  }
});

test("catalog paths must match each known topic subject and ID", () => {
  assert.throws(() => validateCatalogData({ schemaVersion: 1, topics: { X1: "topics/maths/x1/topic.json" } }, known), /unknown topic/);
  assert.throws(() => validateCatalogData({ schemaVersion: 1, topics: { M1: "topics/Maths/m1/topic.json" } }, known), /path must be/);
  assert.throws(() => validateCatalogData({ schemaVersion: 1, topics: { M1: "topics/biology/m1/topic.json" } }, known), /path must be/);
});

test("topic IDs agree and supporting links remain separate from Wikipedia", () => {
  const mismatch = clone(validTopic);
  mismatch.topicId = "P14";
  assert.throws(() => validateTopicData(mismatch, "M1"), /mismatch/);
  const tooFew = clone(validTopic);
  tooFew.links.pop();
  assert.throws(() => validateTopicData(tooFew, "M1"), /at least two/);
  const duplicateWiki = clone(validTopic);
  duplicateWiki.links[0].url = duplicateWiki.wikipedia.url;
  assert.throws(() => validateTopicData(duplicateWiki, "M1"), /separate/);
});

test("topic records reject unsupported kinds and insecure links", () => {
  const badKind = clone(validTopic);
  badKind.links[0].kind = "blog";
  assert.throws(() => validateTopicData(badKind, "M1"), /unsupported kind/);
  const insecure = clone(validTopic);
  insecure.links[0].url = "http://example.com";
  assert.throws(() => validateTopicData(insecure, "M1"), /HTTPS/);
});

test("Markdown rejects raw HTML, unsafe links, and unsupported constructs", () => {
  assert.equal(validateMarkdownSource("## Safe\n\nUse **practice** and [read](https://example.org)."), "## Safe\n\nUse **practice** and [read](https://example.org).");
  assert.throws(() => validateMarkdownSource("<script>alert(1)</script>"), /raw HTML/);
  assert.throws(() => validateMarkdownSource("[bad](javascript:alert(1))"), /HTTPS/);
  assert.throws(() => validateMarkdownSource("![image](https://example.org/a.png)"), /unsupported/);
  assert.throws(() => validateMarkdownSource("#### Too deep"), /unsupported/);
  assert.throws(() => validateMarkdownSource("---"), /unsupported/);
  assert.throws(() => validateMarkdownSource("~~removed~~"), /unsupported/);
});

test("empty and question-mapped paper graphs validate without prerequisite output", () => {
  const empty = validatePaperGraphData({ schemaVersion: 1, nodes: [], edges: [] }, knownIds);
  assert.deepEqual(Object.keys(empty).sort(), ["edges", "nodes", "schemaVersion"]);
  const mapped = validatePaperGraphData(validPaper, knownIds);
  assert.deepEqual(mapped.edges[0].to, "topic:P14");
  assert.equal("requires" in mapped.edges[0], false);
});

test("paper graph rejects unknown endpoints and malformed question mappings", () => {
  const endpoint = clone(validPaper);
  endpoint.edges[0].to = "topic:X1";
  assert.throws(() => validatePaperGraphData(endpoint, knownIds), /unknown topic endpoint/);
  const questions = clone(validPaper);
  questions.edges[0].questions = ["03.1", "03.1"];
  assert.throws(() => validatePaperGraphData(questions, knownIds), /must be unique/);
  const pages = clone(validPaper);
  pages.edges[0].pages = [0];
  assert.throws(() => validatePaperGraphData(pages, knownIds), /positive integers/);
  const duplicate = clone(validPaper);
  duplicate.edges.push({ ...clone(duplicate.edges[0]), questions: ["04.1"] });
  assert.throws(() => validatePaperGraphData(duplicate, knownIds), /duplicate edge mapping/);
});

test("library validation reports missing files", () => {
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), "roadmap-resources-"));
  try {
    fs.writeFileSync(path.join(temporary, "catalog.json"), JSON.stringify({ schemaVersion: 1, topics: { M1: "topics/maths/m1/topic.json" } }));
    fs.writeFileSync(path.join(temporary, "papers.json"), JSON.stringify({ schemaVersion: 1, nodes: [], edges: [] }));
    assert.throws(() => validateResourceLibrary(htmlFile, temporary), /missing topics\/maths\/m1\/topic.json/);
  } finally {
    fs.rmSync(temporary, { recursive: true, force: true });
  }
});
