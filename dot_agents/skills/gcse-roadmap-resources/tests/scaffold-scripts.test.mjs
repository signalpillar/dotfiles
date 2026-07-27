import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { main as topicMain, findManifestTopic, renderTopicDraft, scaffoldTopic } from "../scripts/scaffold-topic.mjs";
import { main as paperMain, renderPaperDraft, scaffoldPaper } from "../scripts/scaffold-paper.mjs";
import { validatePaperGraphData, validateResourceLibrary } from "../../../../prototype.tests/gcse-science-f1-roadmap/tests/validate-resources.mjs";

const skillRoot = path.resolve(import.meta.dirname, "..");
const manifest = {
  nodes: [
    { id: "M1", subject: "maths", title: "Standard form" },
    { id: "P1", subject: "physics", title: "Units" }
  ]
};
const html = `<script type="application/json" id="manifest">\n${JSON.stringify(manifest)}\n</script>\n`;

function temporaryFixture() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "gcse-roadmap-skill-"));
  const htmlFile = path.join(root, "roadmap.html");
  const resourceRoot = path.join(root, "resources");
  fs.mkdirSync(resourceRoot);
  fs.writeFileSync(htmlFile, html);
  fs.writeFileSync(path.join(resourceRoot, "catalog.json"), `${JSON.stringify({ schemaVersion: 1, topics: {} }, null, 2)}\n`);
  fs.writeFileSync(path.join(resourceRoot, "papers.json"), `${JSON.stringify({ schemaVersion: 1, nodes: [], edges: [] }, null, 2)}\n`);
  return { root, htmlFile, resourceRoot };
}

function noTemporarySiblings(parent) {
  assert.equal(fs.readdirSync(parent).some((name) => name.includes(".tmp-")), false);
}

function completeTopicDraft(draftDirectory, resourceRoot, topicId = "M1", subject = "maths") {
  const record = JSON.parse(fs.readFileSync(path.join(draftDirectory, "topic.json"), "utf8"));
  delete record.draft;
  record.links = [
    { label: "Lesson", url: "https://example.org/lesson", publisher: "Example", kind: "lesson", note: "A focused lesson." },
    { label: "Practice", url: "https://example.org/practice", publisher: "Example", kind: "practice", note: "Focused practice." }
  ];
  record.wikipedia = { label: "Scientific notation", url: "https://en.wikipedia.org/wiki/Scientific_notation" };
  const destination = path.join(resourceRoot, "topics", subject, topicId.toLowerCase());
  fs.mkdirSync(destination, { recursive: true });
  fs.writeFileSync(path.join(destination, "topic.json"), `${JSON.stringify(record, null, 2)}\n`);
  fs.writeFileSync(path.join(destination, "summary.md"), "## Synthetic summary\n\nReviewed synthetic test content.\n");
  fs.writeFileSync(path.join(destination, "explain-prompt.md"), "# Synthetic tutor\n\nAsk a question and wait for the answer.\n");
  fs.writeFileSync(path.join(resourceRoot, "catalog.json"), `${JSON.stringify({ schemaVersion: 1, topics: { [topicId]: `topics/${subject}/${topicId.toLowerCase()}/topic.json` } }, null, 2)}\n`);
}

test("manifest lookup derives lowercase paths for multiple subjects", () => {
  assert.deepEqual(findManifestTopic(html, "M1"), { topicId: "M1", subject: "maths", directory: "m1" });
  assert.deepEqual(findManifestTopic(html, "P1"), { topicId: "P1", subject: "physics", directory: "p1" });
});

test("topic rendering is deterministic and marks evidence-only drafts", () => {
  assert.deepEqual(renderTopicDraft("M1"), renderTopicDraft("M1"));
  const draft = JSON.parse(renderTopicDraft("M1")["topic.json"]);
  assert.equal(draft.draft, true);
  assert.match(draft.links[0].url, /EVIDENCE_REQUIRED/);
});

test("topic scaffolding is deterministic and leaves source data unchanged", (context) => {
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const catalogFile = path.join(fixture.resourceRoot, "catalog.json");
  const before = fs.readFileSync(catalogFile);
  const first = path.join(fixture.root, "draft-m1-a");
  const second = path.join(fixture.root, "draft-m1-b");
  const result = scaffoldTopic({ ...fixture, topicId: "M1", output: first });
  scaffoldTopic({ ...fixture, topicId: "M1", output: second });
  assert.equal(result.recordPath, "topics/maths/m1/topic.json");
  for (const name of ["topic.json", "summary.md", "explain-prompt.md"]) {
    assert.deepEqual(fs.readFileSync(path.join(first, name)), fs.readFileSync(path.join(second, name)));
  }
  assert.deepEqual(fs.readFileSync(catalogFile), before);
  noTemporarySiblings(fixture.root);
});

test("topic scaffolding rejects unsafe preconditions without partial output", (context) => {
  assert.throws(() => topicMain([]), /Missing required argument --html/);
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const output = path.join(fixture.root, "draft");
  assert.throws(() => scaffoldTopic({ ...fixture, topicId: "X1", output }), /exactly one topic/);
  assert.equal(fs.existsSync(output), false);

  fs.writeFileSync(path.join(fixture.resourceRoot, "catalog.json"), "not json\n");
  assert.throws(() => scaffoldTopic({ ...fixture, topicId: "M1", output }), /Catalog/);
  assert.equal(fs.existsSync(output), false);
  noTemporarySiblings(fixture.root);
});

test("topic write failures clean their partial output", (context) => {
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const output = path.join(fixture.root, "draft");
  const originalWrite = fs.writeFileSync;
  let writes = 0;
  fs.writeFileSync = (...args) => {
    writes += 1;
    if (writes === 2) throw new Error("synthetic write failure");
    return originalWrite(...args);
  };
  try {
    assert.throws(() => scaffoldTopic({ ...fixture, topicId: "M1", output }), /synthetic write failure/);
  } finally {
    fs.writeFileSync = originalWrite;
  }
  assert.equal(fs.existsSync(output), false);
  noTemporarySiblings(fixture.root);
});

test("topic scaffolding rejects catalog, destination, output, and malformed manifest conflicts", (context) => {
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const output = path.join(fixture.root, "draft");
  fs.writeFileSync(path.join(fixture.resourceRoot, "catalog.json"), JSON.stringify({ schemaVersion: 1, topics: { M1: "topics/maths/m1/topic.json" } }));
  assert.throws(() => scaffoldTopic({ ...fixture, topicId: "M1", output }), /already cataloged/);

  fs.writeFileSync(path.join(fixture.resourceRoot, "catalog.json"), JSON.stringify({ schemaVersion: 1, topics: {} }));
  fs.mkdirSync(path.join(fixture.resourceRoot, "topics/maths/m1"), { recursive: true });
  assert.throws(() => scaffoldTopic({ ...fixture, topicId: "M1", output }), /destination already exists/);
  fs.rmSync(path.join(fixture.resourceRoot, "topics"), { recursive: true });

  fs.mkdirSync(output);
  assert.throws(() => scaffoldTopic({ ...fixture, topicId: "M1", output }), /Output already exists/);
  fs.rmSync(output, { recursive: true });
  fs.writeFileSync(fixture.htmlFile, "<script type=\"application/json\" id=\"manifest\">bad</script>");
  assert.throws(() => scaffoldTopic({ ...fixture, topicId: "M1", output }), /Manifest/);
  noTemporarySiblings(fixture.root);
});

test("a reviewed synthetic topic draft passes the real library validator", (context) => {
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const draft = path.join(fixture.root, "draft");
  scaffoldTopic({ ...fixture, topicId: "M1", output: draft });
  completeTopicDraft(draft, fixture.resourceRoot);
  assert.deepEqual(validateResourceLibrary(fixture.htmlFile, fixture.resourceRoot), { topics: 1, papers: 0, paperEdges: 0 });
});

test("paper rendering is deterministic, preserves its ID, and omits pages", () => {
  const paperId = "paper:board-series-p1";
  assert.deepEqual(renderPaperDraft(paperId), renderPaperDraft(paperId));
  const draft = renderPaperDraft(paperId);
  assert.equal(draft.draft, true);
  assert.equal(draft.node.id, paperId);
  assert.equal(draft.edge.from, paperId);
  assert.equal("pages" in draft.edge, false);
});

test("paper scaffolding is deterministic and leaves the graph unchanged", (context) => {
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const graphFile = path.join(fixture.resourceRoot, "papers.json");
  const before = fs.readFileSync(graphFile);
  const first = path.join(fixture.root, "paper-a.json");
  const second = path.join(fixture.root, "paper-b.json");
  scaffoldPaper({ ...fixture, paperId: "paper:board-series-p1", output: first });
  scaffoldPaper({ ...fixture, paperId: "paper:board-series-p1", output: second });
  assert.deepEqual(fs.readFileSync(first), fs.readFileSync(second));
  assert.deepEqual(fs.readFileSync(graphFile), before);
  noTemporarySiblings(fixture.root);
});

test("paper scaffolding rejects malformed IDs, graphs, duplicates, and outputs", (context) => {
  assert.throws(() => paperMain([]), /Missing required argument --html/);
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const output = path.join(fixture.root, "paper.json");
  assert.throws(() => scaffoldPaper({ ...fixture, paperId: "Paper:UPPER", output }), /lowercase/);
  fs.writeFileSync(path.join(fixture.resourceRoot, "papers.json"), "bad json\n");
  assert.throws(() => scaffoldPaper({ ...fixture, paperId: "paper:valid", output }), /Paper graph/);
  assert.equal(fs.existsSync(output), false);

  fs.writeFileSync(path.join(fixture.resourceRoot, "papers.json"), JSON.stringify({ schemaVersion: 1, nodes: [{ id: "paper:valid" }], edges: [] }));
  assert.throws(() => scaffoldPaper({ ...fixture, paperId: "paper:valid", output }), /already exists/);
  fs.writeFileSync(path.join(fixture.resourceRoot, "papers.json"), JSON.stringify({ schemaVersion: 1, nodes: [], edges: [] }));
  fs.writeFileSync(output, "authored\n");
  assert.throws(() => scaffoldPaper({ ...fixture, paperId: "paper:valid", output }), /Output already exists/);
  assert.equal(fs.readFileSync(output, "utf8"), "authored\n");
  noTemporarySiblings(fixture.root);
});

test("paper write failures leave no output or temporary file", (context) => {
  const fixture = temporaryFixture();
  context.after(() => fs.rmSync(fixture.root, { recursive: true, force: true }));
  const output = path.join(fixture.root, "paper.json");
  const originalLink = fs.linkSync;
  fs.linkSync = () => { throw new Error("synthetic publish failure"); };
  try {
    assert.throws(() => scaffoldPaper({ ...fixture, paperId: "paper:valid", output }), /synthetic publish failure/);
  } finally {
    fs.linkSync = originalLink;
  }
  assert.equal(fs.existsSync(output), false);
  noTemporarySiblings(fixture.root);
});

test("a reviewed synthetic paper draft passes the real graph validator without prerequisites", () => {
  const paperId = "paper:board-series-p1";
  const draft = renderPaperDraft(paperId);
  const graph = {
    schemaVersion: 1,
    nodes: [{
      ...draft.node,
      board: "Board",
      qualification: "GCSE Science",
      series: "June 2026",
      component: "Paper 1",
      tier: "Higher",
      urls: { questionPaper: "https://example.org/questions.pdf", markScheme: "https://example.org/marks.pdf" }
    }],
    edges: [{ ...draft.edge, to: "topic:M1", questions: ["01.1"], note: "Tests synthetic topic knowledge." }]
  };
  const validated = validatePaperGraphData(graph, new Set(["M1"]));
  assert.equal(validated.edges[0].from, paperId);
  assert.equal("requires" in validated.edges[0], false);
});

test("skill entry point follows structural conventions", () => {
  const skillFile = path.join(skillRoot, "SKILL.md");
  const source = fs.readFileSync(skillFile, "utf8");
  assert.ok(source.split("\n").length - 1 < 100, "SKILL.md must remain below 100 physical lines");
  assert.match(source, /^---\nname: gcse-roadmap-resources\n/);
  const links = [...source.matchAll(/\[[^\]]+\]\(([^)]+)\)/g)].map((match) => match[1]);
  assert.deepEqual(links, ["REFERENCE.md"]);
  assert.equal(fs.existsSync(path.join(skillRoot, links[0])), true);
});
