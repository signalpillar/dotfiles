import fs from "node:fs";
import path from "node:path";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";

const REQUIRED_OPTIONS = ["html", "resources", "paper-id", "output"];

function parseJsonFile(file, label) {
  try {
    return JSON.parse(fs.readFileSync(file, "utf8"));
  } catch (error) {
    throw new Error(`${label}: ${error.message}`);
  }
}

export function validatePaperId(paperId) {
  if (typeof paperId !== "string" || !/^paper:[a-z0-9]+(?:-[a-z0-9]+)*$/.test(paperId)) {
    throw new Error("Paper ID must be a lowercase paper: identifier");
  }
  return paperId;
}

export function renderPaperDraft(paperId) {
  validatePaperId(paperId);
  return {
    draft: true,
    node: {
      id: paperId,
      type: "past-paper",
      board: "EVIDENCE_REQUIRED_BOARD",
      qualification: "EVIDENCE_REQUIRED_QUALIFICATION",
      series: "EVIDENCE_REQUIRED_SERIES",
      component: "EVIDENCE_REQUIRED_COMPONENT",
      tier: "EVIDENCE_REQUIRED_TIER_OR_NOT_APPLICABLE",
      urls: {
        questionPaper: "EVIDENCE_REQUIRED_QUESTION_PAPER_HTTPS_URL",
        markScheme: "EVIDENCE_REQUIRED_MARK_SCHEME_HTTPS_URL"
      }
    },
    edge: {
      from: paperId,
      to: "topic:EVIDENCE_REQUIRED_TOPIC_ID",
      questions: ["EVIDENCE_REQUIRED_EXACT_QUESTION_NUMBER"],
      note: "EVIDENCE_REQUIRED_CONCISE_RELEVANCE_NOTE"
    }
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

function readPaperGraph(resourceRoot) {
  const graph = parseJsonFile(path.join(resourceRoot, "papers.json"), "Paper graph");
  if (!graph || Object.keys(graph).sort().join(",") !== "edges,nodes,schemaVersion" || graph.schemaVersion !== 1 || !Array.isArray(graph.nodes) || !Array.isArray(graph.edges)) {
    throw new Error("Paper graph: expected schemaVersion 1 with nodes and edges arrays");
  }
  const nodeIds = new Set();
  for (const node of graph.nodes) {
    if (!node || typeof node.id !== "string" || !node.id) throw new Error("Paper graph: every node must have a non-empty ID");
    if (nodeIds.has(node.id)) throw new Error(`Paper graph: duplicate node ${node.id}`);
    nodeIds.add(node.id);
  }
  for (const edge of graph.edges) if (!edge || typeof edge !== "object" || Array.isArray(edge)) throw new Error("Paper graph: every edge must be an object");
  return graph;
}

export function scaffoldPaper({ htmlFile, resourceRoot, paperId, output }) {
  validatePaperId(paperId);
  fs.readFileSync(htmlFile, "utf8");
  const graph = readPaperGraph(resourceRoot);
  const outputPath = path.resolve(output);
  if (graph.nodes.some((node) => node?.id === paperId)) throw new Error(`Paper ${paperId} already exists`);
  if (fs.existsSync(outputPath)) throw new Error(`Output already exists: ${outputPath}`);

  const contents = `${JSON.stringify(renderPaperDraft(paperId), null, 2)}\n`;
  const parent = path.dirname(outputPath);
  fs.mkdirSync(parent, { recursive: true });
  const temporary = path.join(parent, `.${path.basename(outputPath)}.tmp-${process.pid}-${randomUUID()}`);
  try {
    fs.writeFileSync(temporary, contents, { flag: "wx" });
    fs.linkSync(temporary, outputPath);
    fs.unlinkSync(temporary);
  } catch (error) {
    fs.rmSync(temporary, { force: true });
    throw error;
  }
  return { output: outputPath, paperId };
}

export function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  const result = scaffoldPaper({
    htmlFile: path.resolve(options.html),
    resourceRoot: path.resolve(options.resources),
    paperId: options["paper-id"],
    output: options.output
  });
  console.log(`Draft created: ${result.output}`);
  console.log("Review both official documents before merging this node or edge into papers.json.");
  return result;
}

const isCli = process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isCli) main();
