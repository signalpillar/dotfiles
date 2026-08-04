#!/usr/bin/env node
// Parse-only Mermaid diagram validator.
//
// Runs the real `mermaid` package's parser (via a minimal jsdom shim, no
// browser/puppeteer needed) against every ```mermaid fenced block in one or
// more Markdown files, or against a whole file for raw .mmd/.mermaid input.
// Reports parse errors with a file line number, not just a line number
// relative to the diagram block.
//
// Usage:
//   node validate.mjs <file.md> [more.md ...]
//   node validate.mjs <diagram.mmd>

import { JSDOM } from "jsdom";
import fs from "node:fs";
import path from "node:path";

const dom = new JSDOM("<!DOCTYPE html><html><body></body></html>", { url: "http://localhost/" });
global.window = dom.window;
global.document = dom.window.document;
Object.defineProperty(global, "navigator", { value: dom.window.navigator, configurable: true });
global.SVGElement = dom.window.SVGElement;

const mermaid = (await import("mermaid")).default;
mermaid.initialize({ startOnLoad: false });

function extractBlocks(filePath, content) {
  const ext = path.extname(filePath).toLowerCase();
  if (ext === ".mmd" || ext === ".mermaid") {
    return [{ startLine: 1, text: content }];
  }
  const blocks = [];
  const fenceRe = /```mermaid[ \t]*\n([\s\S]*?)```/g;
  let match;
  while ((match = fenceRe.exec(content)) !== null) {
    const beforeFence = content.slice(0, match.index);
    const fenceLine = beforeFence.split("\n").length; // line of the ``` opener
    blocks.push({ startLine: fenceLine + 1, text: match[1] });
  }
  return blocks;
}

function toFileLine(startLine, parseErrorMessage) {
  const m = /on line (\d+)/.exec(parseErrorMessage || "");
  if (!m) return null;
  const relativeLine = Number(m[1]);
  return startLine + relativeLine - 1;
}

async function validateFile(filePath) {
  const content = fs.readFileSync(filePath, "utf8");
  const blocks = extractBlocks(filePath, content);

  if (blocks.length === 0) {
    console.log(`${filePath}: no mermaid diagrams found`);
    return { checked: 0, failed: 0 };
  }

  let failed = 0;
  for (const [i, block] of blocks.entries()) {
    const label = blocks.length > 1 ? `${filePath} (block ${i + 1})` : filePath;
    try {
      const result = await mermaid.parse(block.text, { suppressErrors: false });
      console.log(`OK    ${label} - ${result.diagramType}`);
    } catch (err) {
      failed += 1;
      const message = err && err.message ? err.message : String(err);
      const fileLine = toFileLine(block.startLine, message);
      const where = fileLine ? `${filePath}:${fileLine}` : label;
      console.error(`FAIL  ${where}`);
      console.error(
        message
          .split("\n")
          .map((l) => `      ${l}`)
          .join("\n")
      );
    }
  }
  return { checked: blocks.length, failed };
}

const files = process.argv.slice(2);
if (files.length === 0) {
  console.error("usage: node validate.mjs <file.md|file.mmd> [more ...]");
  process.exit(2);
}

let totalChecked = 0;
let totalFailed = 0;
for (const file of files) {
  const { checked, failed } = await validateFile(file);
  totalChecked += checked;
  totalFailed += failed;
}

console.log(`\n${totalChecked} diagram(s) checked, ${totalFailed} failed`);
process.exit(totalFailed > 0 ? 1 : 0);
