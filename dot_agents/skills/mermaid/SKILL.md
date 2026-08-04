---
name: mermaid
description: >-
  Validate Mermaid diagrams (flowcharts, mindmaps, sequence diagrams, etc.)
  against the real Mermaid parser before showing them to the user, instead of
  guessing at syntax and rewriting after a render error. Use whenever
  authoring or editing a ```mermaid code block, especially `mindmap` diagrams,
  or when a user reports "mind map cannot be loaded" / "diagram won't
  render" / a Mermaid parse error.
---

# Mermaid

Do not hand-verify Mermaid syntax by eye and wait for the user to hit a parse error in their renderer.
Run the diagram through Mermaid's own parser first - it takes about a second and the error it throws already tells you which line to fix.

## Cold start

```bash
mermaid-validate path/to/file.md
```

Works on any file containing one or more ` ```mermaid ` fenced blocks, or a raw `.mmd`/`.mermaid` file.
Exits non-zero and prints the failing file line number plus Mermaid's own error text when a block does not parse.
Exits 0 and prints the detected diagram type when it does.

If `mermaid-validate` is not on `PATH`, it installs on first use into `~/.local/share/mermaid-validate/node_modules` (chezmoi source: `dot_local/share/mermaid-validate/`).
No manual `npm install` step, no headless browser: it shims a minimal DOM with `jsdom` and calls `mermaid.parse()` directly.

## Workflow

1. Write or edit the diagram.
2. Run `mermaid-validate <file>` before telling the user it is ready.
3. On failure, fix only what the error names, then re-run - do not rewrite the whole diagram from scratch on a guess.
4. Only after it passes, hand the file back or report completion.

## mindmap gotchas (confirmed by running mermaid@11 through this validator)

`mindmap` is the diagram type most likely to break in ways that are not obvious from reading the text:

- Never put `(`, `)`, `{`, or `}` inside a node's text, whether or not the text is wrapped in double quotes.
  A quoted label containing parentheses (e.g. `"Task file (durable state)"`) reliably fails with
  `Expecting 'SPACELINE', 'NL', 'EOF', got 'NODE_ID'`.
  The same characters unquoted sometimes parse and sometimes do not, depending on the indentation of surrounding lines - too fragile to rely on either way.
  Rewrite the label without those characters (a dash, "and", or "or" usually reads fine) instead of trying to escape or requote them.
- Plain colons and forward slashes were fine in testing, quoted or not - they are not the risk.
- The double-parenthesis / bracket / brace forms (`root((Title))`, `id(Title)`, `id[Title]`, `id{{Title}}`) are the intentional **shape** syntax and are fine on their own node line; the failure mode is mixing those characters into ordinary prose labels elsewhere in the tree.
- If a diagram ever came back "cannot be loaded" with a message like `Expecting 'SPACELINE', 'NL', 'EOF', got NODE_ID`, that is this exact class of bug - scan every node label for stray `()[]{}` first.

For other diagram types (flowchart, sequence, class, etc.), the same `mermaid-validate` command validates them using the real parser too; their specific syntax pitfalls have not been catalogued here yet, so treat any parse failure output as authoritative over general Mermaid knowledge.

Deep reference: `~/.local/share/mermaid-validate/README.md` (chezmoi source: `dot_local/share/mermaid-validate/README.md`).
