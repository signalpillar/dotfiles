# mermaid-validate

Parse-only Mermaid diagram validator.
Runs the real `mermaid` npm package's parser against every fenced ` ```mermaid ` block in a Markdown file (or a whole `.mmd`/`.mermaid` file), using a minimal `jsdom` shim so no browser or headless Chromium is needed.
Reports failures with the actual file line number, not just a line number relative to the diagram block.

## Why this exists

Mermaid diagrams, especially `mindmap`, fail to render for syntax reasons that are not obvious from reading the text - the failure only shows up once a human pastes the diagram into a renderer.
Guessing which characters are safe and iterating by trial and error with the user wastes their time.
This script calls Mermaid's own grammar parser directly, so a diagram can be validated in about a second before it is ever shown to anyone.

## Usage

Through the `mermaid-validate` skill (see `dot_agents/skills/mermaid/SKILL.md`), or directly:

```bash
mermaid-validate path/to/file.md [more.md ...]
mermaid-validate path/to/diagram.mmd
```

First run installs `mermaid` and `jsdom` into this directory's `node_modules` (gitignored, machine-local, same idea as the `.venv` next to `pi-job-harness`).
Later runs reuse that install and take well under a second per diagram.

## Files

- `validate.mjs` - the validator; extracts fenced blocks from Markdown, maps parser errors back to file line numbers, exits non-zero on any failure.
- `package.json` - declares the two dependencies (`mermaid`, `jsdom`); no lockfile is required.
- Installed via the wrapper at `~/.local/bin/mermaid-validate` (chezmoi source: `dot_local/bin/executable_mermaid-validate`).

## Known Mermaid `mindmap` gotchas (verified against mermaid@11 via this script)

- A node label wrapped in double quotes that also contains `(`, `)`, `{`, or `}` fails to parse, even though the same characters unquoted sometimes parse fine depending on surrounding indentation.
  The safe rule: never use `()`, `[]`, or `{}` inside `mindmap` node text at all, quoted or not.
  Use a plain dash or "and"/"or" instead of parentheses; use a dash instead of a colon if the colon sits next to other punctuation.
- Plain colons and forward slashes in unquoted or quoted node text parsed fine in testing; they are not the risk - shape-delimiter characters are.
- `root((label))`, `id(label)`, `id[label]`, `id{{label}}` remain valid **shape** syntax on a line that is only that one node; the problem is mixing those characters into free-form prose labels elsewhere in the tree.
- When in doubt, strip the punctuation and rerun `mermaid-validate` - it is faster than reasoning about the grammar.

Other diagram types (flowchart, sequence, etc.) are validated the same way by the same script, since it calls Mermaid's real parser for whatever `diagramType` the block declares; specific gotchas for those types have not been exhaustively probed here.
