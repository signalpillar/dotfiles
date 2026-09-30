---
type: Investigation
title: "TypeScript call trees in Spacemacs"
description: "Comparison of Emacs packages and CLI tools that show call trees for TypeScript in the Spacemacs LSP setup."
tags: [emacs, lsp, typescript, call-hierarchy]
status: stable
sources:
  - id: dot-spacemacs-d-init-el
    resource: /dot_spacemacs.d/init.el
---

# TypeScript call trees in Spacemacs

Date: 2026-09-11

## Question

How do you open a function call tree, or possible call stacks, for TypeScript in this Spacemacs + LSP setup?

This note compares Emacs packages and CLI tools.
It uses primary sources only.

## What you already have

This dotfile uses the TypeScript layer with the LSP backend.

```186:195:dot_spacemacs.d/init.el
     (typescript :variables
                 typescript-linter 'eslint
                 typescript-fmt-tool 'prettier
                 typescript-fmt-on-save t
                 ;; typescript-backend 'tide
                 typescript-backend 'lsp
                 typescript-indent-level 4
                 lsp-javascript-implicit-project-config-experimental-decorators t
                 lsp-clients-typescript-prefer-use-project-ts-server t
                 )
```

That layer pulls in the LSP layer.

```24:29:~/.emacs.d/layers/+lang/typescript/layers.el
(configuration-layer/declare-layer-dependencies '(node javascript prettier))

(when (boundp 'typescript-backend)
  (pcase typescript-backend
    ('lsp (configuration-layer/declare-layer-dependencies '(lsp)))
    ('tide (configuration-layer/declare-layer-dependencies '(tide)))))
```

`SPC m g r` is `xref-find-references`.
That is a flat list of callers.
It is not a tree.

The file browser is `neotree`.
The standalone `lsp` layer line is commented out.
That does not matter.
The TypeScript layer still loads LSP.

## Three different jobs

Do not mix these.

1. Static call hierarchy.
   One function, then its callers, then their callers.
   Or the reverse: callees.
   LSP defines this as a lazy tree.

2. Possible call stacks.
   Every static path from an entry point to a function.
   That is a graph reachability query.
   LSP does not return the full graph in one request.

3. Runtime call stack.
   The live stack in a debugger.
   That is DAP, not LSP.

A CLI wrapper only helps for job 2 or for module graphs.
Job 1 already has an Emacs UI.

## LSP call hierarchy

LSP 3.16 added call hierarchy.

The client asks for a root item.

- Method: `textDocument/prepareCallHierarchy`

Then the client expands one level at a time.

- Incoming: `callHierarchy/incomingCalls`
- Outgoing: `callHierarchy/outgoingCalls`

Source: [LSP 3.17 specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).

The protocol is a tree, not a dump of every stack.
Expand incoming nodes to walk possible caller stacks by hand.
The server decides what counts as a call.

TypeScript limits still apply.

- Dynamic `fn()`, `apply`, and `bind` often hide callees.
- Interface methods fan out to every implementation.
- Higher-order callbacks often stop at the wrapper.
- Aliases and re-exports usually work when tsserver resolves them.

## TypeScript servers

### typescript-language-server

This is the usual `lsp-mode` TypeScript server.

It maps tsserver call-hierarchy items onto LSP.

See `src/features/call-hierarchy.ts` in [typescript-language-server](https://github.com/typescript-language-server/typescript-language-server/blob/master/src/features/call-hierarchy.ts).

The mapping covers incoming and outgoing calls.
The data comes from `ts.server.protocol.CallHierarchyItem`.
tsserver already implements the feature.

Do not write a second analyzer for the same tree.

### vtsls

vtsls wraps the VS Code TypeScript extension.

Its default capabilities set `callHierarchyProvider: true`.

Source: [vtsls capabilities.ts](https://github.com/yioneko/vtsls/blob/main/packages/server/src/capabilities.ts).

Switching server does not add a new kind of tree.
It uses the same LSP methods.

### typescript-go

This research did not verify call-hierarchy support in typescript-go.
Treat it as unproven for this job.

## Emacs packages

### 1. lsp-treemacs (first choice)

Package: [emacs-lsp/lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs).

Command: `lsp-treemacs-call-hierarchy`.

Default: incoming tree.
Prefix argument: outgoing tree.

The README states that.

> Display call hierarchy.
> Use `C-u M-x lsp-treemacs-call-hierarchy` to display outgoing call hierarchy.

Source: [lsp-treemacs README.org](https://github.com/emacs-lsp/lsp-treemacs/blob/master/README.org).

The command checks `textDocument/prepareCallHierarchy`.
It then expands children with `callHierarchy/incomingCalls` or `callHierarchy/outgoingCalls`.

Source: [lsp-treemacs.el](https://github.com/emacs-lsp/lsp-treemacs/blob/master/lsp-treemacs.el).

`lsp-mode` also documents the binding.

`s-l g h` shows the incoming call hierarchy.
It requires `lsp-treemacs`.

Source: [lsp-mode keybindings](https://emacs-lsp.github.io/lsp-mode/page/keybindings/).

Spacemacs binds this only when the package is in use.

```133:137:~/.emacs.d/layers/+tools/lsp/funcs.el
  (when (configuration-layer/package-used-p 'lsp-treemacs)
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "gh" "hierarchy"
      "ghh" #'lsp-treemacs-call-hierarchy
      "gT" #'lsp-treemacs-type-hierarchy)))
```

The package itself is gated on Treemacs.

```24:34:~/.emacs.d/layers/+tools/lsp/packages.el
(defconst lsp-packages
  '(
    lsp-mode
    (lsp-ui :toggle lsp-use-lsp-ui)
    (consult-lsp :requires consult)
    (helm-lsp :requires helm)
    (lsp-ivy :requires ivy)
    (lsp-treemacs :requires treemacs)
```

This setup uses `neotree`.
It does not use the `treemacs` layer.
So Spacemacs does not install `lsp-treemacs`.
So `SPC m g hh` never binds.

That is the gap.
The server already has the data.
Emacs never got the tree view.

Install path for this machine:

1. Add the `treemacs` layer.
   Keep `neotree` if you want it.
2. Reload layers with `SPC f e R`.
3. Open a TypeScript buffer.
4. Put point on a function name.
5. Press `SPC m g hh` for incoming calls.
6. Press `SPC u SPC m g hh` for outgoing calls.

You can also run `M-x lsp-treemacs-call-hierarchy`.

Do not replace Neotree first.
Treemacs is only the renderer for this buffer.

### 2. lsp-ui peek

`lsp-ui-peek-find-references` is a peek overlay.
Spacemacs binds it under `SPC m G r` when `lsp-navigation` is `both`.

Source: [Spacemacs LSP layer README](https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Btools/lsp/README.org).

This is still a flat reference list.
It is not a multi-level call tree.

### 3. eglot-hierarchy

Package: [dolmens/eglot-hierarchy](https://github.com/dolmens/eglot-hierarchy).

Commands:

- `eglot-hierarchy-call-hierarchy`
- `eglot-hierarchy-type-hierarchy`

This package needs Eglot.
This setup uses `lsp-mode`.
Do not add it unless you switch clients.

### 4. dap-mode (runtime stacks only)

Spacemacs documents DAP as the debugger layer next to LSP.

Source: [Spacemacs LSP layer README](https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Btools/lsp/README.org).

For JavaScript and TypeScript, `dap-mode` uses Node, Chrome, Edge, or Firefox adapters.

Source: [dap-mode configuration](https://emacs-lsp.github.io/dap-mode/page/configuration/#javascript).

This shows the live stack at a breakpoint.
It does not show all possible static paths.

Use this when you want one real stack from a run.
Do not use it as a substitute for call hierarchy.

### 5. Older Emacs tools

`call-graph` and Ctags or Citre index names.
They do not use the TypeScript type checker.
They miss aliases, overloads, and re-exports.

Do not wrap those for this job.

## CLI tools

### Do not wrap these for function trees

[madge](https://www.npmjs.com/package/madge) graphs module imports.
It can emit DOT or SVG.
It answers "which file imports which file".
It does not answer "which function calls which function".

`dependency-cruiser` is the same class of tool.
It is a module graph.

A module graph is useful for architecture.
It is the wrong object for a call stack.

### tsserver is the function-level CLI

tsserver already exposes call hierarchy.
`typescript-language-server` and vtsls only translate it.

A new Node CLI would call the same APIs.

- `prepareCallHierarchy`
- `provideCallHierarchyIncomingCalls`
- `provideCallHierarchyOutgoingCalls`

You could recurse those requests and print a tree.
That wrapper would duplicate `lsp-treemacs-call-hierarchy`.
It would also start a second tsserver.

Do not do that unless you want a batch report outside Emacs.

### Full "possible stacks" dump

No maintained TypeScript CLI is the standard tool for "every path from `main` to `foo`".

Reasons:

- The call graph is huge.
- Dynamic calls make it incomplete.
- Recursion and shared helpers explode the tree.

If you later want a batch dump, recurse LSP incoming calls from Emacs.
Cap depth.
Print paths that reach a chosen root.

That is a small Elisp or Node script on top of the running server.
It is not a new analyzer.

### Commercial and archived IDEs

SciTools Understand and Sourcetrail target this problem in C and C++.
They are not a good TypeScript Emacs path.

## Ranked recommendation

| Rank | Option | What you get | Fit here |
| --- | --- | --- | --- |
| 1 | Enable `treemacs` so `lsp-treemacs` installs | Incoming and outgoing call tree from tsserver | Best. Already on the LSP path. |
| 2 | `dap-mode` + `dap-node` or `dap-chrome` | One live stack at a breakpoint | Use for runtime only. |
| 3 | Stay on `SPC m g r` | Flat callers | You already have this. |
| 4 | Recurse LSP incoming calls in a small script | Batch "possible stacks" with a depth cap | Only if the tree is not enough. |
| 5 | madge or dependency-cruiser | File import graph | Wrong object. |
| 6 | New Emacs package around a TS-morph CLI | Second parser, worse than tsserver | Avoid. |
| 7 | eglot-hierarchy | Same LSP tree, different client | Only after an Eglot move. |

## Decision

Enable Treemacs and use `lsp-treemacs-call-hierarchy`.

Do not wrap a CLI first.
The TypeScript language server already implements the protocol.
This config never installed the Emacs tree view.

## Sources

- [LSP 3.17 specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [lsp-treemacs README](https://github.com/emacs-lsp/lsp-treemacs/blob/master/README.org)
- [lsp-treemacs.el](https://github.com/emacs-lsp/lsp-treemacs/blob/master/lsp-treemacs.el)
- [lsp-mode keybindings](https://emacs-lsp.github.io/lsp-mode/page/keybindings/)
- [Spacemacs LSP layer README](https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Btools/lsp/README.org)
- Local `~/.emacs.d/layers/+tools/lsp/packages.el`
- Local `~/.emacs.d/layers/+tools/lsp/funcs.el`
- Local `~/.emacs.d/layers/+lang/typescript/layers.el`
- [typescript-language-server call-hierarchy.ts](https://github.com/typescript-language-server/typescript-language-server/blob/master/src/features/call-hierarchy.ts)
- [vtsls capabilities.ts](https://github.com/yioneko/vtsls/blob/main/packages/server/src/capabilities.ts)
- [eglot-hierarchy](https://github.com/dolmens/eglot-hierarchy)
- [dap-mode JS configuration](https://emacs-lsp.github.io/dap-mode/page/configuration/#javascript)
- [madge](https://www.npmjs.com/package/madge)
