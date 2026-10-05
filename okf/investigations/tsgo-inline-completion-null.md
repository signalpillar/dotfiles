---
type: Investigation
title: "tsgo never finished initialize: inlineCompletion was null"
description: "Why the TypeScript Go language server rejected lsp-mode client capabilities, and how an advice sends an object instead."
tags: [emacs, lsp-mode, typescript, tsgo]
status: stable
sources:
  - id: dot-spacemacs-d-init-el
    resource: /dot_spacemacs.d/init.el
---

# tsgo never finished initialize: inlineCompletion was null

A post-mortem from a real debug session on this Spacemacs setup.

The file opened.
lsp-mode asked which directory to import.
After the import, the server connected and then rejected `initialize`.
The buffer stayed outside any workspace.

## Cause in one paragraph

`lsp--client-capabilities` in lsp-mode (snapshot `20260716`) ends the text-document map with `(inlineCompletion . ())`.
`()` is nil.
`json-serialize` with `:null-object nil` writes that as `"inlineCompletion": null`.
The LSP spec defines `InlineCompletionClientCapabilities` as an object.
The field is optional, so the client may omit it.
The client may not send null.
tsgo is a Go server.
Its decoder refuses null for a struct field and returns `InvalidParams`.
Initialize never completes, so the file never joins a workspace.

## Pipeline

```text
open a .ts buffer
  -> lsp
  -> root is missing from the session
  -> prompt: "<file> is not part of any project"
  -> action i imports the suggested root          this step is healthy
  -> start tsgo --lsp --stdio
  -> initialize
  -> capabilities.textDocument.inlineCompletion = null
  -> tsgo: InvalidParams, null is not allowed
  -> no initialized notification
  -> buffer stays outside the workspace
```

The import prompt is not the failure.
The failure is the next message, the `initialize` request.

## The form that becomes null

lsp-mode builds the capability here:

```elisp
(inlineCompletion . ())
```

The encoder is `lsp--json-serialize`, which calls `json-serialize` with `:null-object nil`.

A batch check of that form prints:

```text
{"inlineCompletion":null,"ok":{"dynamicRegistration":true}}
```

The same encoder with `((dynamicRegistration . :json-false))` prints an object:

```text
{"inlineCompletion":{"dynamicRegistration":false}}
```

## Server check

Send `initialize` to `tsgo --lsp --stdio`.
Use `processId` 1 and a throwaway `rootUri`.

With `"inlineCompletion": null` the server returns code `-32602`:

```text
InvalidParams: json: cannot unmarshal into Go lsproto.TextDocumentClientCapabilities
within "/capabilities/textDocument/inlineCompletion":
null value is not allowed for field "inlineCompletion"
```

With `"inlineCompletion": {"dynamicRegistration": false}` the server returns a result.
That result includes `textDocumentSync`, `completionProvider`, and `hoverProvider`.

Upstream tracks the same bug as emacs-lsp/lsp-mode issue 5081.
The issue was still open when this note was written.
A package update alone does not fix it until that change lands.

## Fix

Keep the installed lsp-mode.
Rewrite a null `inlineCompletion` entry into an object before encode:

```1047:1060:~/.spacemacs.d/init.el
  ;; lsp-mode sends (inlineCompletion . ()), which JSON-encodes as null.
  ;; tsgo rejects null for that field and never finishes initialize.
  ;; An object matches the LSP spec. See emacs-lsp/lsp-mode#5081 and
  ;; okf/investigations/tsgo-inline-completion-null.md.
  (defun vv/lsp-inline-completion-object (caps)
    "Replace a null inlineCompletion capability with an object."
    (when-let ((td (assoc 'textDocument caps)))
      (when (null (alist-get 'inlineCompletion (cdr td)))
        (setf (alist-get 'inlineCompletion (cdr td))
              '((dynamicRegistration . :json-false)))))
    caps)
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp--client-capabilities :filter-return
                #'vv/lsp-inline-completion-object))
```

Restart Emacs, then open the TypeScript file again.
If a dead workspace is still attached, run `M-x lsp-workspace-restart`.

## Watch list

Issue 5081 names a second rejection.
lsp-mode sends `"params": null` on methods that take no parameters, including `shutdown`.
JSON-RPC wants that field omitted, or an object.
tsgo rejects null there too.
Initialize is the failure that blocks the first connection.
The `shutdown` case shows up on workspace restart.

Drop the advice after an lsp-mode release that sends an object, or omits the field.
