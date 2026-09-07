# TypeScript LSP: why go to definition failed

This is a post-mortem of a broken TypeScript jump in Spacemacs.
The language server was healthy.
Emacs dropped the initialize result, so hover and definition looked unsupported.

---

## Part 0: 60-second version

`lsp-mode` stores JSON-RPC objects as hash-tables or as plists.
The choice is a compile-time contract.
The parser also reads that choice at runtime.

Those two sides must match.

This config set `lsp-use-plists` in `dotspacemacs/user-config`.
That function runs after packages load.
`user-config` is too late to choose the compile-time side.

Native-comp then baked plist accessors into `~/.emacs.d/eln-cache/` `lsp*.eln` files.
After the late `setq` was removed, the parser produced hash-tables.
The stale `.eln` files still expected plists.

```
compile hash-table + runtime plist  ->  hash-table-p on a plist
compile plist      + runtime hash-table ->  plistp on a hash-table
```

Both errors discard initialize.
The server still sent `hoverProvider` and `definitionProvider`.
Emacs never stored them.

---

## Part 1: Two clocks in lsp-mode

`lsp-use-plists` is not a normal preference.
`lsp-protocol.el` uses it in two ways.

Load time: `lsp-get` becomes `plist-get` or `gethash`.
Compile time: `lsp-interface` and `(-lambda ((&InitializeResult :capabilities)) ...)` expand to one of those accessors.

The JSON reader checks the variable at runtime:

```elisp
(json-parse-buffer :object-type (if lsp-use-plists 'plist 'hash-table) ...)
```

A late `setq` changes the reader.
It does not rewrite compiled accessors.

The initialize handler then calls generated readers such as `lsp:server-capabilities-position-encoding?` on the capabilities object.
If that reader is a plist function and the object is a hash-table, Emacs signals `plistp`.
If that reader is `gethash` and the object is a plist, Emacs signals `hash-table-p`.

`lsp-mode` wraps that failure as `Error processing message`.
Capabilities stay empty.
`lsp--capability` then reports no hover and no definition.

Vendor steps for a real plist build are on the lsp-mode performance page:
https://emacs-lsp.github.io/lsp-mode/page/performance/

Those steps require `LSP_USE_PLISTS` in the environment before compile, then a full rebuild.
A `setq` after load is not that procedure.

---

## Part 2: Which file loads, and when

Spacemacs reads `~/.spacemacs.d/init.el` when that file exists.
It does not read `~/.spacemacs`.

Emacs 27 and later load files in this order.
A later box cannot change a value that an earlier box already compiled.

```
[1] ~/.emacs.d/early-init.el
[2] ~/.emacs.d/init.el          (Spacemacs core)
[3] ~/.spacemacs.d/init.el
      dotspacemacs/user-init    (set vars before packages)
[4] layers and packages         (byte-compile / native-comp)
[5] dotspacemacs/user-config    (too late for lsp-use-plists)
```

The GNU Emacs manual describes step 1 at
https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

`user-init` exists for variables that packages consume at load time:

```737:742:dot_spacemacs.d/init.el
(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
```

`user-config` runs at the end of startup:

```962:967:dot_spacemacs.d/init.el
(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
```

The plist switch lived in `user-config`, after `exec-path-from-shell-initialize`.
That is the wrong stage.

Byte-compiled `lsp-protocol.elc` from package install used hash-table accessors.
The first Emacs session after that install native-compiled `lsp*.eln` while `LSP_USE_PLISTS` was already `true` in the process.
Emacs prefers `.eln` over `.elc`.
The cache then won.

---

## Part 3: Post-mortem

### Symptom

Go to definition and hover failed in `typescript-mode`.
`*Messages*` showed a connection to `ts-ls`.
It then showed `Error processing message` and `The connected server(s) does not support method textDocument/hover`.

### Wrong hypothesis

The TypeScript server or the TypeScript layer was broken.
The server payload included `hoverProvider` and `definitionProvider`.
The layer selected `typescript-backend` `lsp`.
The failure was client-side decode.

### Evidence

1. The initialize capabilities object arrived.
   Processing died on a type predicate (`hash-table-p` or `plistp`).
2. A no-config batch load of the installed `lsp-protocol.elc` accepted a hash-table and rejected a plist.
   That is the package-install compile.
3. After the late `setq` was removed, the error flipped from `hash-table-p` on a plist to `plistp` on a hash-table.
   The reader had moved.
   The native-comp cache had not.
4. After the `lsp*.eln` files were deleted, the same batch load used `.elc` again.
   `lsp:server-capabilities-hover-provider?` returned `t` on a hash-table capabilities object.

### Fix

1. Remove `(setenv "LSP_USE_PLISTS" "true")` and `(setq lsp-use-plists t)` from `user-config`.
2. Delete stale native-comp output:

```
find ~/.emacs.d/eln-cache -name 'lsp*.eln' -delete
find ~/.emacs.d/eln-cache -name 'lsp*.eln.tmp' -delete
```

3. Restart Emacs.

The live `user-config` now continues from `exec-path-from-shell` into package-quickstart.
It does not touch `lsp-use-plists`:

```1242:1247:dot_spacemacs.d/init.el
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; Speed up package loading and loads package only when needed
  (setq package-quickstart t)
  (setq use-package-always-defer t)
```

---

## Part 4: Wrong fixes

Do not set `lsp-use-plists` in `user-config` again.
The next native-comp pass will bake the mismatch back in.

Do not treat `textDocument/hover` as a missing server feature when initialize already failed.

Do not reinstall `typescript-language-server` for this error pair.
The server already advertised the methods.

If you want the plist performance path later, set `LSP_USE_PLISTS` in `~/.emacs.d/early-init.el`, delete `lsp*.eln` and the lsp-mode package bytecode, then reinstall lsp-mode while that environment is set.
Keep the value stable after that compile.
`user-config` is not that path.

---

## Part 5: Lessons to reuse

A performance snippet that says "set this variable" is incomplete when the package compiles against the value.

`user-init` is the Spacemacs hook for pre-package variables.
`early-init.el` is the hook for values that package native-comp must see.

An error that names `hash-table-p` or `plistp` on a JSON-RPC body is a decode contract failure.
Fix the contract before you debug the language server.

After you change `lsp-use-plists`, delete the `lsp*.eln` cache.
Restart Emacs.
A reconnect in the old process is not enough.
