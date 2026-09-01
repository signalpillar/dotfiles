# Why chezmoi tracks `~/.emacs.d/early-init.el`

Chezmoi tracks this one Spacemacs core file on purpose.
It does not track the rest of `~/.emacs.d`.

Emacs loads `early-init.el` before packages and before the first frame.
`~/.spacemacs.d/init.el` runs too late for the settings in the overlay tail.

The GNU Emacs manual names this file in the node `Early Init File`.

---

## Decision

Keep `dot_emacs.d/early-init.el` in chezmoi.

Treat it as a personal overlay on the Spacemacs clone.
Do not add other Spacemacs core files to chezmoi.

---

## Load pipeline

Emacs 27 and later load files in this order.
A later box cannot set a value that the earlier box already consumed.

```
[1] ~/.emacs.d/early-init.el
      package-enable-at-startup
      load core/core-early-funcs.el
      overlay: GC, font cache, frame resize, native-comp

[2] Emacs package-initialize  (skipped when step 1 set package-enable-at-startup to nil)

[3] ~/.emacs.d/init.el
      Spacemacs core boots

[4] ~/.spacemacs.d/init.el
      dotspacemacs/layers
      dotspacemacs/init
      dotspacemacs/user-init

[5] layers and packages

[6] emacs-startup-hook
      core-spacemacs.el writes gc-cons-threshold from dotspacemacs-gc-cons
      dotspacemacs/user-config
```

Step 1 is the only safe place for `frame-inhibit-implied-resize`.
Step 6 overwrites the startup GC threshold with the value in `dotspacemacs/init`.

```269:269:dot_spacemacs.d/init.el
   dotspacemacs-gc-cons '(100000000 0.1)
```

```327:329:~/.emacs.d/core/core-spacemacs.el
     (setq gc-cons-threshold (car dotspacemacs-gc-cons)
           gc-cons-percentage (cadr dotspacemacs-gc-cons))))
```

---

## Ownership pipeline

Two repositories write into `~/.emacs.d`.
Each owns a different set of paths.

```
spacemacs git clone          chezmoi source
        |                            |
        |                            +-- dot_spacemacs.d/          ->  ~/.spacemacs.d/
        |                            +-- dot_emacs.d/private/      ->  ~/.emacs.d/private/
        |                            +-- dot_emacs.d/early-init.el ->  ~/.emacs.d/early-init.el
        |                            |
        +-- core/, layers/, init.el, early-init.el (upstream body)
        |
        v
   ~/.emacs.d   (clone first, then chezmoi apply)
```

A new machine follows this sequence.

1. The setup script clones Spacemacs into `~/.emacs.d`.
2. `chezmoi apply` writes the overlay file on top of the clone.

```29:31:run_onchange_setup_box.sh.tmpl
function setup_emacs {
    mv $HOME/.emacs.d $HOME/.emacs.d`date -I`
    git clone https://github.com/syl20bnr/spacemacs $HOME/.emacs.d
```

A clone without the later apply drops the overlay tail.

---

## What the overlay adds

The chezmoi file keeps the upstream body.
It then appends a personal tail.

```36:40:dot_emacs.d/early-init.el
(setq package-enable-at-startup nil)

(load (concat (file-name-directory load-file-name)
              "core/core-early-funcs")
      nil (not init-file-debug))
```

```54:67:dot_emacs.d/early-init.el
;; Don't collect garbage until there's a good bit of it, and don't
;; compact the font cache (to avoid a performance regression with
;; doom-modeline).
(setq gc-cons-threshold (* 256 1024 1024))
(setq inhibit-compacting-font-caches t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
(setq comp-deferred-compilation nil)
```

Upstream `early-init.el` on the `develop` branch ends at the commented GUI hooks.
The tail is not upstream.

`dotspacemacs-gc-cons` is the post-startup GC policy.
The `256` MiB value in the tail is the startup GC policy only.

---

## Sync rules

The live file has two owners.
`git status` in `~/.emacs.d` reports `early-init.el` as modified.
That dirty state is expected.

After a Spacemacs pull:

1. Diff upstream `early-init.el` against `dot_emacs.d/early-init.el`.
2. If upstream changed the body, copy those lines into the chezmoi file.
3. Keep the overlay tail unless you intend to drop it.
4. Run `chezmoi apply` so the live file matches chezmoi.

Do not run `chezmoi add ~/.emacs.d/early-init.el` after a pull until you finish that merge.
`chezmoi add` copies home into source.
An upstream-only home file drops the tail.

Do not add `core/`, `layers/`, or `~/.emacs.d/init.el` to chezmoi.
Those paths belong to the Spacemacs clone.

---

## Check the contract

These commands confirm the overlay is still the live file.

```bash
chezmoi managed | rg 'early-init'
chezmoi diff ~/.emacs.d/early-init.el
diff -u "$(chezmoi source-path)/dot_emacs.d/early-init.el" ~/.emacs.d/early-init.el
git -C ~/.emacs.d diff -- early-init.el
```

An empty `chezmoi diff` means live matches chezmoi.
A non-empty `git diff` in the clone means the overlay tail is present.

---

## Related files

- `dot_emacs.d/early-init.el`: chezmoi overlay.
- `dot_spacemacs.d/init.el`: user config.
- `dot_emacs.d/private/`: user layers, snippets, and themes.
- `run_onchange_setup_box.sh.tmpl`: clone step for a new machine.
- `~/.emacs.d/core/core-early-funcs.el`: loaded from `early-init.el`.
- `~/.emacs.d/core/core-spacemacs.el`: writes the post-startup GC values.
