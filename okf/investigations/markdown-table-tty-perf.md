---
type: Investigation
title: "Markdown tables in terminal Emacs: hangs and scroll hitch"
description: "Debug record of font-lock cost on large GFM tables in TTY Spacemacs."
tags: [emacs, markdown, font-lock, tty, performance]
status: stable
sources:
  - id: dot-spacemacs-d-init-el
    resource: /dot_spacemacs.d/init.el
---

# Markdown tables in terminal Emacs: why edits hung and scroll hitch

This note records a TTY Spacemacs debug of Markdown files with large GFM tables.

The first symptom: one keystroke froze the editor for a long time.

The second symptom: scroll felt sluggish after the hang was gone.

The file size was small.
The cost was `font-lock` on tables with many `|` and backtick characters.

---

## Part 0: 60-second version

`markdown-mode` fontifies GFM tables through `jit-lock`.
Each newly visible or edited region runs every Markdown matcher.

`markdown-hide-urls` adds a second pass.
That pass calls `markdown--fontify-table-alignment` on every pipe.
Each pipe then calls `markdown-inline-code-at-pos`, which rescans the whole table.

TTY scroll does the same work for each new window of lines.
Smooth scrolling (`scroll-conservatively` 101) paints every line at once, so the matcher cost shows as hitch.

The edit hang is off when `markdown-hide-urls` is `nil`.
The scroll hitch is off when redisplay skips font-lock while keys are pending.

---

## Part 1: Pipeline

Five stages sit between a key and painted text.

```
[1] markdown-mode installs font-lock keywords
[2] jit-lock picks a region (window, or whole table if multiline)
[3] matchers attach faces (code, table, bold, attributes)
[4] faces resolve (theme)
[5] redisplay paints the TTY
```

Edit hang failed at stage 3.
`markdown-fontify-tables` called `markdown--fontify-table-alignment`.
That function called `markdown-inline-code-at-pos-p` once per `|`.

Scroll hitch failed at stage 2 plus stage 5.
`redisplay_internal` called `jit-lock-function` for each newly visible line.
Stage 3 still ran `markdown-match-code` and `markdown-match-inline-attributes`.
Those matchers are expensive on tables that contain hundreds of backticks.
They are cheap next to the old alignment pass.

```
edit, hide-urls on:
  key -> after-change -> jit-lock -> fontify-tables
       -> alignment per | -> inline-code scan of the table
       -> TTY freeze

scroll, hide-urls off:
  C-v / C-n -> redisplay -> jit-lock of new window
            -> match-code + other keywords
            -> hitch, not a multi-minute freeze
```

Do not treat both symptoms as one bug.
The profiler stack names the stage.

---

## Part 2: Domain rules

`markdown-fontify-tables` always paints `markdown-table-face`.
It calls `markdown--fontify-table-alignment` only when `markdown-hide-markup` or `markdown-hide-urls` is non-nil.

Alignment walks every `[ \t]+|` on the line.
For each pipe it asks `markdown-inline-code-at-pos-p`.
That search starts at the text-block start.
A GFM table has no blank lines, so the block is the whole table.

`markdown-match-code` uses `markdown-regex-code`.
The regex spans lines and then calls `markdown-code-block-at-pos`.
A table cell full of `` `field` `` fragments makes that search quadratic.

`font-lock-multiline` is on in `markdown-mode`.
`markdown-syntax-propertize-extend-region` grows a change to the nearest blank-line pair.
One edit in a table fontifies every row.

Spacemacs smooth scrolling sets `scroll-conservatively` to 101.
Each line of motion exposes new text and asks `jit-lock` again.

---

## Part 3: Environment

This debug used `emacs -nw`.
`display-graphic-p` is nil, so `variable-pitch-mode` does not start.

`valign-mode` is not the cause here.
The Org layer keeps `org-enable-valign` off unless you set it.
The Markdown layer only `post-init`s `valign` when another layer loads it.

A GUI frame plus `variable-pitch-mode` plus table alignment is a separate cost.
Do not mix that case with this TTY report.

---

## Part 4: Which config file loads

Spacemacs loads `~/.spacemacs.d/init.el` when that file exists.
Edits in `~/.spacemacs` then have no effect.

Chezmoi source for that file is `dot_spacemacs.d/init.el`.
Apply dest after you edit the source.

---

## Part 5: Post-mortem

### Symptom 1: edit freeze

`profiler-report` put about 90 percent of samples in `redisplay_internal` -> `jit-lock-fontify-now` -> `markdown-fontify-tables` -> `markdown--fontify-table-alignment` -> `markdown-inline-code-at-pos`.

`markdown-hide-urls` was `t` in the Markdown layer variables.

The file had on the order of a hundred table rows and many more pipes than rows.
Each pipe rescanned the table for inline code.

`emacs -Q` plus `markdown-mode` plus `font-lock-ensure` stayed under a second.
The same insert in full Spacemacs with `markdown-hide-urls` t was about 0.9s per key.
Ten keys then cost several seconds.
Repeated `jit-lock` plus TTY redisplay felt like a hang.

Wrong first guesses: file size, live preview (`vmd`), tree-sitter, Hyperbole, CodeTutor.
Those packages were present or possible.
The profiler named `markdown--fontify-table-alignment` instead.

### Symptom 2: sluggish scroll

After `markdown-hide-urls` was `nil`, table alignment dropped to a few percent.

A new profile during scroll still showed about 90 percent in `jit-lock-function`.
Top matchers were `markdown-match-code` and `markdown-match-inline-attributes`.

Twenty simulated window fontifications cost about 0.3s with hide-urls off.
That is a hitch, not a freeze.
Smooth scrolling repeats that cost on every motion command.

### Fix

Set `markdown-hide-urls` to `nil` in the Markdown layer.

See `dot_spacemacs.d/init.el` around the `markdown :variables` form.

Set `redisplay-skip-fontification-on-input` and `fast-but-imprecise-scrolling` to `t` in `dotspacemacs/user-config`.

In buffers with more than 40 table lines, set `jit-lock-defer-time` to `0.05`.
`my/markdown-defer-jit-lock-on-tables` does that on `markdown-mode-hook`.

See `dot_spacemacs.d/init.el` around `my/markdown-defer-jit-lock-on-tables`.

Toggle URL hiding on a small file with `SPC m T l` when you need it.

---

## Part 6: Wrong fixes

Do not turn off `font-lock-mode` for all Markdown.
That hides the hang and also hides useful faces.

Do not blame the terminal first.
`emacs -Q -nw` on the same file was fast.

Do not install `valign` to "fix alignment" on TTY tables.
`valign` is for pixel alignment and is laggy on large tables.

Do not keep `markdown-hide-urls` on and only raise `gc-cons-threshold`.
The alignment scan is CPU in matchers, not garbage collection.

---

## Part 7: Further reading

- `markdown-fontify-tables` and `markdown--fontify-table-alignment` in the installed `markdown-mode.el`
- `markdown-inline-code-at-pos` and `markdown-match-code` in the same file
- GNU Emacs manual, Font Lock: https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
- GNU Emacs Lisp reference, JIT Lock: https://www.gnu.org/software/emacs/manual/html_node/elisp/JIT-Lock.html
