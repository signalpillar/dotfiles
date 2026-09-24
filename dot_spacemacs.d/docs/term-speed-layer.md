# Fast Emacs terminals: the `sp-term-speed` layer

This note records how this Spacemacs setup adopts James Cherti's terminal
performance settings.

Source: https://www.jamescherti.com/emacs-terminal-performance-vterm-eat-ansi-term-ghostel/

The shell layer still selects vterm as the default shell.
This layer does not replace that choice.
It cuts redisplay and minor-mode cost inside terminal buffers.

---

## Part 0: 60-second version

Heavy stdout makes Emacs terminal buffers lag.
The cost is font-lock, undo, bidi scan, line wrap, and extra minor modes.

`sp-term-speed` loads a local package `term-speed`.
That package sets vterm/eat/ghostel timers and scrollback.
Then each terminal buffer turns off work the emulator does not need.

Your global `line-spacing` of 7 stays for code buffers.
Terminal buffers set `line-spacing` to 0 so TUI borders line up.

---

## Part 1: What the article changes

Five groups of settings:

1. Output batch delay (`vterm-timer-delay`, eat latency, ghostel delay).
2. Scrollback caps (lines in vterm, characters in eat, bytes in ghostel).
3. Redisplay (`truncate-lines`, bidi off, no wrap, no auto hscroll).
4. Process I/O (`read-process-output-max` 1MiB, no adaptive buffering).
5. Minor modes off (company, flycheck, evil-surround, Spacemacs extras).

vterm cmake flags request `-O3 -march=native` and system libvterm.
Those flags apply only when the C module compiles.

---

## Part 2: Files

Chezmoi source:

- `dot_emacs.d/private/sp-term-speed/packages.el`
- `dot_emacs.d/private/sp-term-speed/local/term-speed/term-speed.el`
- `dot_emacs.d/private/sp-term-speed/README.org`

Enable the layer in `dot_spacemacs.d/init.el` next to `shell`.

Apply dest after you edit the source.
Spacemacs loads `~/.spacemacs.d/init.el` when that file exists.

---

## Part 3: How to check

1. Reload config with `SPC f e R`, or restart Emacs.
2. Open vterm (`SPC ' `).
3. Run `M-x describe-mode`.
4. Confirm company, flycheck, line numbers, and hl-line are off.
5. Run `C-h v line-spacing`. The local value is 0.
6. Run a noisy printer, for example `seq 1 20000`.
7. The buffer stays usable while lines arrive.

If cmake flags must apply, delete `vterm-module.so` under `~/.emacs.d/elpa/`
and run `M-x vterm-module-compile`.

For live output that jumps the window, use `vterm-copy-mode`.
That is a separate issue.
See `dot_spacemacs.d/docs/vterm-scroll-while-running.md`.

---

## Part 4: Trade-offs you accepted

- vterm keeps 500 lines of scrollback, not the package default of 1000.
- Evil jump list (`C-o` / `C-i`) does not record terminal positions.
- RTL text in the shell renders left-to-right.
- The mode line is hidden in vterm/eat/term unless you set
  `term-speed-hide-mode-line` to `nil`.
