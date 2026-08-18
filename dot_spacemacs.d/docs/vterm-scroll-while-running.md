# Why vterm jumps to the bottom while a command still prints

A short guide from a real question about this Spacemacs setup.

The symptom: you scroll up in a vterm buffer while a long command still runs.
New lines of output appear, and the window snaps back to the bottom.
You cannot keep reading the earlier output.

The cause: vterm is a live terminal emulator, not a plain text buffer.
In normal mode it keeps point on the process cursor.
Redisplay follows that point, so the window follows the bottom.

This is not a bug in `~/.spacemacs.d/init.el`.
Your shell layer only selects vterm as the default shell:

```162:163:~/.spacemacs.d/init.el
            shell-default-shell 'vterm
            shell-default-position 'bottom)
```

Facts here come from emacs-libvterm on this machine (`vterm-20260730.1414`) and from upstream issues [#397](https://github.com/akermu/emacs-libvterm/issues/397) and [#730](https://github.com/akermu/emacs-libvterm/issues/730).

---

## Part 0: The 60-second version

Three layers cooperate when text arrives in vterm:

1. The child process writes bytes to a PTY.
2. libvterm updates a terminal screen plus scrollback.
3. The Emacs module redraws the buffer and moves point to the live cursor.

When you scroll up in normal vterm mode, you move the Emacs window only.
The live cursor stays at the bottom.
The next redraw puts point back on that cursor.
The window follows, so your scroll is lost.

```
process writes  ->  libvterm updates  ->  point to cursor  ->  window snaps down
```

`vterm-copy-mode` breaks that loop on purpose.
It freezes the live display so the buffer acts like normal Emacs text.
You scroll, search, and select freely.
When you leave copy-mode, queued output appears.

---

## Part 1: What vterm is

### 1.1 Not `shell-mode`, not `comint`

`shell-mode` and many compilation buffers append text to an Emacs buffer.
Emacs owns the text.
Scroll rules like `compilation-scroll-output` decide whether point stays at the end.

vterm does something different.
It embeds [libvterm](https://github.com/neovim/libvterm), a real terminal emulator.
The process sees a PTY.
Escape codes for color, cursor motion, and alternate screens work as in kitty or alacritty.

That fidelity is why people pick vterm.
It is also why scroll behavior differs from comint.

### 1.2 Two cursors in one window

vterm keeps two ideas of "where I am":

| Concept | Owner | Meaning |
| --- | --- | --- |
| Terminal cursor | libvterm | Where the process writes next |
| Emacs point | Emacs | Where editing and redisplay focus |

In normal vterm mode, the package keeps those two aligned.
New output moves the terminal cursor.
The module moves Emacs point with it.
A window that shows point therefore shows the bottom of the stream.

### 1.3 Scrollback still exists

vterm stores scrollback.
`vterm-max-scrollback` sets how many lines stay available (default 1000 in this package).
You can move through that history when the view is not forced to the live cursor.

The problem is not missing history.
The problem is live redraw that re-centers on the cursor while the process prints.

---

## Part 2: The live-output pipeline

```
[1] Child process (shell, build, test runner)
        |
        | writes stdout/stderr
        v
[2] PTY
        |
        v
[3] libvterm screen + scrollback
        |
        v
[4] vterm Emacs module redraws buffer text
        |
        v
[5] Point moves to terminal cursor (bottom while printing)
        |
        v
[6] Redisplay aligns the window with point
```

Stage 5 is the stage that fights your scroll.
You change stage 6 by scrolling the window.
Stage 5 runs again on the next output chunk and undoes stage 6.

Many standalone terminals skip stage 5 when you leave the bottom.
They append below and keep your viewport.
emacs-libvterm does not do that in normal mode today.
Maintainers point users to `vterm-copy-mode` instead of a "sticky scroll" option.

---

## Part 3: What to do

### 3.1 Enter copy-mode, then scroll

Condition: a command still prints and you need to read earlier lines.

1. Press `C-c C-t` to run `vterm-copy-mode`.
2. Scroll and search as in any normal buffer.
3. Leave copy-mode with `C-c C-t` again, or finish with `RET` (`vterm-copy-mode-done`).

Package docstring (local install):

```999:1009:/home/volodymyrvitvitskyi/.emacs.d/elpa/30.1/develop/vterm-20260730.1414/vterm.el
(define-minor-mode vterm-copy-mode
  "Toggle `vterm-copy-mode'.

When `vterm-copy-mode' is enabled, the terminal will not display
additional output received from the underlying process and will
behave similarly to buffer in `fundamental-mode'.  This mode is
typically used to copy text from vterm buffers.

A convenient way to exit `vterm-copy-mode' is with
`vterm-copy-mode-done', which copies the selected text and exit
`vterm-copy-mode'."
```

Default bindings in `vterm-mode-map`:

| Key | Command |
| --- | --- |
| `C-c C-t` | `vterm-copy-mode` |
| `S-<prior>` / `S-<next>` | `scroll-down-command` / `scroll-up-command` |
| `C-c C-l` | `vterm-clear-scrollback` |

### 3.2 Optional: enter copy-mode on PageUp

Condition: you want PageUp to freeze the view without a separate toggle.

Bind a small wrapper in user config:

```elisp
(defun my/vterm-page-up ()
  "Enter vterm-copy-mode, then scroll up one page."
  (interactive)
  (unless vterm-copy-mode
    (vterm-copy-mode 1))
  (scroll-down-command))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map [prior] #'my/vterm-page-up))
```

Test the binding with `C-h k` on PageUp inside a vterm buffer.

### 3.3 Pause output at the PTY (different tool)

Condition: you need the process itself to stop writing for a moment.

Use `vterm-send-stop` and `vterm-send-start` (XON/XOFF style flow control).
This pauses the stream.
It does not give the same free Emacs navigation as copy-mode.
Prefer copy-mode for reading and copying text.

---

## Part 4: Post-mortem of the confusion

| What people assume | What is true |
| --- | --- |
| Spacemacs shell settings broke scroll | They only pick vterm as the shell backend |
| Scrollback is missing | Scrollback exists; live follow overrides the viewport |
| Mouse or evil scroll is broken | Scroll works; the next redraw resets the window |
| Same as `*compilation*` sticky scroll | comint/compilation use different scroll variables |
| A missing `setq` will fix it | No supported sticky-scroll option in normal mode |

Upstream closed a request to preserve scroll position during output as a duplicate of the existing discussion.
The practical answer remains: use `vterm-copy-mode`.

---

## Part 5: Glossary

| Term | Meaning |
| --- | --- |
| PTY | Pseudo-terminal; the process believes it talks to a real terminal |
| libvterm | C terminal emulator library that vterm embeds |
| Scrollback | Lines that left the visible screen but stay in the buffer history |
| Terminal cursor | Write position inside the emulated screen |
| Emacs point | Buffer position Emacs uses for editing and window focus |
| `vterm-copy-mode` | Minor mode that freezes live display for normal Emacs motion |
| Sticky scroll | Keep the viewport when new output arrives below (common in GUI terminals; not default in vterm normal mode) |

---

## Part 6: Quick verification

Condition: you want to confirm the behavior on this machine.

1. Open vterm (`SPC ' ` with the shell layer defaults, or `M-x vterm`).
2. Run a slow printer, for example: `seq 1 500; sleep 30; seq 501 1000`.
3. While it prints, scroll up with the mouse or `S-<prior>`.
4. Watch the window jump down on new output.
5. Press `C-c C-t`, scroll up again, and confirm the view stays.
6. Press `C-c C-t` to leave copy-mode and see caught-up output.

Expected result: steps 3-4 jump; steps 5-6 stay put until you exit copy-mode.
