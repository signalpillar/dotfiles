# Config investigation reference

Supporting detail for [SKILL.md](SKILL.md).
Read sections as needed during an investigation.

## Tmux escape-code harness

### Sample buffer

```bash
cat > /tmp/faces-test.org <<'EOF'
Plain then *bold word* then /italic word/ then _underline_ then =verbatim= then ~code~ end.
EOF
```

### Baseline (no user config)

```bash
tmux kill-session -t orgmin 2>/dev/null
tmux new-session -d -s orgmin -x 120 -y 20 \
  'TERM=xterm-256color emacs -nw -Q --eval "(progn (setq org-hide-emphasis-markers t) (find-file \"/tmp/faces-test.org\") (font-lock-ensure))"'
sleep 6
tmux capture-pane -e -p -t orgmin | head -4 | cat -v
tmux kill-session -t orgmin
```

Healthy output contains `^[[1m`, `^[[3m`, `^[[4m`.

Always set `TERM=xterm-256color` (or your real terminal type).
Do not test display in a shell with `TERM=dumb`.

### Full config

Spacemacs needs a long startup sleep (often 60-90s).

Open files **after** startup when testing `org-mode-hook` or `user-config` hooks.
Command-line file arguments open before `dotspacemacs/user-config` runs.

```bash
tmux send-keys -t orgfull Escape; sleep 1
tmux send-keys -t orgfull 'M-:'; sleep 1
tmux send-keys -t orgfull '(progn (find-file "/tmp/faces-test.org") (org-show-all) (font-lock-ensure))' Enter
sleep 4
tmux capture-pane -e -p -t orgfull | head -3 | cat -v
```

### Reading noisy captures

Real captures interleave theme restore codes after each reset:

```text
^[[1mbold word^[[0m^[[38;2;7;10;1m^[[48;2;250;250;250m
```

Read in three steps:

1. Find the style code (`^[[1m`, `^[[3m`, ...).
2. Read the text after it.
3. Ignore restore codes after `^[[0m`.

## Chezmoi checks

```bash
chezmoi source-path
chezmoi managed | rg 'spacemacs|emacs'
chezmoi status ~/.spacemacs.d/init.el
chezmoi diff ~/.spacemacs.d/init.el
```

After `chezmoi add`:

```bash
cd "$(chezmoi source-path)"
git diff dot_spacemacs.d/init.el
git show HEAD:dot_spacemacs.d/init.el > /tmp/before.el   # previous commit
diff -u /tmp/before.el dot_spacemacs.d/init.el
```

Look for blocks present in `before.el` but missing in the new file.
Restore them unless the user asked to remove them.

## Spacemacs dotfile precedence

From `core/core-dotspacemacs.el`:

1. If `~/.spacemacs.d/init.el` exists, use it.
2. Else use `~/.spacemacs`.

Once `init.el` exists, edits to `~/.spacemacs` have no effect.
Delete or rename the stale file to prevent future confusion.

## Org emphasis switches

| Variable | Controls |
| --- | --- |
| `org-fontify-emphasized-text` | Installs the font-lock matcher at stage 1 |
| `org-hide-emphasis-markers` | Hides `*` `/` markers via `invisible` text property |

With fontification off, markers may still show raw.
Hiding lives inside the matcher in `org-do-emphasis-faces`.

Org manual: https://orgmode.org/manual/Emphasis-and-Monospace.html

Export still works when fontification is off.
Export uses `org-element`, not font-lock.

## Inside-Emacs diagnostics

| Command | Answers |
| --- | --- |
| `C-u C-x =` | Text properties on point (`face`, `invisible`) |
| `M-x describe-face RET bold RET` | Resolved face attributes |
| `M-x list-faces-display` | All faces as the current frame renders them |
| `M-: (message "%S" face-remapping-alist)` | Buffer-local face overrides |

Run face and variable checks in the buffer under test.

## Educational guide outline

When the user picks **Yes**, use this skeleton:

```markdown
# [Topic]: how it works and why it broke

## Part 0: 60-second version
Root cause in one paragraph.

## Part 1: Concepts
Pipeline or architecture the user must understand.

## Part 2: Domain specifics
Tables, regexp rules, or config keys touched.

## Part 3: Environment
GUI vs TTY, terminfo, TERM traps.

## Part 4: Which config file loads
Precedence rules for this stack.

## Part 5: Post-mortem
Symptom, wrong hypothesis, evidence, fix.

## Part 6: Wrong fixes
Why the first attempt failed.

## Part 7: Test harness
Copy-paste commands, verified during the investigation.
Use placeholders for live host values.

## Part 8: Glossary

## Part 9: Lessons to reuse

## Part 10: Further reading
Verify Info nodes and packages exist before you link them.
```

Match depth to `~/.spacemacs.d/docs/org-emphasis-tty-explained.md` when the investigation was similarly deep.

### Public-aware rewrite

Write the guide as if the dest repo is public.
Keep live hostname, lease, neighbor, MAC, ULA, uid, and session names in the chat only.

Vendor-documented prefixes stay (`10.211.55.0/24` for Parallels Shared).
A specific host octet becomes `N`.
A home path becomes a repo-relative path or `~`.

Before you close, grep the draft for tokens from the live capture.
See **Public-aware guides** in [SKILL.md](SKILL.md).
Example: `docs/parallels-host-reachable-ip.md`.
