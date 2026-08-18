# Org emphasis in terminal Emacs: how it works and why yours was broken

A beginner-friendly deep dive, written after a real debug session on this exact config.

The bug: `*bold*` and `/italic/` in Org files looked like plain text in `emacs -nw`.
The cause: one line in `~/.spacemacs.d/init.el` set `org-fontify-emphasized-text` to `nil`.
The terminal was never at fault.

This document explains every part of that sentence.
Read Part 1 to Part 3 for the concepts.
Read Part 4 to Part 6 for the post-mortem.
Read Part 7 for the test harness that proved it.

Facts here come from the sources on this machine: Emacs 30.1, Org 9.7.11, and this Spacemacs checkout.
File and line references point to real code you can open.

---

## Part 0: The 60-second version

Emacs does not "highlight Org markup" as a single feature.
Five separate stages cooperate.
A break in any stage produces the same visible result: plain text.

1. Org tells `font-lock` which patterns to search for.
2. `font-lock` searches the buffer and finds `*bold word*`.
3. `font-lock` attaches a `face` property with the value `bold` to the text.
4. Emacs resolves the face `bold` into concrete attributes, for example `:weight bold`.
5. Redisplay paints those attributes, either with a font (GUI) or with escape codes (terminal).

Your config disabled stage 1.
Org never installed the emphasis matcher, so no `bold` face ever reached the text.
Stages 2 to 5 stayed healthy, and they received no input.

```
STAGE 1 broken  ->  no match  ->  no face  ->  nothing to resolve  ->  plain text
STAGE 5 broken  ->  match     ->  face ok  ->  attributes ok      ->  plain text
```

Both failures look identical on screen.
That ambiguity sent the first fix attempt to the wrong stage.

---

## Part 1: How Emacs colors any text

### 1.1 Buffer text carries no color

An Emacs buffer holds plain characters.
The characters carry no color and no weight.
Emacs stores appearance separately, in **text properties** attached to character ranges.

The property that controls appearance is named `face`.

### 1.2 font-lock is the machine that attaches faces

`font-lock-mode` is the syntax highlighting engine.
Each major mode gives `font-lock` a list of rules.
Each rule pairs a pattern with a face.

`font-lock` scans the visible text, applies the rules, and attaches `face` properties.
`font-lock` re-runs on every edit, for the changed region only.

### 1.3 A face is a named bundle of attributes

A face is a name, for example `bold`, `italic`, or `org-code`.
Each face holds attributes: `:weight`, `:slant`, `:underline`, `:foreground`, `:background`, and more.

Themes set those attributes.
Your active theme is `paper`, the first entry in `dotspacemacs-themes` at line 361 of `init.el`.
`paper` is a light theme, and it paints the background near white, at RGB 250,250,250.
Remember that number, because it matters in Part 6.

A face can also inherit from another face.
Emacs resolves inheritance at display time, not at match time.

### 1.4 Redisplay turns attributes into pixels or bytes

The final stage depends on the frame type.

In a GUI frame, Emacs selects a font variant.
Bold text uses the bold cut of your font, `Ubuntu Mono` here.

In a terminal frame, Emacs owns no fonts.
Emacs writes **escape codes** into the terminal, and the terminal renders them.

### 1.5 The full pipeline

```
 Buffer text        "Plain then *bold word* then /italic word/ ..."
      |
      v
 [1] Major mode installs font-lock rules
     org-mode -> org-set-font-lock-defaults
     It adds (org-do-emphasis-faces) ONLY IF
     org-fontify-emphasized-text is non-nil      <-- YOUR BUG WAS HERE
      |
      v
 [2] font-lock scans the buffer and matches org-emph-re
      |
      v
 [3] font-lock attaches text properties
     face      -> bold          (looked up in org-emphasis-alist)
     invisible -> t             (on the markers, if they are hidden)
      |
      v
 [4] Emacs resolves the face into attributes
     theme `paper` + inheritance -> :weight bold, :foreground ...
     buffer-local face-remapping-alist rewrites THIS stage
      |
      v
 [5] Redisplay paints the attributes
     GUI -> bold font variant
     TTY -> escape codes, for example ESC[1m ... ESC[0m
```

Keep this diagram.
Every question in the rest of the document maps to a numbered stage.

---

## Part 2: How Org handles emphasis, in detail

### 2.1 The marker table

Org keeps the marker-to-face mapping in `org-emphasis-alist`.
The default value on this machine:

```elisp
(("*" bold)
 ("/" italic)
 ("_" underline)
 ("=" org-verbatim verbatim)
 ("~" org-code verbatim)
 ("+" (:strike-through t)))
```

Three points deserve attention.

`*` maps to the plain Emacs face `bold`, not to an Org-specific face.
Org owns no `org-bold` face, so a theme that styles `bold` weakly affects Org too.

`=` and `~` map to real Org faces, `org-verbatim` and `org-code`.
The extra `verbatim` flag stops Org from parsing markup inside them.

`+` maps to an inline attribute list, not to a named face.

### 2.2 The emphasis regexp is strict

Org builds the matcher from `org-emphasis-regexp-components`:

```elisp
("-[:space:]('\"{"            ; allowed characters BEFORE the opening marker
 "-[:space:].,:!?;'\")}\\["   ; allowed characters AFTER the closing marker
 "[:space:]"                  ; characters forbidden as the body border
 "."                          ; characters allowed in the body
 1)                           ; maximum newlines inside one emphasis
```

Three practical rules follow from those components.

A marker needs a valid boundary character before it, or the line start.
So `foo*bar*` does not become bold, and ` *bar*` does.

The body must not start or end with whitespace.
So `* bar *` stays plain.

Emphasis spans one newline at most.
So a phrase wrapped across three lines stays plain.

These rules explain most "Org highlighting is broken" reports.
They were **not** your problem, because your markers followed the rules.

### 2.3 Two switches control the outcome

Org exposes two independent variables.
Confusing them wastes hours, and it wasted some here.

| Variable | Question it answers | Default |
| --- | --- | --- |
| `org-fontify-emphasized-text` | Does Org style emphasis at all? | `t` |
| `org-hide-emphasis-markers` | Does Org hide the `*` and `/` characters? | `nil` |

The first switch is the master switch.
Org checks it while building the font-lock rules, in `org-set-font-lock-defaults`:

```elisp
;; Emphasis
;; `org-do-emphasis-faces' prepends faces
(when org-fontify-emphasized-text '(org-do-emphasis-faces))
```

That line sits at line 5940 of `org.el`.
With the value `nil`, the list entry becomes `nil`, and the matcher never enters the rule list.
Stage 1 of the pipeline drops out, permanently, for every Org buffer.

The Org manual documents this switch, and it adds an important nuance.
The manual states that the markup stays recognized, and only the visual highlighting stops.

So `org-fontify-emphasized-text nil` affects display alone.
Export still produces bold output, because the exporter parses the file with `org-element`, not with `font-lock`.
That split explains a real user experience: exported HTML looks correct while the buffer looks plain.

The second switch works much later, inside the matcher itself.

### 2.4 What the matcher does, group by group

`org-do-emphasis-faces` starts at line 5226 of `org.el`.
The important part:

```elisp
(pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
            (m (if org-hide-emphasis-markers 4 2)))
  (font-lock-prepend-text-property
   (match-beginning m) (match-end m) 'face face)
  ...
  (when (and org-hide-emphasis-markers
             (not (org-at-comment-p)))
    (add-text-properties (match-end 4) (match-beginning 5)
                         '(invisible t))
    ...
    (add-text-properties (match-beginning 3) (match-end 3)
                         '(invisible t))
```

Map the regexp groups onto real text:

```
text:        Plain then *bold word* then
                       ^^^^^^^^^^^^
group 1:     " "            the boundary character before the marker
group 2:     "*bold word*"  the whole emphasis, markers included
group 3:     "*"            the opening marker
group 4:     "bold word"    the body
group 5:     "*"            the closing marker

org-hide-emphasis-markers nil  ->  face lands on group 2, markers included
org-hide-emphasis-markers t    ->  face lands on group 4, body only
                                   plus invisible t on group 3 and group 5
```

Note the mechanism for hiding.
Org does not delete the markers.
Org marks them `invisible`, and redisplay skips them.
The characters stay in the file and in the buffer.

### 2.5 The trap this creates

Set the master switch to `nil` and the marker switch to `t`.
Now Org styles nothing, and also hides nothing, because the hiding code lives inside the matcher.
You see raw `*bold word*` with no styling.

That combination looks exactly like a terminal that ignores styling and shows literal characters.
Your config held that exact combination, and produced that exact illusion.

---

## Part 3: Terminal Emacs versus GUI Emacs

### 3.1 The terminal renders, Emacs only describes

A GUI Emacs owns the pixels.
A terminal Emacs owns nothing but a byte stream.

Emacs converts face attributes into **SGR codes**, a subset of ANSI escape sequences.
The terminal emulator interprets them.

| Attribute | Escape code | Bytes |
| --- | --- | --- |
| bold | `ESC[1m` | `\e[1m` |
| italic | `ESC[3m` | `\e[3m` |
| underline | `ESC[4m` | `\e[4m` |
| 256-color foreground | `ESC[38;5;Nm` | `\e[38;5;33m` |
| 24-bit foreground | `ESC[38;2;R;G;Bm` | `\e[38;2;0;205;205m` |
| reset | `ESC[0m` | `\e[0m` |

This table gives you a debug superpower.
Capture the raw bytes, and you learn whether Emacs even tried to style the text.
Part 7 shows how.

### 3.2 Emacs asks terminfo what the terminal supports

Emacs reads the terminal description from the terminfo database, keyed by `$TERM`.
Relevant capabilities:

- `bold` for bold.
- `sitm` and `ritm` to start and end italic.
- `smul` for underline.
- `colors` for the color count.

Check your own terminal:

```bash
infocmp xterm-256color | tr ',' '\n' | grep -w 'sitm\|bold\|smul\|colors'
```

On this machine `xterm-256color` reports `bold=\E[1m`, `sitm=\E[3m`, `smul=\E[4m`, and 256 colors.
`tmux-256color` reports the same three.
So italic works here.

Older descriptions omit `sitm`.
With such a `$TERM`, Emacs prints italic text without any escape code, and italic looks plain.
That is a genuine terminal-side failure, and it is the failure the first fix attempt assumed.

### 3.3 True color needs COLORTERM

The captures in Part 7 contain `38;2;R;G;B`, which is 24-bit color.
Emacs enables that path when `COLORTERM` equals `truecolor` or `24bit`.
Without it, Emacs quantizes colors down to the 256-color palette, or to 8 colors.

Quantization changes appearance in a subtle way.
Two distinct theme colors can collapse into one palette entry, and the distinction disappears.
Keep this in mind when a terminal theme looks flatter than the GUI.

### 3.4 A trap for automated checks: TERM=dumb

A non-interactive shell often exports `TERM=dumb`.
`tput colors` then reports `-1`, and Emacs treats the display as monochrome.

So a batch test can report "no color" while your real terminal works.
Never conclude anything about colors from a `TERM=dumb` shell.
Part 7 avoids this by running Emacs inside tmux with an explicit `TERM`.

### 3.5 What terminal Emacs genuinely cannot do

Terminal Emacs cannot use proportional fonts, variable text heights, or per-buffer font families.
Packages that depend on them degrade or refuse to run.

Your config shows one example.
`fontaine` manages font presets, so it prints a warning in a terminal frame:

```
Warning (fontaine): Cannot use Fontaine in a terminal emulator; try the Emacs GUI
```

This warning is harmless.
It appeared in the `*Warnings*` buffer during the investigation, and it distracted attention.
It has no connection to Org emphasis.

---

## Part 4: Which config file your Emacs actually reads

This part explains a second illusion: an edit that changes nothing, because Emacs reads a different file.

### 4.1 Your machine holds two candidate dotfiles

```
~/.spacemacs              38K, last touched 2026-07-15   <- IGNORED
~/.spacemacs.d/init.el    54K, edited today              <- ACTIVE
```

Both define `dotspacemacs/layers` with an `org` layer.
Both set Org variables.
Only one runs.

### 4.2 The selection rule, from the source

`core/core-dotspacemacs.el` computes the answer in two steps.

First it computes a directory, at lines 46 to 62:

1. `$SPACEMACSDIR`, when the directory exists.
2. `$XDG_CONFIG_HOME/spacemacs/`, when that directory exists.
3. `~/.spacemacs.d/`, when that directory exists.
4. Otherwise `nil`.

Then it computes the file, at lines 64 to 80:

```elisp
(defconst dotspacemacs-filepath
  (let* ((spacemacs-init
          (if dotspacemacs-directory
              (concat dotspacemacs-directory "init.el")
            "~/.spacemacs")))
    (if (file-regular-p spacemacs-init)
        spacemacs-init
      ...
```

Read that with your directory listing.
`~/.spacemacs.d/` exists, so the candidate becomes `~/.spacemacs.d/init.el`.
That file is a regular file, so it wins immediately.
`~/.spacemacs` never enters the picture.

**Rule to remember:** once `~/.spacemacs.d/init.el` exists, `~/.spacemacs` is dead weight.
Delete the stale file, or you will edit it again by mistake.

### 4.3 The init.elc red herring

Your directory also holds `~/.spacemacs.d/init.elc`, a byte-compiled copy.
The `auto-compile` package produced it, and it is listed in your packages.

A stale `.elc` is a classic reason for "my edit did nothing", so it deserved a check.
Here it is innocent, for a precise reason.

Spacemacs loads the dotfile in `dotspacemacs/load-file`, at line 1079:

```elisp
(load dotspacemacs)
```

The argument is the full path, and it ends in `.el`.
When the name already carries a `.el` suffix, `load` reads that exact file.

Verified with a controlled experiment.
A `lt.el` printing one message, and a **newer** `lt.elc` printing another:

```
$ emacs -Q --batch --eval '(load "/tmp/lt.el")'
Loading /tmp/lt.el (source)...
LOADED-SOURCE-EL              <- the .el ran, although the .elc was newer

$ emacs -Q --batch --eval '(load "/tmp/lt")'
LOADED-SOURCE-EL-DIFFERENT    <- without the extension, the .elc ran
```

Conclusion: your `init.elc` is never loaded, and it cannot shadow your edits.
Editing `init.el` is always enough.

### 4.4 Layer variables: the last duplicate wins

You configure Org through the layer system, not through plain `setq`:

```elisp
(org :variables
     org-fontify-emphasized-text t
     org-startup-indented t
     ...)
```

Spacemacs walks that plist in `configuration-layer//set-layer-variables`, at line 1589 of `core/core-configuration-layer.el`.
It calls `set-default` on each pair, in order, before Org loads.

Two consequences matter.

Duplicate keys are legal, and Spacemacs never warns about them.
The **last** occurrence wins, because it assigns last.

Your block held `org-hide-emphasis-markers` three times: `nil`, then `t`, then `t`.
The live session reported `t`, exactly as the rule predicts.

### 4.5 Startup order, and why a hook can arrive too late

```
emacs -nw
  |
  v
~/.emacs.d/early-init.el          package-initialize disabled, GC tuned
  |
  v
~/.emacs.d/init.el                Spacemacs core boots
  |
  v
dotspacemacs/load-file            (load "~/.spacemacs.d/init.el")
  |
  v
dotspacemacs/layers               layer list and :variables plists
  |
  v
set-layer-variables               set-default, in order, LAST WINS
  |
  v
dotspacemacs/user-init
  |
  v
packages load                     org reads the variables now
  |
  v
command-line files open           emacs -nw file.org lands HERE
  |
  v
emacs-startup-hook
  -> spacemacs/startup-hook
  |
  v
dotspacemacs/user-config          your add-hook calls run HERE
```

`core/core-spacemacs.el` builds that tail at lines 286 to 297.
`dotspacemacs/user-config` runs from `emacs-startup-hook`, after Emacs opens the files you named on the command line.

This ordering produced a confusing result during the investigation.
A hook added in `user-config` did not affect a file opened with `emacs -nw file.org`.
The buffer already existed when the hook appeared.

Practical consequences:

- To test `org-mode-hook` changes, open the file **after** startup, with `C-x C-f`.
- To style buffers that already exist, revert them, or run your function by hand.

### 4.6 Errors in user-config fail quietly

Spacemacs calls the dotfile functions through `dotspacemacs|call-func`, at line 838 of `core/core-dotspacemacs.el`.
The macro wraps the call in `condition-case-unless-debug`, counts the error, and prints it in the Spacemacs home buffer.

So an error in the middle of `user-config` stops the rest of `user-config`, without a crash and without a visible traceback.
Every later setting silently disappears.

When a change in `user-config` does nothing, check the Spacemacs home buffer for an error line.
Then re-run the tail by hand to confirm.

---

## Part 5: The post-mortem of this bug

### 5.1 The symptom

In `emacs -nw`, an Org line rendered like this:

```
Plain text then *bold word* then /italic word/ then _underline_ then =verbatim= then ~code~ end.
```

No styling, and visible markers.

### 5.2 The first hypothesis, and why it was wrong

The first hypothesis blamed stage 5, the terminal.
The reasoning ran like this: markers are hidden by your settings, so Org must be styling the text, so the terminal must be dropping the style.

Every clause was false.
The markers were **not** hidden on screen, which already contradicted the premise.
The suggested fix, a `face-remapping-alist` change, targets stage 4.
A stage 4 fix cannot repair a stage 1 failure, because no `bold` face exists to remap.

Lesson: locate the failing stage before choosing a fix.

### 5.3 Establishing a baseline

A minimal Emacs, with no user config, ran in tmux and produced these bytes:

```
Plain text then ^[[1mbold word^[[0m then ^[[3mitalic word^[[0m then ^[[4munderline^[[0m ...
```

`^[[1m` is bold, `^[[3m` is italic, `^[[4m` is underline.
So terminal Emacs, this terminal, and this `$TERM` all handle emphasis correctly.
Stage 5 was innocent, and the config carried the fault.

This step matters.
Without a baseline, a config bug and an environment bug look the same.

### 5.4 The same test with the real config

```
Plain text then *bold word* then /italic word/ then _underline_ then =verbatim= then ~code~ end.
```

No escape codes at all.
Emacs never asked for styling, so Emacs never attached a face.
The failure sat at stage 1, 2, or 3.

### 5.5 Reading the live state

Evaluating the variables inside the running session gave the verdict:

```
FONTIFY=nil HIDE=t ALIST=6 SPACEDOC=nil FLK=t
```

`font-lock-mode` was on.
`org-emphasis-alist` held its six entries.
`org-hide-emphasis-markers` was `t`, as the layer rule predicts.
`org-fontify-emphasized-text` was `nil`, and that is the master switch from Part 2.3.

### 5.6 The line that caused it

```elisp
(org :variables
     ...
     org-hide-emphasis-markers nil

     ;; org-descriptive-links nil
     org-fontify-emphasized-text nil     ; <- the bug
```

The comment above it hints at the history.
Someone tested `org-descriptive-links nil` and left a neighbour switched off.
The effect is global, and it applies to the GUI as well.

### 5.7 Why it hid so well

Four factors combined.

The master switch disables both styling **and** marker hiding, because hiding lives inside the matcher.
The visible result mimics a terminal problem exactly.

The `org` block set `org-hide-emphasis-markers` three times, so reading it suggested markers were hidden.

An unrelated `fontaine` warning sat in `*Warnings*` and looked like a terminal-related failure.

A stale `~/.spacemacs` contained a healthy Org configuration, including `org-fontify-emphasized-text` untouched.
Grepping the wrong file gives a reassuring and useless answer.

### 5.8 The fix

```diff
 (org :variables
      org-enable-modern-support nil
-     org-hide-emphasis-markers nil
 
      ;; org-descriptive-links nil
-     org-fontify-emphasized-text nil
+     org-fontify-emphasized-text t
```

```diff
      org-log-state-notes-insert-after-drawers nil
-     org-hide-emphasis-markers t
      ;; Syntax highlighting in #+BEGIN_SRC blocks
      org-src-fontify-natively t
```

One duplicate `org-hide-emphasis-markers t` remains, so markers stay hidden.
The verified capture after a restart, byte for byte:

```
Plain then ^[[1mbold word^[[0m^[[38;2;7;10;1m^[[48;2;250;250;250m then
^[[3mitalic word^[[0m^[[38;2;7;10;1m^[[48;2;250;250;250m then
^[[4munderline^[[0m^[[38;2;7;10;1m^[[48;2;250;250;250m then
^[[38;2;127;127;127mverbatim^[[38;2;7;10;1m then
^[[38;2;127;127;127mcode^[[38;2;7;10;1m end.
```

Real captures look noisy, and the noise carries meaning.
`^[[0m` resets every attribute at once, including the colors.
So Emacs immediately re-states the theme foreground, `38;2;7;10;1`, and the theme background, `48;2;250;250;250`.

Read such a capture in three steps.
Find the style code, for example `^[[1m`.
Find the text after it.
Ignore the restore codes that follow the reset.

---

## Part 6: Why the face-remap workaround was the wrong tool

The discarded code:

```elisp
(defun my/org-tty-emphasis-faces ()
  (unless (display-graphic-p)
    (face-remap-add-relative 'bold '(:weight bold :foreground "brightwhite"))
    (face-remap-add-relative 'italic '(:slant italic :foreground "cyan"))
    ...))
```

It failed for three separate reasons.
Each reason teaches something.

### 6.1 It addressed the wrong stage

`face-remap-add-relative` edits `face-remapping-alist`, a buffer-local variable consulted at stage 4.
Remapping rewrites a face **after** something attaches it.

With `org-fontify-emphasized-text` set to `nil`, nothing attaches `bold`.
So the remap had no text to affect.
A perfect stage 4 fix repairs nothing when stage 1 is off.

### 6.2 Its colors were nearly invisible on this theme

After the real fix, the remap did apply.
The captured bytes exposed the outcome:

```
^[[1m^[[38;2;255;255;255mbold word^[[0m
```

`brightwhite` resolves to RGB 255,255,255.
The `paper` theme paints the background at RGB 250,250,250.
So bold text became white on white, and it read worse than the default.

Lesson: hardcoded color names assume a background.
`brightwhite` suits a dark theme, and the first six entries of `dotspacemacs-themes` are light themes.

### 6.3 It duplicated work the theme already does

Stage 5 already emits `ESC[1m` for `:weight bold`.
The remap re-stated `:weight bold` and added nothing.

### 6.4 When face remapping is the right tool

Face remapping earns its place in narrow cases.

Use it when a face carries correct attributes that your terminal cannot express, and you want a color substitute.
A `$TERM` without `sitm` is the honest example: italic vanishes, so mapping `italic` to a color helps.

Two guidelines make it safe.

Read colors from the theme instead of hardcoding them:

```elisp
(face-remap-add-relative 'italic
                         `(:slant italic
                           :foreground ,(face-attribute 'font-lock-comment-face
                                                        :foreground nil t)))
```

Verify the result with the harness in Part 7, and check the actual bytes.
Never trust a color name to be visible.

---

## Part 7: The test harness

This harness answers one question with evidence: did Emacs ask the terminal for styling?

It works because tmux can dump a pane **with** escape codes, through `capture-pane -e`.
You read exactly what Emacs emitted.

### 7.1 Prerequisites

- `tmux` installed.
- `cat -v` to make escape bytes printable, as `^[`.

### 7.2 Step 1: a sample file

```bash
cat > /tmp/faces-test.org <<'EOF'
Plain then *bold word* then /italic word/ then _underline_ then =verbatim= then ~code~ end.
EOF
```

### 7.3 Step 2: the baseline, with no config

Always run this first.
It separates environment failures from config failures.

```bash
tmux kill-session -t orgmin 2>/dev/null
tmux new-session -d -s orgmin -x 120 -y 20 \
  'TERM=xterm-256color emacs -nw -Q --eval "(progn (setq org-hide-emphasis-markers t) (find-file \"/tmp/faces-test.org\") (font-lock-ensure))"'
sleep 6
tmux capture-pane -e -p -t orgmin | head -4 | cat -v
tmux kill-session -t orgmin
```

Healthy output contains `^[[1m`, `^[[3m`, and `^[[4m`.

Note the explicit `TERM`.
It defends against the `TERM=dumb` trap from Part 3.4.

### 7.4 Step 3: the same test with your real config

Spacemacs needs far longer to start, so the sleep grows.

```bash
tmux kill-session -t orgfull 2>/dev/null
tmux new-session -d -s orgfull -x 120 -y 30 \
  'TERM=xterm-256color emacs -nw 2>/tmp/emacs-err.log'
sleep 80
```

Open the file **after** startup, for the reason in Part 4.5:

```bash
tmux send-keys -t orgfull Escape; sleep 1
tmux send-keys -t orgfull 'M-:'; sleep 1
tmux send-keys -t orgfull '(progn (find-file "/tmp/faces-test.org") (org-show-all) (font-lock-ensure))' Enter
sleep 4
tmux capture-pane -e -p -t orgfull | head -3 | cat -v
```

Compare the two captures.
Baseline styled and real config plain means a config bug.
Both plain means an environment bug.

### 7.5 Step 4: read the live variables

`M-:` evaluates an expression in the running Emacs, so you inspect the true state:

```elisp
(message "FONTIFY=%s HIDE=%s ALIST=%s FLK=%s"
         org-fontify-emphasized-text
         org-hide-emphasis-markers
         (length org-emphasis-alist)
         font-lock-mode)
```

Evaluate it **inside the Org buffer**.
Buffer-local values differ per buffer, and reading them in `*Warnings*` returns misleading results.
That happened during this session, and it produced a false `REMAP=nil`.

To target a buffer explicitly:

```elisp
(with-current-buffer "faces-test.org" (message "REMAP=%S" face-remapping-alist))
```

### 7.6 Step 5: export a buffer that resists copying

```elisp
(with-temp-file "/tmp/dump.txt"
  (insert (with-current-buffer "*Warnings*" (buffer-string))))
```

Useful for `*Warnings*`, `*Messages*`, and the Spacemacs home buffer with its error lines.

### 7.7 Two gotchas

Org files open folded, and a folded heading hides the text you want to inspect.
Run `M-x org-show-all` before you capture.

Escape codes vanish without `-e`.
`tmux capture-pane -p` returns plain text, which tells you nothing about styling.

### 7.8 Inspecting from inside Emacs, without tmux

Three commands answer most questions.

`C-u C-x =` on a character reports every text property, including `face` and `invisible`.
No `face` entry on a bold word means stage 1, 2, or 3 failed.

`M-x describe-face RET bold RET` reports the resolved attributes.
Attributes present while the screen looks plain points at stage 5, or at a face remap.

`M-x list-faces-display` shows every face as your current frame renders it.
Run it in the terminal frame to see what your terminal really does.

---

## Part 8: Glossary

**Attribute**
One appearance property of a face, for example `:weight`, `:slant`, or `:foreground`.

**Byte compilation**
Translation of `.el` source into faster `.elc` bytecode.
`load` with an explicit `.el` path ignores the `.elc`, as Part 4.3 proves.

**Dotfile (Spacemacs)**
Your configuration file, either `~/.spacemacs` or `~/.spacemacs.d/init.el`.
Spacemacs picks exactly one, and `init.el` wins when it exists.

**Face**
A named bundle of appearance attributes, for example `bold` or `org-code`.

**Face remapping**
A buffer-local override of a face, held in `face-remapping-alist`.
`face-remap-add-relative` adds an entry.
It acts at stage 4, after a face is attached.

**font-lock**
The syntax highlighting engine.
It matches patterns from the major mode and attaches `face` properties.

**Frame**
One Emacs window at the operating system level.
A frame is graphical or terminal-based, and `display-graphic-p` reports which.

**Invisible property**
A text property that hides characters from redisplay.
The characters remain in the buffer and in the file.
Org hides emphasis markers this way.

**Layer variable**
A `key value` pair inside a Spacemacs `(layer :variables ...)` form.
Spacemacs applies them with `set-default`, in order, before the packages load.
The last duplicate wins.

**Overlay**
A second mechanism for appearance, attached to a region instead of to characters.
Overlays win over text properties, and `flycheck` and `hl-line` use them.
Org emphasis uses text properties, not overlays.

**Redisplay**
The Emacs stage that converts buffer content plus faces into output.

**SGR code**
"Select Graphic Rendition", the ANSI escape sequence family for styling terminal text.
`ESC[1m` starts bold, and `ESC[0m` resets everything.

**terminfo**
The system database of terminal capabilities, keyed by `$TERM`.
`infocmp` prints an entry.
Emacs consults it to learn whether the terminal supports italic, bold, and colors.

**Text property**
Metadata attached to characters in a buffer, for example `face` or `invisible`.

**TTY / terminal frame**
An Emacs frame inside a terminal emulator, started with `emacs -nw`.

**True color**
24-bit color support, enabled when `COLORTERM` equals `truecolor` or `24bit`.
Emacs then emits `ESC[38;2;R;G;Bm`.

---

## Part 9: Lessons to reuse

1. Identify the failing stage before you fix anything.
Stage 1 and stage 5 failures look identical, and they need opposite fixes.

2. Measure a baseline with `emacs -Q`.
It separates your config from your environment in one step.

3. Read the bytes, not the screen.
`tmux capture-pane -e` shows whether Emacs asked for styling.

4. Read variables from the live session, in the correct buffer.
A grep of a config file describes intent, and the running value describes reality.

5. Confirm which file Emacs loads.
Your `~/.spacemacs` looks authoritative, and Emacs ignores it.

6. Treat duplicate layer variables as a bug.
Spacemacs stays silent, and the last value wins.

7. Distrust hardcoded colors.
`brightwhite` disappears on a light theme.

8. Check the Spacemacs home buffer for dotfile errors.
`user-config` stops silently at the first error.

---

## Part 10: Further reading

### 10.1 Your local manuals are missing

This machine has no Emacs manual, no Elisp reference, and no Org manual.
Only two Info manuals exist here, `efaq` and `transient`.
So `C-h i` shows almost nothing, and every `(elisp) ...` reference fails.

Install them once:

```bash
sudo apt install emacs-common-non-dfsg
```

The package holds the GNU manuals, and version `1:30.1+1-1` matches your Emacs 30.1.

After installation, read these nodes with `C-h i`:

- `(elisp) Search-based Fontification`, for the font-lock rule format from Part 1.2.
- `(elisp) Special Properties`, for the `face` and `invisible` properties from Part 2.4.
- `(elisp) Face Remapping`, for `face-remapping-alist` from Part 6.
- `(elisp) Text Terminal Colors`, for the terminal color handling from Part 3.

### 10.2 What already works locally

Your `emacs-el` package provides the Lisp sources, so self-documentation works today.

- `C-h v org-fontify-emphasized-text` shows the value, the docstring, and the defining file.
- `C-h f org-do-emphasis-faces` shows the matcher, with a link to `org.el`.
- `M-x list-faces-display` renders every face in the current frame.

These three commands answer most questions without any manual.

### 10.3 Online

- Org manual, "Emphasis and Monospace": https://orgmode.org/manual/Emphasis-and-Monospace.html
  It documents both switches, and it confirms that `nil` stops highlighting only.
- Emacs manuals index: https://www.gnu.org/software/emacs/manual/

### 10.4 Sources on this machine

Open these once, and the pipeline stops feeling magic.

- `/usr/share/emacs/30.1/lisp/org/org.el.gz`, lines 3671, 3710, 5226, and 5940.
- `~/.emacs.d/core/core-dotspacemacs.el`, lines 46 to 80, 838, and 1070 to 1086.
- `~/.emacs.d/core/core-configuration-layer.el`, line 1589.
- `~/.emacs.d/core/core-spacemacs.el`, lines 286 to 304.

The `.gz` suffix is not an obstacle.
Emacs opens compressed Lisp sources directly.

Sources on this machine, worth opening once:

- `/usr/share/emacs/30.1/lisp/org/org.el.gz`, lines 3671, 3710, 5226, and 5940.
- `~/.emacs.d/core/core-dotspacemacs.el`, lines 46 to 80, 838, and 1070 to 1086.
- `~/.emacs.d/core/core-configuration-layer.el`, line 1589.
- `~/.emacs.d/core/core-spacemacs.el`, lines 286 to 304.
