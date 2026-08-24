# Tmux activity highlight: status bar vs C-b w

A short post-mortem from a real question about this chezmoi tmux config.

The symptom: the status bar paints windows with new output in yellow.
`C-b w` lists the same windows and does not change their color.

The cause is not a broken `monitor-activity` setting.
Two different UIs read the same activity flag.
Only the status line applies `window-status-activity-style`.

Facts here come from tmux 3.5a on this machine and from `dot_tmux.conf.tmpl`.
File and line references point to real source in this repo.

---

## Part 0: The 60-second version

Tmux records activity on each window when `monitor-activity` is on.
The status bar and `C-b w` both see that record.
They do not share a style.

The status bar expands `status-format`.
That format checks `window_activity_flag` and then applies `window-status-activity-style`.
Your theme sets that style to yellow.

`C-b w` runs `choose-tree -Zw`.
The default tree format prints `#{window_name}#{window_flags}`.
An activity window gets a `#` after the name.
The tree does not read `window-status-activity-style` unless `-F` adds it.

This config now sets `@choose-tree-format` and binds `w`, `S`, and `V` to that format.

```
output in unfocused window
        |
        v
monitor-activity sets window_activity_flag=1
        |
        +---> status-format --> window-status-activity-style
        |
        +---> choose-tree -F --> same style via @choose-tree-format
```

---

## Part 1: Post-mortem

### 1.1 Symptom

Windows with new output turn yellow in the session status bar.
`C-b w` opens the window tree.
Nothing in that list uses the same yellow.

### 1.2 First hypothesis

The window picker reuses the status-bar window list.
A missing option, or a theme gap, hides the activity style in that picker.

That hypothesis is wrong.

### 1.3 What the live server actually does

`C-b w` is the default prefix bind, not a line in this config:

```text
bind-key -T prefix w choose-tree -Zw
```

`choose-tree` is a tree mode.
It is not the status-line window list.

`monitor-activity` is on in `dot_tmux.conf.tmpl`:

```82:84:dot_tmux.conf.tmpl
## set window notifications
set-option -g visual-activity on
set-window-option -g monitor-activity on
```

The status-bar yellow comes from a different option:

```112:120:dot_tmux.conf.tmpl
# Window status
set -g window-status-format " #I:#W#F "
set -g window-status-current-format " #I:#W#F "

# Current window status
set -g window-status-current-style bg=red,fg=black

# Window with activity status
set -g window-status-activity-style bg=black,fg=yellow
```

A live `list-windows` on this server showed the flag already set.
Example: `vmd-manager`, `btop`, and `pi-job` had `window_activity_flag=1` and flags `#`.
The data is present.
The tree does not paint it.

The default `choose-tree` format in tmux 3.5a includes `#{window_flags}` after the window name.
Activity is the `#` flag.
`*` is the current window.
`-` is the last window.
The `#` is easy to miss next to the yellow you see in the bar.

### 1.4 Failing stage

```
[1] Child writes to an unfocused window
[2] monitor-activity sets window_activity_flag
[3] status-format applies window-status-activity-style
[4] choose-tree expands its own -F format
```

Stage 2 and stage 3 work.
Stage 4 never reads the status style.
The failure is the UI boundary, not a dead option.

### 1.5 Fix

`dot_tmux.conf.tmpl` now sets one format and binds `w`, `S`, and `V` to it.
The format reads `window-status-activity-style`, so the tree stays in sync with the bar.

Reload after apply:

```bash
tmux source-file ~/.tmux.conf
```

### 1.6 Why a style-only change fails

`set -g window-status-activity-style` already has a value.
A second status-style tweak cannot reach `choose-tree`.
`choose-tree` only takes style from `-F` (or from `mode-style` for the selected row).

`mode-style` in this config is `bg=red`.
That style marks the current selection in the tree.
It does not mark activity.

---

## Part 2: Wrong fixes

| Change | Why it fails |
| --- | --- |
| Raise `window-status-activity-style` contrast | Status bar only. The tree never reads this option. |
| Add `#F` to `window-status-format` | The bar already has `#F`. The tree uses a different format. |
| Toggle `visual-activity` | That option shows a message. It does not style `choose-tree`. |
| Change `mode-style` | That style paints the selected row, not every activity window. |

---

## Part 3: Terms used here

**Activity.** Output in a window that is not the current window, while `monitor-activity` is on.

**`window_activity_flag`.** Format variable. Value is `1` until you visit the window.

**`window-status-activity-style`.** Style for activity in the status-line window list only.

**`choose-tree`.** Interactive tree used by `C-b w`. Default bind is `choose-tree -Zw`.
