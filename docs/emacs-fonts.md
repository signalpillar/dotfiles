# Emacs fonts

Chezmoi installs the fonts and deploys the Spacemacs configuration.

## Font roles

GUI Emacs uses a fixed-pitch font for code, tables, and source blocks.

GUI Emacs uses a variable-pitch font for Org and Markdown prose.

This separation keeps code aligned and improves document reading.

## Available pairs

The mapping lives in `dot_spacemacs.d/init.el` as `my/font-pairs`.

| Pair | Fixed pitch | Variable pitch | Use |
| --- | --- | --- | --- |
| `ibm-plex` | IBM Plex Mono | IBM Plex Sans | Default balanced pair |
| `jetbrains-inter` | JetBrainsMono Nerd Font | Inter | Programming-focused pair |
| `iosevka-source-sans` | Iosevka Nerd Font | Source Sans 3 | Compact code and readable prose |
| `fira-ibm` | FiraCode Nerd Font | IBM Plex Sans | Ligatures and readable prose |

Change `my/default-font-pair` to select a pair.

For example:

```elisp
(defconst my/default-font-pair 'jetbrains-inter
  "Font pair used by GUI Emacs.")
```

Restart GUI Emacs after changing the pair.

## Font sizes

The default size is `15.0` points.

Increase the size to `16.0` or `17.0` for a larger display.

Fontaine presets also provide `medium` and `large` sizes.

Use `M-x fontaine-set-preset` to select a preset during a session.

## Reproduce the setup

On macOS, `Brewfile` installs the programming and prose fonts.

On Linux, `run_onchange_setup_box.sh.tmpl` installs the matching Ubuntu packages.

Apply the chezmoi source after reviewing its diff:

```sh
chezmoi diff
chezmoi apply
```

Open an Org or Markdown buffer in a GUI frame to verify the result.

Code, tables, and inline code remain fixed pitch.

## Org emphasis

Keep `org-fontify-emphasized-text` at `t` so Org emphasis faces attach in TTY frames.
