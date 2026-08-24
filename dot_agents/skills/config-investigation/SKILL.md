---
name: config-investigation
description: >-
  Investigate dotfile, chezmoi, Emacs, and terminal config bugs with evidence
  (baseline tests, live state, escape codes). Use when a config change does
  nothing, behavior differs between GUI and TTY, chezmoi sync drops blocks, or
  the user asks why a setting failed. Always ends by asking whether to write
  educational material. Written guides default to public-safe placeholders.
license: personal
compatibility: all
metadata:
  audience: developer
---

# Config investigation

Run this skill for config bugs where the symptom hides the cause.
Examples: Org emphasis looks plain in `emacs -nw`, a dotfile edit has no effect, chezmoi `add` reverts newer blocks.

## Non-negotiable close

Before you finish, **always** run `AskQuestion`.
Do not skip this step.
Do not substitute a free-text "want a doc?" in the summary.

Use this form:

```yaml
title: "Educational material?"
questions:
  - id: write_doc
    prompt: "Create educational material from this investigation?"
    options:
      - id: "yes"
        label: "Yes - write a guide (Recommended)"
      - id: "no"
        label: "No - fix only, no doc"
  - id: doc_location
    prompt: "If yes, where should the guide live?"
    allow_multiple: false
    options:
      - id: next_to_config
        label: "Next to the config it explains (e.g. ~/.spacemacs.d/docs/) (Recommended)"
      - id: chezmoi_docs
        label: "In chezmoi source under a docs/ folder"
      - id: repo_docs
        label: "In the current repo docs/ or syllabus/"
      - id: tmp
        label: "/tmp (throwaway)"
  - id: doc_extras
    prompt: "What should the guide include?"
    allow_multiple: true
    options:
      - id: postmortem
        label: "Post-mortem of what failed and why (Recommended)"
      - id: pipeline
        label: "Pipeline / architecture diagram (ASCII)"
      - id: glossary
        label: "Glossary of terms"
      - id: harness
        label: "Reusable test harness (commands you ran)"
      - id: exercises
        label: "Hands-on exercises"
```

If the user picks **Yes**, write the guide before you close.
Follow ASD-STE100 from `AGENTS.md` for technical prose.
Put each full sentence on its own line in long Markdown.
Cite repo-relative paths and line numbers for managed files.
Verify commands and Info node names before you cite them.
Write the guide as if the dest repo is public.
Keep live host facts in the chat.
Do not copy them into the file unless the user asks for a private capture.
See **Public-aware guides** below.

If the user picks **No**, give a short fix summary only.

## Workflow

Copy this checklist and track progress:

```text
Investigation progress:
- [ ] 1. Reproduce in the real environment
- [ ] 2. Run a minimal baseline
- [ ] 3. Compare baseline vs full config
- [ ] 4. Read live state (not just grep)
- [ ] 5. Confirm which file Emacs/chezmoi actually loads
- [ ] 6. Name the failing pipeline stage
- [ ] 7. Fix the smallest correct change
- [ ] 8. Re-test with evidence
- [ ] 9. If chezmoi: diff against previous source for accidental drops
- [ ] 10. AskQuestion: educational material?
- [ ] 11. If writing a guide: redact live host facts; use placeholders
```

### 1. Reproduce in the real environment

Match how the user hits the bug.

- GUI vs `emacs -nw` vs SSH + tmux are different frames.
- `TERM=dumb` in batch shells lies about color support.
- Org files open folded hide the text you need to inspect.

### 2. Run a minimal baseline

Strip user config first.

```bash
TERM=xterm-256color emacs -nw -Q --eval '...'
```

If the baseline works, the environment is fine.
The bug lives in config, not in the terminal.

### 3. Compare baseline vs full config

Run the same test with the full init.
Capture raw terminal bytes when display is involved:

```bash
tmux capture-pane -e -p -t SESSION | head -5 | cat -v
```

Look for SGR codes: `^[[1m` bold, `^[[3m` italic, `^[[4m` underline.
No codes means the app never asked for styling.
Codes present but screen looks plain means a terminal or theme issue.

Details: [reference.md](reference.md).

### 4. Read live state

Grep of a config file shows intent.
Eval in the running session shows reality.

```elisp
(message "VAR=%S" some-variable)
```

Rules:

- Evaluate inside the buffer under test, not in `*Warnings*` or `*Messages*`.
- For buffer-local values, use `with-current-buffer`.
- Run `C-u C-x =` on a character to see `face` and `invisible` properties.

### 5. Confirm which file loads

For Spacemacs:

- `~/.spacemacs.d/init.el` wins over `~/.spacemacs` when it exists.
- A stale `~/.spacemacs` still confuses future greps. Flag it.

For chezmoi:

```bash
chezmoi source-path
chezmoi managed | rg 'pattern'
chezmoi diff ~/.path/to/file
```

After `chezmoi add`, diff the source against the previous commit.
`chezmoi add` copies **from home into source**.
An older home file can overwrite newer chezmoi-only blocks.

Spacemacs loads `init.el` with an explicit `.el` suffix.
A byte-compiled `init.elc` does not shadow it when load uses `.el`.

### 6. Name the failing pipeline stage

For fontification and display bugs, map the failure to a stage:

```text
[1] Major mode installs font-lock rules
[2] font-lock matches patterns
[3] text properties attach (face, invisible)
[4] faces resolve (theme, face-remapping-alist)
[5] redisplay paints (font or escape codes)
```

Stage 1 and stage 5 failures look identical on screen.
Do not fix stage 5 when stage 1 is off.

Org example: `org-fontify-emphasized-text nil` removes the matcher at stage 1.
Face remapping at stage 4 cannot help.

### 7. Fix the smallest correct change

- One root cause, one focused diff.
- Remove duplicate layer variables. Spacemacs applies `:variables` in order; last wins silently.
- Do not hardcode face colors without checking the active theme background.

### 8. Re-test with evidence

Repeat the baseline/full comparison after the fix.
State what changed in the capture, not only "it works now".

### 9. Chezmoi sync guard

When the fix touches managed dotfiles:

```bash
chezmoi add ~/.path/to/file
cd "$(chezmoi source-path)"
git diff HEAD~1 -- path/to/file   # if a prior commit exists
```

Restore any chezmoi-only blocks the sync dropped unless the user asked to remove them.
Common victims: `COPYFILE_DISABLE`, package `:if` guards, `use-package` blocks, workarounds in `user-init`.

Then commit and push only when the user asks.

## Public-aware guides

Treat every written guide as public.
The investigation chat can hold live evidence.
The file cannot.

Keep in the guide:

- The method and the decision rule
- Commands you verified
- Option names, format variables, and vendor-documented defaults
- Repo-relative paths to managed files, with line citations

Replace live values with placeholders:

| Live fact | Public form |
| --- | --- |
| Hostname, username, home path | Drop, or write "the guest" / `~` |
| DHCP host octet, neighbor IP, MAC | `10.211.55.N`, "another neighbor" |
| IPv6 ULA or link-local | `fd00::/8`, `fe80::...` |
| Session, window, or container names | Generic names or omit |
| Patch/build that only IDs this box | Product family, or omit |
| Count of private resources | "Docker bridges", not "six bridges" |
| Capture date of a lease | Omit; say leases change |

After you write the draft, grep it for tokens from the live session: hostname, `src` IPs, `uid`, `fe80`/`fd` prefixes, window names.
If a line teaches nothing without that token, delete the line.

Example of the public form: `docs/parallels-host-reachable-ip.md`.

## Anti-patterns

| Trap | Why it misleads |
| --- | --- |
| Grep the wrong dotfile | Spacemacs reads one file; you edited another |
| Trust `chezmoi add` blindly | Home can be older than source |
| Fix terminal faces first | Master switches can disable fontification entirely |
| Read vars outside the target buffer | Buffer-local values differ |
| Skip the baseline | You cannot separate env from config |
| Skip AskQuestion at the end | User wants the option every time |
| Paste live hostname, IP, or window names into a guide | The dest repo is public; those facts do not teach the method |

## Additional resources

- Tmux harness, chezmoi checks, Org switches: [reference.md](reference.md)
- Example guide from this workflow: `~/.spacemacs.d/docs/org-emphasis-tty-explained.md`
- Public-aware example: `docs/parallels-host-reachable-ip.md`
