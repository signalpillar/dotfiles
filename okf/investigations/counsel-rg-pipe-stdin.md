---
type: Investigation
title: "Spacemacs SPC / showed no results: rg waited on a pipe"
description: "Why counsel-rg hung after a start-process advice forced pipes on macOS, and why an explicit path argument fixes it."
tags: [emacs, spacemacs, ivy, counsel, ripgrep, process]
status: stable
sources:
  - id: dot-spacemacs-d-init-el
    resource: /dot_spacemacs.d/init.el
---

# Spacemacs `SPC /` showed no results: rg waited on a pipe

A post-mortem from a real debug session on this Spacemacs setup.

## Symptom

`SPC /` (`spacemacs/search-project-auto`) opens the `rg from [...]:` prompt.
You type a query.
The candidate list stays empty.
RET does nothing.
No error appears in `*Messages*`.

## Cause in one paragraph

A `start-process` advice in `~/.spacemacs.d/init.el` forces a pipe instead of a PTY for every local subprocess on macOS.
`counsel-rg` runs `rg` without a path argument.
When `rg` gets no path and its stdin is not a terminal, it searches stdin.
Emacs never closes that pipe, so `rg` waits forever.
Ivy receives no candidates.

## Pipeline

```text
SPC /
  -> spacemacs/search-project-auto          (layers/+completion/ivy/funcs.el)
  -> spacemacs/counsel-search, tool = "rg"
  -> counsel-rg                             (counsel.el)
  -> counsel-ag-function                    builds the command from
                                            counsel-rg-base-command
  -> counsel--async-command-1
  -> start-file-process
  -> start-process                          <- advice: process-connection-type nil
  -> rg ... -i QUERY                        no path, stdin is a pipe
  -> rg reads stdin and never exits
  -> counsel--async-sentinel never fires
  -> ivy shows no candidates
```

The stage that fails is the subprocess stage.
Ivy, counsel, and the Spacemacs layer all behave as designed.

## The two pieces of config that collide

The advice:

```1027:1037:~/.spacemacs.d/init.el
  (when (eq system-type 'darwin)
    (setq magit-process-connection-type nil)
    (defun vv/darwin-use-pipe (fn &rest args)
      "Run FN with a pipe, not a PTY, when `default-directory' is local."
      (if (file-remote-p default-directory)
          (apply fn args)
        (let ((process-connection-type nil))
          (apply fn args))))
    (advice-add 'start-process :around #'vv/darwin-use-pipe)
    ;; compile uses start-file-process-shell-command, not start-process.
    (advice-add 'compilation-start :around #'vv/darwin-use-pipe))
```

The counsel default command, which has no path argument on Unix:

```elisp
(defcustom counsel-rg-base-command
  `("rg"
    "--max-columns" "240"
    "--with-filename"
    "--no-heading"
    "--line-number"
    "--color" "never"
    "%s"
    ,@(and (memq system-type '(ms-dos windows-nt))
           (list "--path-separator" "/" "."))))
```

Counsel adds `.` only on Windows.
With a PTY on stdin, `rg` sees a terminal and searches the current directory.
With a pipe on stdin, `rg` reads the pipe.

## How the cause was found

1. Reproduce in the real environment.
   Start `emacs -nw` with the full config inside tmux.
   Run `M-x spacemacs/search-project-auto`, type a query, capture the pane.
   The pane showed the prompt and no candidates.

2. Check the easy suspects.
   `(executable-find "rg")` returned the Homebrew binary.
   `ivy-more-chars-alist` was the default `((t . 3))`.
   `*Messages*` had no `post-command-hook` error.

3. Read live state instead of code.
   A timer wrote ivy internals to a file while the prompt was open:

   ```elisp
   (run-with-timer 10 nil
     (lambda ()
       (with-temp-file "/tmp/ivy-state.txt"
         (prin1 (list :text ivy-text
                      :cands ivy--all-candidates
                      :cmd counsel--async-last-command
                      :procs (mapcar (lambda (p)
                                       (list (process-name p)
                                             (process-status p)
                                             (process-command p)))
                                     (process-list)))
                (current-buffer)))))
   ```

   The ` *counsel*` process had status `run` after 10 seconds.
   The same command finished in under one second in a shell.
   The command had no path argument.

4. Prove the mechanism outside Emacs.

   ```bash
   (sleep 2; echo "needle in stdin") | rg --color never -i needle
   # prints: <stdin>:1:needle in stdin

   (sleep 2; echo "x") | rg --color never -i needle .
   # searches the directory, ignores stdin
   ```

5. Confirm inside Emacs.
   `(advice-remove 'start-process #'vv/darwin-use-pipe)` and rerun: results appeared at once.
   Re-add the advice and append `"."` to `counsel-rg-base-command`: results appeared at once.

## Fix

Keep the advice.
Give `rg` an explicit path so it never considers stdin:

```1038:1045:~/.spacemacs.d/init.el
    ;; With a pipe on stdin and no path argument, rg searches stdin and
    ;; waits forever. counsel-rg passes no path, so SPC / shows nothing.
    ;; An explicit "." makes rg search the directory, as counsel already
    ;; does on Windows. See okf/investigations/counsel-rg-pipe-stdin.md.
    (with-eval-after-load 'counsel
      (unless (member "." counsel-rg-base-command)
        (setq counsel-rg-base-command
              (append counsel-rg-base-command '("."))))))
```

Results now carry a `./` prefix.
Jump to result and `wgrep` edits still work.

The Spacemacs fallback command in `layers/+completion/ivy/config.el` already ends with `.`.
Only the `rg` and `ag` branches, which delegate to counsel, lacked it.

## Why not remove the advice

The advice exists because macOS PTY buffers are small and Git subprocesses stall on them.
`magit-process-connection-type nil` covers Magit on its own.
The `compilation-start` advice covers compile.
The global `start-process` advice also speeds up LSP and other subprocesses.
Removing it fixes `rg` but trades away that benefit.
Adding `.` fixes the one tool that reads stdin.

## Watch list

Any program that reads stdin when stdin is not a terminal breaks the same way under this advice:

* `fzf` without `FZF_DEFAULT_COMMAND` (`counsel-fzf`).
* `ag` without a path.
* Any filter-style tool run through `start-process` with no input.

If a counsel or async command hangs with status `run` and no output, check for a missing path argument first.

## Decision rule

When an async Emacs command shows nothing and logs no error:

1. Read `(process-status p)` and `(process-command p)` for the live process.
2. Run the exact command in a shell with a pipe on stdin: `(sleep 2; echo x) | CMD`.
3. If the shell run also waits, the tool reads stdin.
   Give it a path or close stdin.
4. If the shell run finishes, look at the sentinel and filter instead.
