# Why dired-quick-sort said `insert-directory-program` is not GNU ls

A short post-mortem from a startup warning on this Spacemacs setup.

The warning text:

```text
`insert-directory-program' does
not point to GNU ls.  Please set `insert-directory-program' to GNU ls.  The
package `dired-quick-sort' will not work and thus is not set up by
`dired-quick-sort-setup'.
```

---

## Part 0: 60-second version

Ubuntu 25.10 points `/usr/bin/ls` at **uutils coreutils**, the Rust rewrite.

`dired-quick-sort` runs `ls --version` and looks for the string `GNU`.
uutils prints no `GNU`, so the package warns and skips setup.

GNU coreutils is still installed. It uses prefixed names, so GNU ls is `gnuls`.

`dotspacemacs/user-init` now points `insert-directory-program` at the first
candidate that reports `GNU`.

---

## Part 1: The wrong first guess

The obvious suspect was the shell alias:

```text
alias ls='eza --classify=always'
```

That alias is not the cause.

Emacs does not read shell aliases.
It resolves `insert-directory-program` through `exec-path` and `call-process`.
`call-process` runs a binary directly. No shell, no alias expansion.

Removing the `eza` alias changes nothing in Emacs.

---

## Part 2: The real cause

`/usr/bin/ls` is a symlink into the Rust coreutils build:

```text
/usr/bin/ls -> ../lib/cargo/bin/coreutils/ls
/usr/bin/ls (uutils coreutils) 0.2.2
```

Ubuntu 25.10 ships `rust-coreutils` and `coreutils-from-uutils` by default.

The check in `dired-quick-sort.el`:

```285:287:~/.emacs.d/elpa/30.1/develop/dired-quick-sort-20260331.2219/dired-quick-sort.el
         (with-temp-buffer
           (call-process insert-directory-program nil t nil "--version")
           (string-match-p "GNU" (buffer-string))))
```

uutils prints `uutils coreutils`, so `string-match-p` fails.

---

## Part 3: GNU ls is already present

The `gnu-coreutils` package is installed next to the Rust one.
It prefixes every binary with `gnu` to avoid the name clash:

```text
/usr/bin/gnuls --version
ls (GNU coreutils) 9.5
```

Other examples: `gnucp`, `gnudate`, `gnusort`.

On macOS, Homebrew's `coreutils` uses the prefix `g`, so GNU ls is `gls`.

---

## Part 4: The Emacs fix

`dotspacemacs/user-init` picks the first candidate whose `--version` reports
`GNU`, in the order `gnuls`, `gls`, `ls`.

This runs in `user-init`, not `user-config`.
Layer configuration calls `dired-quick-sort-setup` after `user-init`.
A fix in `user-config` would run too late.

See `dot_spacemacs.d/init.el` near the `insert-directory-program` form.

The order is portable:

- Ubuntu with uutils default picks `gnuls`.
- macOS with Homebrew picks `gls`.
- Any machine with GNU as the system default falls through to `ls`.

The cost is one `call-process` at startup.

---

## Part 5: Optional system switch

Ubuntu supports a swap back to GNU as the system default:

```sh
sudo apt install coreutils-from-gnu coreutils-from-uutils- rust-coreutils-
```

apt marks `coreutils-from-uutils` and `rust-coreutils` as **essential**.
It asks for a typed confirmation phrase.
Run it in a real terminal so you can read the prompt.

The Emacs fix does not depend on this swap.
After a swap, `gnuls` disappears and the candidate list falls through to `ls`.

---

## Part 6: What else needed GNU ls

Dired uses GNU-only `ls` switches:

| Switch | Purpose |
| --- | --- |
| `--dired` | Machine-readable file name offsets for Dired |
| `--group-directories-first` | Directories before files |
| `--time-style` | Stable date column |

uutils supports some of these, but not the `GNU` version string that
`dired-quick-sort` tests. Pointing at real GNU ls fixes both concerns at once.
