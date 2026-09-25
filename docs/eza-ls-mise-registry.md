# Why `ls` printed mise WARN lines and then `eza: command not found`

A short post-mortem from a Darwin zsh prompt that mixed two failures.

The prompt looked like one error.
It was two pipelines that share one shell hook.

This guide uses public placeholders.
It cites managed files in this repo.

---

## Part 0: The 60-second version

Zsh does not run `/bin/ls` for the `ls` token.
It expands an alias to `eza`.
Homebrew owns `eza` through [Brewfile](../Brewfile).
If `brew bundle` has not installed that formula, the alias fails.

`mise activate` still runs first on every command.
It resolves every tool in [dot_config/mise/config.toml](../dot_config/mise/config.toml).
A short name that is missing from the installed mise registry prints `WARN` on stderr.
That warning is not the reason `eza` is missing.

```
typed: ls
        |
        v
mise hook-env  -->  resolve every [tools] entry
        |                 |
        |                 +-- short name in registry --> shim PATH
        |                 +-- short name missing     --> WARN on stderr
        v
alias ls --> eza --classify=always
        |
        +-- eza on PATH (brew) --> directory listing
        +-- eza missing        --> command not found
```

---

## Part 1: Post-mortem

### 1.1 Symptom

The guest types `ls`.
The shell prints mise `WARN` lines about `codex` and `antigravity-cli`.
Then zsh prints `command not found: eza`.

### 1.2 First hypothesis

Mise failed to install `eza`, so the listing command is gone.

That hypothesis is wrong.
Mise does not list `eza` in `dot_config/mise/config.toml`.
The alias points at a Homebrew formula.

### 1.3 Baseline

Run the system binary, not the alias:

```bash
/bin/ls
```

If `/bin/ls` lists the directory, the terminal and filesystem are fine.
The bug is in the alias and the package install, not in `ls` itself.

Confirm the alias and the keg:

```bash
type ls
brew info eza
```

Expect `ls` to be an alias for `eza`.
Expect `brew info eza` to say `Not installed` when the keg is missing.

### 1.4 Why brew did not install `eza`

[Brewfile](../Brewfile) already names the formula:

```30:30:Brewfile
brew "eza"
```

Darwin apply installs that file with [run_onchange_brew.sh.tmpl](../run_onchange_brew.sh.tmpl).
The script runs `brew bundle install` when the Brewfile hash changes:

```22:23:run_onchange_brew.sh.tmpl
brew bundle install --no-upgrade --file="${BREWFILE}"
echo "brew: bundle install from ${BREWFILE}"
```

If that onchange script is not in the applied chezmoi source, `brew bundle` does not run.
The Brewfile then describes intent only.
The Cellar can still lack `eza`.

Install the missing formula with:

```bash
brew install eza
```

Or apply chezmoi so the onchange script runs `brew bundle install`.

### 1.5 Why mise printed WARN before the alias failed

[dot_zshrc](../dot_zshrc) activates mise when the binary exists:

```66:68:dot_zshrc
if command -v mise &> /dev/null; then
  eval "$(mise activate zsh)"
fi
```

Activation installs `hook-env`.
That hook runs for every command, including `ls`.
It reads the whole `[tools]` table.

A short name such as `codex = "latest"` works only if that name is in the **installed** mise registry.
Mise 2025.9.19 has no `codex` or `antigravity-cli` short name.
Newer mise releases add those names later.

The hook then prints:

```text
codex not found in mise tool registry
antigravity-cli not found in mise tool registry
```

Those lines are stderr from `hook-env`.
They do not install `eza`.

### 1.6 Explicit backends

This repo pins tools that lack a short name with a backend prefix.
That pattern already exists for Task and Stripe CLI.

```2:11:dot_config/mise/config.toml
1password-cli = "latest"
"aqua:go-task/task" = "latest"
"aqua:openai/codex" = "latest"
"aqua:stripe/stripe-cli" = "latest"
awscli = "latest"
babashka = "latest"
bun = "latest"
chezmoi = "latest"
cmake = "3.31.5"
"github:google-antigravity/antigravity-cli" = "latest"
```

`mise registry NAME` shows the short name.
If that command errors, write `aqua:…` or `github:owner/repo` instead of the short name.

After the pin, install:

```bash
mise install
```

Confirm the hook is quiet:

```bash
mise hook-env -s zsh 2>&1
```

The output must not contain `not found in mise tool registry`.

The GitHub backend for Antigravity CLI exposes `antigravity`.
The Homebrew cask uses `agy`.
Treat those as two command names, not one.

### 1.7 eza `-F` is not GNU `ls -F`

eza 0.23 parses `-F` as `--classify [<WHEN>]`.
`WHEN` is `always`, `auto`, or `never`.

This fails:

```bash
eza -F README.md
```

eza treats `README.md` as `WHEN`.

The shell aliases now pass an explicit value:

```39:43:dot_zshrc
# eza 0.23 treats bare -F as --classify WHEN, so it eats the path.
alias ll='eza -al --classify=always'
alias la='eza -A'
alias l='eza --classify=always'
alias ls='eza --classify=always'
```

[dot_bashrc](../dot_bashrc) uses the same aliases.
Reload the shell after apply:

```bash
source ~/.zshrc
```

---

## Part 2: Decision rule

Separate the stderr owner from the command owner.

| Observation | Owner | Action |
| --- | --- | --- |
| `not found in mise tool registry` | `hook-env` + `[tools]` | Use an explicit backend, or upgrade mise until `mise registry NAME` works |
| `command not found: eza` | zsh alias + Homebrew | Install `eza`, or stop the alias |
| `/bin/ls` works, `ls` fails | alias only | Do not debug the kernel `ls` |
| Brewfile lists a formula, `brew info` says not installed | chezmoi onchange / `brew bundle` | Apply the brew onchange script, or `brew install` that formula |

Do not add `eza` to mise to silence a missing brew keg.
Do not remove mise tools to fix a missing `eza` binary unless those tools are the WARN source.

---

## Part 3: Glossary

**Short name.**
A bare key in `[tools]`, such as `jq`.
Mise maps it through its bundled registry.

**Registry.**
The table inside the installed mise binary that maps short names to backends.
`mise registry` lists it.
A newer mise can contain names that an older binary lacks.

**Explicit backend.**
A quoted tool id with a prefix: `aqua:owner/name`, `github:owner/repo`, `npm:@scope/pkg`.
Mise does not need a short-name row for that id.

**hook-env.**
The function `mise activate` injects into the shell.
It updates `PATH` from `[tools]` on each command.
Failures here print before the command runs.

**Brewfile.**
The Homebrew bundle list in this repo.
It is a declaration.
`brew bundle install` is the apply step.

**Classify WHEN.**
eza flag `--classify` with `always`, `auto`, or `never`.
GNU `ls -F` always appends type marks.
Bare `eza -F` on 0.23 does not mean GNU `-F`.
