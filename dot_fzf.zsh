# Setup fzf
# ---------

if [[ -d "/opt/homebrew/opt/fzf" ]]; then
  FZF_BASE="/opt/homebrew/opt/fzf"
elif [[ -d "/usr/local/opt/fzf" ]]; then
  FZF_BASE="/usr/local/opt/fzf"
else
  # Fallback to brew --prefix if available
  if command -v brew >/dev/null; then
    FZF_BASE="$(brew --prefix fzf)"
  fi
fi

if [[ -n "$FZF_BASE" ]]; then
  if [[ ! "$PATH" == *"$FZF_BASE/bin"* ]]; then
    export PATH="${PATH:+${PATH}:}$FZF_BASE/bin"
  fi

  # Auto-completion
  # ---------------
  [[ $- == *i* ]] && source "$FZF_BASE/shell/completion.zsh" 2> /dev/null

  # Key bindings
  # ------------
  if [[ -f "$FZF_BASE/shell/key-bindings.zsh" ]]; then
    source "$FZF_BASE/shell/key-bindings.zsh"
  fi
fi
