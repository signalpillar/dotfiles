#!/usr/bin/env bash
set -euo pipefail

PLUGIN_DIR="${HOME}/.tmux/plugins"
mkdir -p "${PLUGIN_DIR}"

clone_plugin() {
  local name="$1"
  local url="$2"
  local dest="${PLUGIN_DIR}/${name}"
  if [ ! -d "${dest}/.git" ]; then
    echo "tmux plugins: cloning ${name}..."
    git clone --depth 1 "${url}" "${dest}"
  else
    echo "tmux plugins: ${name} already present"
  fi
}

clone_plugin tpm https://github.com/tmux-plugins/tpm.git
clone_plugin tmux-resurrect https://github.com/tmux-plugins/tmux-resurrect.git
clone_plugin tmux-continuum https://github.com/tmux-plugins/tmux-continuum.git

echo "tmux plugins: ready"
