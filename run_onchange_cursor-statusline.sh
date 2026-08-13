#!/usr/bin/env bash
# Merge personal Cursor CLI statusLine settings into ~/.cursor/cli-config.json.
# Do not manage cli-config.json in chezmoi: it holds machine/account/company state
# (auth, team, MCP allowlists, caches). This script only writes the statusLine key.

set -euo pipefail

CFG="${HOME}/.cursor/cli-config.json"
STATUS_LINE_JSON='{"type":"command","command":"~/.cursor/statusline.sh","padding":2}'

mkdir -p "${HOME}/.cursor"

if [[ ! -f "${CFG}" ]]; then
  printf '%s\n' "{\"version\":1,\"statusLine\":${STATUS_LINE_JSON}}" >"${CFG}"
  echo "Created ${CFG} with statusLine only."
  exit 0
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to merge statusLine into ${CFG}" >&2
  exit 1
fi

tmp="$(mktemp)"
# shellcheck disable=SC2016
jq --argjson statusLine "${STATUS_LINE_JSON}" '.statusLine = $statusLine' "${CFG}" >"${tmp}"
mv "${tmp}" "${CFG}"
echo "Merged statusLine into ${CFG}."
