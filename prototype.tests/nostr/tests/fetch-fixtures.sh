#!/bin/sh
set -eu

fixture_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/fixtures"
mkdir -p "$fixture_dir"

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d ' ' -f 1; else shasum -a 256 "$1" | cut -d ' ' -f 1; fi
}

url="https://cdn.jsdelivr.net/npm/nostr-tools@2.25.2/lib/nostr.bundle.js"
curl -L --fail --silent --show-error "$url" -o "$fixture_dir/nostr.bundle.js"
test "$(sha256 "$fixture_dir/nostr.bundle.js")" = "a7c4846fde84a0d6ce7596f3fcfc88435d29fdcd616887205ab97f9a16bc6407" || { echo "Checksum mismatch: nostr.bundle.js" >&2; exit 1; }
