#!/bin/sh
set -eu

fixture_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/fixtures"
mkdir -p "$fixture_dir"

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d ' ' -f 1; else shasum -a 256 "$1" | cut -d ' ' -f 1; fi
}

# name url sha256
while read -r name url sum; do
  curl -L --fail --silent --show-error "$url" -o "$fixture_dir/$name"
  test "$(sha256 "$fixture_dir/$name")" = "$sum" || { echo "Checksum mismatch: $name" >&2; exit 1; }
done <<'LIST'
pico.min.css https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css fbc9a63fc9fc9f72d12fd7fc9806e11fa9f77ae4f9cad146b27003a1119ba3db
datascript.js https://cdn.jsdelivr.net/npm/datascript@1.8.1/datascript.js cec7547584d6807dcd58a194c341f7a1a49d6e541d3b77738b8c63906f68fdde
cytoscape.min.js https://cdn.jsdelivr.net/npm/cytoscape@3.30.4/dist/cytoscape.min.js 1bb5340e549511e111b31e5684872c949ad33d40ea5dba0ad8e7d90c62c7b3b9
layout-base.js https://cdn.jsdelivr.net/npm/layout-base@2.0.1/layout-base.js ec15ab5df9af3f20708f4faab994accf91cda71848cd5bb10a23432cc50b6745
cose-base.js https://cdn.jsdelivr.net/npm/cose-base@2.2.0/cose-base.js 7cae9509bd36235a63a85e71c8d9fa2cd0bc1d0c1ecc5b5a737976f39d040ddf
cytoscape-fcose.js https://cdn.jsdelivr.net/npm/cytoscape-fcose@2.2.0/cytoscape-fcose.js 4b1cab218d74996aa59cd8473f9239cc6398b8c1774d84d7e59ad9a68959cb57
LIST
