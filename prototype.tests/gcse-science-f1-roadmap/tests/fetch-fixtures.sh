#!/bin/sh
set -eu

fixture_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/fixtures"
mkdir -p "$fixture_dir"
curl -L --fail --silent --show-error "https://jelly-ui.com/dist/jelly.js" -o "$fixture_dir/jelly.js"
curl -L --fail --silent --show-error "https://unpkg.com/open-props" -o "$fixture_dir/open-props.css"
curl -L --fail --silent --show-error "https://cdn.jsdelivr.net/npm/marked@15.0.12/marked.min.js" -o "$fixture_dir/marked.js"

actual_jelly="$(shasum -a 256 "$fixture_dir/jelly.js" | cut -d ' ' -f 1)"
actual_props="$(shasum -a 256 "$fixture_dir/open-props.css" | cut -d ' ' -f 1)"
actual_marked="$(shasum -a 256 "$fixture_dir/marked.js" | cut -d ' ' -f 1)"
test "$actual_jelly" = "68af6000710c7b8bd22d3ed8e337308fb20d767511483d1458f27288d34950ef"
test "$actual_props" = "4bc9bec663eb7fef2cb7680d2983e21f0f79778a61a77bee74224030dc981f5c"
test "$actual_marked" = "3e7e7d7feb3e5d58cb6c804f68ab5c24cc7e5eb6270fd6e5cbb9124739217d0c"
