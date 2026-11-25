#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Org Todo
# @raycast.mode silent
# @raycast.packageName Org
# @raycast.argument1 { "type": "text", "placeholder": "Todo" }

# Optional parameters:
# @raycast.icon ✅
# @raycast.description TODO under "Tasks" in ~/gsync-docs/today.org

set -euo pipefail
"$HOME/.local/bin/org-quick-capture" todo "$1"
echo "Org Todo: $1"
