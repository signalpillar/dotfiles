#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Org Journal
# @raycast.mode silent
# @raycast.packageName Org
# @raycast.argument1 { "type": "text", "placeholder": "Entry" }

# Optional parameters:
# @raycast.icon 📓
# @raycast.description Entry in today's datetree in ~/gsync-docs/today.org

set -euo pipefail
"$HOME/.local/bin/org-quick-capture" journal "$1"
echo "Org Journal: $1"
