#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Org Work Journal
# @raycast.mode silent
# @raycast.packageName Org
# @raycast.argument1 { "type": "text", "placeholder": "Entry" }

# Optional parameters:
# @raycast.icon 💼
# @raycast.description Entry under "Weekly" in this week's ~/Documents/work-journal file

set -euo pipefail
"$HOME/.local/bin/org-quick-capture" work "$1"
echo "Org Work Journal: $1"
