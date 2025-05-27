#!/usr/bin/env bash

# tmex-in-pane.sh - Run tmex inside a specific pane (splits that pane)
# Usage: ./tmex-in-pane.sh <layout> [target-pane]

set -euo pipefail

LAYOUT="${1:-111}"
TARGET_PANE="${2:-%1}"

echo "Running tmex layout '${LAYOUT}' in pane ${TARGET_PANE}"
echo "Note: This will split the target pane, not create a new session"

# Send the tmex command without a session name
# When run inside a tmux pane, tmex splits the current pane
tmux send-keys -t "${TARGET_PANE}" "tmex ${LAYOUT}" Enter