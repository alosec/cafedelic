#!/usr/bin/env bash

# send-tmex.sh - Send tmex commands to arbitrary tmux panes
# Usage: ./send-tmex.sh <layout-string> <target-pane>
# Example: ./send-tmex.sh 111 %2

set -euo pipefail

# Check for required arguments
if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <layout-string> <target-pane>"
    echo "Example: $0 111 %2"
    echo ""
    echo "Layout string: tmex layout pattern (e.g., 111, 222, 12[34]5)"
    echo "Target pane: tmux pane identifier (e.g., %0, %1, session:window.pane)"
    exit 1
fi

LAYOUT_STRING="$1"
TARGET_PANE="$2"

# Check if we're in a tmux session
if [[ -z "${TMUX:-}" ]]; then
    echo "Error: Not in a tmux session"
    exit 1
fi

# Send the tmex command to the target pane
echo "Sending 'tmex ${LAYOUT_STRING}' to pane ${TARGET_PANE}"
tmux send-keys -t "${TARGET_PANE}" "tmex ${LAYOUT_STRING}" Enter