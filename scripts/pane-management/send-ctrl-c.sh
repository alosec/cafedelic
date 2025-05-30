#!/bin/bash
# send-ctrl-c.sh - Send Ctrl-C to a named pane with optional double-tap
# Usage: ./send-ctrl-c.sh NAME [DOUBLE_TAP]

set -e

NAME="$1"
DOUBLE_TAP="${2:-false}"  # Default to false

if [ -z "$NAME" ]; then
    echo "Error: Missing pane name"
    echo "Usage: $0 NAME [DOUBLE_TAP]"
    echo "DOUBLE_TAP: true/false (default: false)"
    exit 1
fi

# Find the pane coordinates
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COORDS=$("$SCRIPT_DIR/../utils/find-pane.sh" "$NAME")

if [ $? -ne 0 ]; then
    echo "$COORDS"  # Error message from find-pane
    exit 1
fi

# Send Ctrl-C
tmux send-keys -t "$COORDS" C-c

# For Claude Code or similar apps, send C-c twice quickly
if [ "$DOUBLE_TAP" = "true" ]; then
    sleep 0.1
    tmux send-keys -t "$COORDS" C-c
    echo "Sent double Ctrl-C to pane '${NAME}' (${COORDS})"
else
    echo "Sent Ctrl-C to pane '${NAME}' (${COORDS})"
fi
