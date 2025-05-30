#!/bin/bash
# send-to-pane.sh - Send text to a named pane
# Usage: ./send-to-pane.sh NAME TEXT

set -e

NAME="$1"
TEXT="$2"

if [ -z "$NAME" ] || [ -z "$TEXT" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 NAME TEXT"
    exit 1
fi

# Find the pane coordinates
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COORDS=$("$SCRIPT_DIR/../utils/find-pane.sh" "$NAME")

if [ $? -ne 0 ]; then
    echo "$COORDS"  # Error message from find-pane
    exit 1
fi

# Send the text to the pane
# Use printf to handle special characters properly
printf '%s' "$TEXT" | tmux load-buffer -
tmux paste-buffer -t "$COORDS"

echo "Sent text to pane '${NAME}' (${COORDS})"
