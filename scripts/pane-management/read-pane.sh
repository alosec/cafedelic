#!/bin/bash
# read-pane.sh - Read the last N lines from a named pane
# Usage: ./read-pane.sh NAME [LINES]

set -e

NAME="$1"
LINES="${2:-100}"  # Default to 100 lines

if [ -z "$NAME" ]; then
    echo "Error: Missing pane name"
    echo "Usage: $0 NAME [LINES]"
    exit 1
fi

# Validate lines is a number
if ! [[ "$LINES" =~ ^[0-9]+$ ]]; then
    echo "Error: LINES must be a positive number"
    exit 1
fi

# Find the pane coordinates
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COORDS=$("$SCRIPT_DIR/../utils/find-pane.sh" "$NAME")

if [ $? -ne 0 ]; then
    echo "$COORDS"  # Error message from find-pane
    exit 1
fi

# Capture pane content
# -p: output to stdout
# -S: start line (negative = from end)
# -E: end line (- = to end)
OUTPUT=$(tmux capture-pane -t "$COORDS" -p -S "-${LINES}" -E -)

if [ -z "$OUTPUT" ]; then
    echo "(empty)"
else
    echo "$OUTPUT"
fi
