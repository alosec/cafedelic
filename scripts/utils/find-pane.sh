#!/bin/bash
# find-pane.sh - Find pane coordinates by name
# Usage: ./find-pane.sh NAME
# Output: SESSION:WINDOW.PANE or error

set -e

NAME="$1"

if [ -z "$NAME" ]; then
    echo "Error: Missing pane name"
    echo "Usage: $0 NAME"
    exit 1
fi

PANE_NAMES_FILE="$HOME/.cafedelic/pane-names"

if [ ! -f "$PANE_NAMES_FILE" ]; then
    echo "Error: No panes have been named yet"
    exit 1
fi

# Find the pane coordinates
COORDS=$(grep "=${NAME}$" "$PANE_NAMES_FILE" 2>/dev/null | cut -d= -f1)

if [ -z "$COORDS" ]; then
    echo "Error: Pane '${NAME}' not found"
    exit 1
fi

# Verify the pane still exists
SESSION=$(echo "$COORDS" | cut -d: -f1)
WINDOW_PANE=$(echo "$COORDS" | cut -d: -f2)
WINDOW=$(echo "$WINDOW_PANE" | cut -d. -f1)
PANE=$(echo "$WINDOW_PANE" | cut -d. -f2)

if ! tmux list-panes -t "${SESSION}:${WINDOW}" -F "#{pane_index}" 2>/dev/null | grep -q "^${PANE}$"; then
    echo "Error: Pane '${NAME}' (${COORDS}) no longer exists"
    # Clean up the stale entry
    grep -v "^${COORDS}=" "$PANE_NAMES_FILE" > "$PANE_NAMES_FILE.tmp"
    mv "$PANE_NAMES_FILE.tmp" "$PANE_NAMES_FILE"
    exit 1
fi

echo "$COORDS"
