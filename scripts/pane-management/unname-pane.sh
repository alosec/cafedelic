#!/bin/bash
# unname-pane.sh - Remove a custom name from a pane
# Usage: ./unname-pane.sh NAME

set -e

NAME="$1"

if [ -z "$NAME" ]; then
    echo "Error: Missing pane name"
    echo "Usage: $0 NAME"
    exit 1
fi

PANE_NAMES_FILE="$HOME/.cafedelic/pane-names"
TEMP_FILE="$PANE_NAMES_FILE.tmp"

if [ ! -f "$PANE_NAMES_FILE" ]; then
    echo "Error: No panes have been named yet"
    exit 1
fi

# Check if the name exists
if ! grep -q "=${NAME}$" "$PANE_NAMES_FILE"; then
    echo "Error: Pane '${NAME}' not found"
    exit 1
fi

# Get the coordinates before removing
COORDS=$(grep "=${NAME}$" "$PANE_NAMES_FILE" | cut -d= -f1)

# Remove the entry
grep -v "=${NAME}$" "$PANE_NAMES_FILE" > "$TEMP_FILE"
mv "$TEMP_FILE" "$PANE_NAMES_FILE"

echo "Successfully removed name '${NAME}' from pane ${COORDS}"
