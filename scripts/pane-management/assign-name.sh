#!/bin/bash
# assign-name.sh - Assign a custom name to a tmux pane
# Usage: ./assign-name.sh SESSION WINDOW PANE NAME

set -e

SESSION="$1"
WINDOW="$2"
PANE="$3"
NAME="$4"

# Validate inputs
if [ -z "$SESSION" ] || [ -z "$WINDOW" ] || [ -z "$PANE" ] || [ -z "$NAME" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 SESSION WINDOW PANE NAME"
    exit 1
fi

# Validate name (no spaces, colons, or dots)
if [[ "$NAME" =~ [[:space:]:\.]+ ]]; then
    echo "Error: Name cannot contain spaces, colons, or dots"
    exit 1
fi

# Check if pane exists
if ! tmux list-panes -t "${SESSION}:${WINDOW}" -F "#{pane_index}" 2>/dev/null | grep -q "^${PANE}$"; then
    echo "Error: Pane ${SESSION}:${WINDOW}.${PANE} does not exist"
    exit 1
fi

# Store the name mapping
PANE_NAMES_FILE="$HOME/.cafedelic/pane-names"
TEMP_FILE="$PANE_NAMES_FILE.tmp"

# Remove any existing mapping for this pane or name
if [ -f "$PANE_NAMES_FILE" ]; then
    grep -v "^${SESSION}:${WINDOW}\.${PANE}=" "$PANE_NAMES_FILE" 2>/dev/null | \
    grep -v "=${NAME}$" > "$TEMP_FILE" || true
    mv "$TEMP_FILE" "$PANE_NAMES_FILE"
fi

# Add the new mapping
echo "${SESSION}:${WINDOW}.${PANE}=${NAME}" >> "$PANE_NAMES_FILE"

echo "Successfully assigned name '${NAME}' to pane ${SESSION}:${WINDOW}.${PANE}"
