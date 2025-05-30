#!/bin/bash
# list-named-panes.sh - List all panes that have been assigned names
# Usage: ./list-named-panes.sh

set -e

PANE_NAMES_FILE="$HOME/.cafedelic/pane-names"

if [ ! -f "$PANE_NAMES_FILE" ]; then
    echo "No panes have been named yet"
    exit 0
fi

echo "Named Panes:"
echo "============"

# Read each line and verify the pane still exists
while IFS='=' read -r coords name; do
    if [ -z "$coords" ] || [ -z "$name" ]; then
        continue
    fi
    
    # Parse coordinates
    SESSION=$(echo "$coords" | cut -d: -f1)
    WINDOW_PANE=$(echo "$coords" | cut -d: -f2)
    WINDOW=$(echo "$WINDOW_PANE" | cut -d. -f1)
    PANE=$(echo "$WINDOW_PANE" | cut -d. -f2)
    
    # Check if pane exists
    if tmux list-panes -t "${SESSION}:${WINDOW}" -F "#{pane_index}" 2>/dev/null | grep -q "^${PANE}$"; then
        # Get additional info about the pane
        PANE_INFO=$(tmux list-panes -t "${SESSION}:${WINDOW}" -F "#{pane_index} #{pane_current_command} #{pane_width}x#{pane_height}" | grep "^${PANE} ")
        COMMAND=$(echo "$PANE_INFO" | cut -d' ' -f2)
        DIMENSIONS=$(echo "$PANE_INFO" | cut -d' ' -f3)
        
        echo "  ${name} -> ${coords} (${COMMAND}, ${DIMENSIONS})"
    else
        echo "  ${name} -> ${coords} (STALE - pane no longer exists)"
    fi
done < "$PANE_NAMES_FILE"
