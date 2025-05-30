#!/bin/bash
# find-pane-by-source-and-role.sh - Find a pane by source and role properties
# Usage: ./find-pane-by-source-and-role.sh SOURCE ROLE
# Returns: Pane coordinates (e.g., "dev:0.1") or error

set -e

SOURCE="$1"
ROLE="$2"

if [ -z "$SOURCE" ] || [ -z "$ROLE" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 SOURCE ROLE"
    echo "Sources: user, claude-desktop, claude-code, system"
    echo "Roles: editor, terminal, logs, tests, debug, monitor"
    exit 1
fi

# Find all panes with matching source AND role
MATCHES=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}" | while read -r pane; do
    # Get both properties for this pane
    pane_source=$(tmux show-options -pqv -t "$pane" @source 2>/dev/null || echo "")
    pane_role=$(tmux show-options -pqv -t "$pane" @role 2>/dev/null || echo "")
    
    if [ "$pane_source" = "$SOURCE" ] && [ "$pane_role" = "$ROLE" ]; then
        echo "$pane"
    fi
done)

# Count matches
COUNT=$(echo "$MATCHES" | grep -v "^$" | wc -l)

if [ "$COUNT" -eq 0 ]; then
    echo "Error: No pane found with source='$SOURCE' and role='$ROLE'"
    exit 1
elif [ "$COUNT" -eq 1 ]; then
    echo "$MATCHES"
else
    # Multiple matches - return the first one and warn
    FIRST=$(echo "$MATCHES" | head -n1)
    echo "Warning: Multiple panes found with source='$SOURCE' and role='$ROLE', using $FIRST" >&2
    echo "$FIRST"
fi
