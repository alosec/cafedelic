#!/bin/bash
# find-best-pane-for-role.sh - Find the best pane for a role with fallback logic
# Usage: ./find-best-pane-for-role.sh ROLE [PREFERRED_SOURCE]
# Returns: Pane coordinates with fallback strategy

set -e

ROLE="$1"
PREFERRED_SOURCE="${2:-}"

if [ -z "$ROLE" ]; then
    echo "Error: Missing required role argument"
    echo "Usage: $0 ROLE [PREFERRED_SOURCE]"
    echo "Roles: editor, terminal, logs, tests, debug, monitor"
    exit 1
fi

# First try: exact match with preferred source
if [ -n "$PREFERRED_SOURCE" ]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    EXACT_MATCH=$("$SCRIPT_DIR/find-pane-by-source-and-role.sh" "$PREFERRED_SOURCE" "$ROLE" 2>/dev/null || true)
    
    if [ -n "$EXACT_MATCH" ]; then
        echo "$EXACT_MATCH"
        exit 0
    fi
fi

# Second try: any pane with the specified role
ROLE_MATCHES=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}" | while read -r pane; do
    pane_role=$(tmux show-options -pqv -t "$pane" @role 2>/dev/null || echo "")
    
    if [ "$pane_role" = "$ROLE" ]; then
        # Include source info for prioritization
        pane_source=$(tmux show-options -pqv -t "$pane" @source 2>/dev/null || echo "unknown")
        echo "${pane}|${pane_source}"
    fi
done)

if [ -n "$ROLE_MATCHES" ]; then
    # If we have a preferred source, try to find a match with that source
    if [ -n "$PREFERRED_SOURCE" ]; then
        PREFERRED_MATCH=$(echo "$ROLE_MATCHES" | grep "|${PREFERRED_SOURCE}$" | head -n1 | cut -d'|' -f1)
        if [ -n "$PREFERRED_MATCH" ]; then
            echo "$PREFERRED_MATCH"
            exit 0
        fi
    fi
    
    # Otherwise, return the first match
    FIRST_MATCH=$(echo "$ROLE_MATCHES" | head -n1 | cut -d'|' -f1)
    echo "$FIRST_MATCH"
    exit 0
fi

# Third try: fallback to name-based lookup
# Check if there's a pane with @name matching the role
NAME_MATCH=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}" | while read -r pane; do
    pane_name=$(tmux show-options -pqv -t "$pane" @pane_name 2>/dev/null || echo "")
    
    if [ "$pane_name" = "$ROLE" ]; then
        echo "$pane"
        break
    fi
done | head -n1)

if [ -n "$NAME_MATCH" ]; then
    echo "Warning: No pane with role='$ROLE' found, falling back to name='$ROLE' at $NAME_MATCH" >&2
    echo "$NAME_MATCH"
    exit 0
fi

# No match found
echo "Error: No pane found with role='$ROLE' (or name='$ROLE' as fallback)"
exit 1
