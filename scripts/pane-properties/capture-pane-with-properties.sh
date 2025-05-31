#!/bin/bash
# Capture pane content with property-based filtering and advanced options
# Combines property-based pane discovery with tmux capture-pane functionality

set -euo pipefail

# Parse arguments
SOURCE=""
ROLE=""
NAME=""
START=""
END=""
LAST=""
JOIN_LINES=false
ESCAPE_SEQUENCES=false
PRESERVE_TRAILING=false
GREP=""
GREP_CONTEXT=0
INVERT_MATCH=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --source) SOURCE="$2"; shift 2 ;;
        --role) ROLE="$2"; shift 2 ;;
        --name) NAME="$2"; shift 2 ;;
        --start) START="$2"; shift 2 ;;
        --end) END="$2"; shift 2 ;;
        --last) LAST="$2"; shift 2 ;;
        --join-lines) JOIN_LINES=true; shift ;;
        --escape-sequences) ESCAPE_SEQUENCES=true; shift ;;
        --preserve-trailing) PRESERVE_TRAILING=true; shift ;;
        --grep) GREP="$2"; shift 2 ;;
        --grep-context) GREP_CONTEXT="$2"; shift 2 ;;
        --invert-match) INVERT_MATCH=true; shift ;;
        *) echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Find the best pane based on properties
FIND_ARGS=()
if [[ -n "$SOURCE" ]]; then
    FIND_ARGS+=("$SOURCE")
fi
if [[ -n "$ROLE" ]]; then
    FIND_ARGS+=("$ROLE")
fi

# If name is provided, use direct pane lookup instead
if [[ -n "$NAME" ]]; then
    PANE_INFO=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index} #{pane_id}" | \
        while read -r pane_spec pane_id; do
            if tmux show-environment -t "$pane_id" "CAFEDELIC_NAME" 2>/dev/null | grep -q "^CAFEDELIC_NAME=$NAME$"; then
                echo "$pane_spec"
                break
            fi
        done)
else
    # Use property-based discovery
    PANE_INFO=$("$SCRIPT_DIR/find-best-pane-for-role.sh" "${FIND_ARGS[@]}")
fi

if [[ -z "$PANE_INFO" ]]; then
    echo "Error: No pane found matching the specified criteria" >&2
    exit 1
fi

# Build tmux capture-pane command
CAPTURE_CMD="tmux capture-pane -t $PANE_INFO -p"

# Add capture range options
if [[ -n "$LAST" ]]; then
    # Last N lines
    CAPTURE_CMD+=" -S -$LAST"
elif [[ -n "$START" || -n "$END" ]]; then
    # Specific range
    if [[ -n "$START" ]]; then
        CAPTURE_CMD+=" -S $START"
    fi
    if [[ -n "$END" ]]; then
        CAPTURE_CMD+=" -E $END"
    fi
fi

# Add formatting options
if [[ "$JOIN_LINES" == "true" ]]; then
    CAPTURE_CMD+=" -J"
fi
if [[ "$ESCAPE_SEQUENCES" == "true" ]]; then
    CAPTURE_CMD+=" -e"
fi
if [[ "$PRESERVE_TRAILING" == "true" ]]; then
    CAPTURE_CMD+=" -N"
fi

# Execute capture and apply grep if needed
if [[ -z "$GREP" ]]; then
    # No grep, just output
    eval "$CAPTURE_CMD"
else
    # Apply grep with options
    GREP_CMD="grep"
    
    if [[ "$GREP_CONTEXT" -gt 0 ]]; then
        GREP_CMD+=" -C $GREP_CONTEXT"
    fi
    
    if [[ "$INVERT_MATCH" == "true" ]]; then
        GREP_CMD+=" -v"
    fi
    
    # Add color when escape sequences are enabled
    if [[ "$ESCAPE_SEQUENCES" == "true" ]]; then
        GREP_CMD+=" --color=always"
    fi
    
    # Execute capture and pipe to grep
    eval "$CAPTURE_CMD" | $GREP_CMD "$GREP" || true
fi