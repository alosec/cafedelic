#!/bin/bash
# list-panes-by-properties.sh - List panes filtered by properties
# Usage: ./list-panes-by-properties.sh [--source SOURCE] [--role ROLE] [--name NAME]
# Shows all panes matching the specified criteria

set -e

# Parse arguments
FILTER_SOURCE=""
FILTER_ROLE=""
FILTER_NAME=""

while [ $# -gt 0 ]; do
    case "$1" in
        --source)
            FILTER_SOURCE="$2"
            shift 2
            ;;
        --role)
            FILTER_ROLE="$2"
            shift 2
            ;;
        --name)
            FILTER_NAME="$2"
            shift 2
            ;;
        *)
            echo "Error: Unknown argument: $1"
            echo "Usage: $0 [--source SOURCE] [--role ROLE] [--name NAME]"
            exit 1
            ;;
    esac
done

echo "Panes with properties:"
echo "====================="

# List all panes and check properties
tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}|#{pane_current_command}|#{pane_active}" | while IFS='|' read -r pane cmd active; do
    # Get all properties for this pane
    pane_name=$(tmux show-options -pqv -t "$pane" @pane_name 2>/dev/null || echo "")
    pane_source=$(tmux show-options -pqv -t "$pane" @source 2>/dev/null || echo "")
    pane_role=$(tmux show-options -pqv -t "$pane" @role 2>/dev/null || echo "")
    
    # Apply filters
    if [ -n "$FILTER_NAME" ] && [ "$pane_name" != "$FILTER_NAME" ]; then
        continue
    fi
    if [ -n "$FILTER_SOURCE" ] && [ "$pane_source" != "$FILTER_SOURCE" ]; then
        continue
    fi
    if [ -n "$FILTER_ROLE" ] && [ "$pane_role" != "$FILTER_ROLE" ]; then
        continue
    fi
    
    # Skip panes with no properties if filters are specified
    if [ -n "$FILTER_NAME$FILTER_SOURCE$FILTER_ROLE" ] && [ -z "$pane_name$pane_source$pane_role" ]; then
        continue
    fi
    
    # Show only panes with at least one property
    if [ -n "$pane_name" ] || [ -n "$pane_source" ] || [ -n "$pane_role" ]; then
        echo ""
        echo "Pane: $pane"
        [ -n "$pane_name" ] && echo "  name: $pane_name"
        [ -n "$pane_source" ] && echo "  source: $pane_source"
        [ -n "$pane_role" ] && echo "  role: $pane_role"
        echo "  command: $cmd"
        [ "$active" = "1" ] && echo "  status: ACTIVE"
    fi
done

echo ""
