#!/bin/bash
# get-routing-config.sh - Show current routing configuration
# Usage: ./get-routing-config.sh

set -e

ROUTING_FILE="$HOME/.cafedelic/routing-config"
PANE_NAMES_FILE="$HOME/.cafedelic/pane-names"

echo "Routing Configuration:"
echo "====================="

if [ ! -f "$ROUTING_FILE" ]; then
    echo "No routing configured yet"
    exit 0
fi

# Read each routing rule
while IFS='=' read -r type pane_name; do
    if [ -z "$type" ] || [ -z "$pane_name" ]; then
        continue
    fi
    
    # Check if the target pane still exists
    if [ -f "$PANE_NAMES_FILE" ] && grep -q "=${pane_name}$" "$PANE_NAMES_FILE"; then
        COORDS=$(grep "=${pane_name}$" "$PANE_NAMES_FILE" | cut -d= -f1)
        echo "  ${type} -> ${pane_name} (${COORDS})"
    else
        echo "  ${type} -> ${pane_name} (WARNING: pane not found)"
    fi
done < "$ROUTING_FILE"

echo ""
echo "Available output types: files, activity, logs, errors, terminal"
