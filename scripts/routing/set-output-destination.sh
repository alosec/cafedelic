#!/bin/bash
# set-output-destination.sh - Configure where specific output types go
# Usage: ./set-output-destination.sh TYPE PANE_NAME

set -e

TYPE="$1"
PANE_NAME="$2"

if [ -z "$TYPE" ] || [ -z "$PANE_NAME" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 TYPE PANE_NAME"
    echo "Types: files, activity, logs, errors, terminal"
    exit 1
fi

# Verify the pane exists
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COORDS=$("$SCRIPT_DIR/../utils/find-pane.sh" "$PANE_NAME")

if [ $? -ne 0 ]; then
    echo "$COORDS"  # Error message from find-pane
    exit 1
fi

ROUTING_FILE="$HOME/.cafedelic/routing-config"
TEMP_FILE="$ROUTING_FILE.tmp"

# Remove any existing mapping for this type
if [ -f "$ROUTING_FILE" ]; then
    grep -v "^${TYPE}=" "$ROUTING_FILE" 2>/dev/null > "$TEMP_FILE" || true
    mv "$TEMP_FILE" "$ROUTING_FILE"
fi

# Add the new mapping
echo "${TYPE}=${PANE_NAME}" >> "$ROUTING_FILE"

echo "Successfully configured '${TYPE}' output to go to pane '${PANE_NAME}'"
