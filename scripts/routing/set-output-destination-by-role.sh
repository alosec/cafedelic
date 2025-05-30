#!/bin/bash
# set-output-destination-by-role.sh - Configure output routing using roles
# Usage: ./set-output-destination-by-role.sh TYPE ROLE [PREFERRED_SOURCE]

set -e

TYPE="$1"
ROLE="$2"
PREFERRED_SOURCE="${3:-}"

if [ -z "$TYPE" ] || [ -z "$ROLE" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 TYPE ROLE [PREFERRED_SOURCE]"
    echo "Types: files, activity, logs, errors, terminal"
    echo "Roles: editor, terminal, logs, tests, debug, monitor"
    exit 1
fi

# Verify a pane with this role exists
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -n "$PREFERRED_SOURCE" ]; then
    PANE=$("$SCRIPT_DIR/../pane-properties/find-best-pane-for-role.sh" "$ROLE" "$PREFERRED_SOURCE" 2>&1)
else
    PANE=$("$SCRIPT_DIR/../pane-properties/find-best-pane-for-role.sh" "$ROLE" 2>&1)
fi

if [ $? -ne 0 ]; then
    echo "Error: No pane found with role='$ROLE'"
    exit 1
fi

# Store routing with role info
ROUTING_FILE="$HOME/.cafedelic/routing-config"
TEMP_FILE="$ROUTING_FILE.tmp"

# Remove any existing mapping for this type
if [ -f "$ROUTING_FILE" ]; then
    grep -v "^${TYPE}=" "$ROUTING_FILE" 2>/dev/null > "$TEMP_FILE" || true
    mv "$TEMP_FILE" "$ROUTING_FILE"
fi

# Add the new mapping - using role as the value
# This maintains compatibility while documenting intent
echo "${TYPE}=${ROLE}" >> "$ROUTING_FILE"

# Also store enhanced config for property-aware systems
ENHANCED_FILE="$HOME/.cafedelic/routing-config-enhanced"
TEMP_FILE="$ENHANCED_FILE.tmp"

if [ -f "$ENHANCED_FILE" ]; then
    grep -v "^${TYPE}=" "$ENHANCED_FILE" 2>/dev/null > "$TEMP_FILE" || true
    mv "$TEMP_FILE" "$ENHANCED_FILE"
fi

# Store with full property info
if [ -n "$PREFERRED_SOURCE" ]; then
    echo "${TYPE}=role:${ROLE},source:${PREFERRED_SOURCE}" >> "$ENHANCED_FILE"
else
    echo "${TYPE}=role:${ROLE}" >> "$ENHANCED_FILE"
fi

echo "Successfully configured '${TYPE}' output to go to panes with role='${ROLE}'"
if [ -n "$PREFERRED_SOURCE" ]; then
    echo "Preferring source='${PREFERRED_SOURCE}' when available"
fi
echo "Resolved to pane: $PANE"
