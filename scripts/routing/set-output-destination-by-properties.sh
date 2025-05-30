#!/bin/bash
# set-output-destination-by-properties.sh - Configure output routing using pane properties
# Usage: ./set-output-destination-by-properties.sh TYPE --source SOURCE --role ROLE

set -e

TYPE="$1"
shift

SOURCE=""
ROLE=""

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --source)
            SOURCE="$2"
            shift 2
            ;;
        --role)
            ROLE="$2"
            shift 2
            ;;
        *)
            echo "Error: Unknown argument: $1"
            echo "Usage: $0 TYPE --source SOURCE --role ROLE"
            echo "Types: files, activity, logs, errors, terminal"
            exit 1
            ;;
    esac
done

if [ -z "$TYPE" ] || [ -z "$SOURCE" ] || [ -z "$ROLE" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 TYPE --source SOURCE --role ROLE"
    exit 1
fi

# Find the pane by source and role
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PANE=$("$SCRIPT_DIR/../pane-properties/find-pane-by-source-and-role.sh" "$SOURCE" "$ROLE")

if [ $? -ne 0 ]; then
    echo "Error: $PANE"
    exit 1
fi

# Store routing with property info
ROUTING_FILE="$HOME/.cafedelic/routing-config-properties"
TEMP_FILE="$ROUTING_FILE.tmp"

# Remove any existing mapping for this type
if [ -f "$ROUTING_FILE" ]; then
    grep -v "^${TYPE}=" "$ROUTING_FILE" 2>/dev/null > "$TEMP_FILE" || true
    mv "$TEMP_FILE" "$ROUTING_FILE"
fi

# Add the new mapping with properties
echo "${TYPE}=source:${SOURCE},role:${ROLE}" >> "$ROUTING_FILE"

echo "Successfully configured '${TYPE}' output to go to pane with source='${SOURCE}' and role='${ROLE}'"
echo "Resolved to pane: $PANE"
