#!/bin/bash
# assign-properties.sh - Assign properties to a tmux pane (name, source, role)
# Usage: ./assign-properties.sh SESSION WINDOW PANE [--name NAME] [--source SOURCE] [--role ROLE]
# Supports backward compatibility with name-only assignment

set -e

# Parse arguments
SESSION=""
WINDOW=""
PANE=""
NAME=""
SOURCE=""
ROLE=""

# First three args are positional
if [ $# -lt 3 ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 SESSION WINDOW PANE [--name NAME] [--source SOURCE] [--role ROLE]"
    exit 1
fi

SESSION="$1"
WINDOW="$2"
PANE="$3"
shift 3

# Parse optional named arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --name)
            NAME="$2"
            shift 2
            ;;
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
            echo "Usage: $0 SESSION WINDOW PANE [--name NAME] [--source SOURCE] [--role ROLE]"
            exit 1
            ;;
    esac
done

# Validate at least one property is being set
if [ -z "$NAME" ] && [ -z "$SOURCE" ] && [ -z "$ROLE" ]; then
    echo "Error: At least one property must be specified"
    exit 1
fi

# Check if pane exists
if ! tmux list-panes -t "${SESSION}:${WINDOW}" -F "#{pane_index}" 2>/dev/null | grep -q "^${PANE}$"; then
    echo "Error: Pane ${SESSION}:${WINDOW}.${PANE} does not exist"
    exit 1
fi

# Set properties via tmux user options
if [ -n "$NAME" ]; then
    # Validate name (no spaces, colons, or dots)
    if [[ "$NAME" =~ [[:space:]:\.]+ ]]; then
        echo "Error: Name cannot contain spaces, colons, or dots"
        exit 1
    fi
    
    # Update file-based mapping for backward compatibility
    PANE_NAMES_FILE="$HOME/.cafedelic/pane-names"
    TEMP_FILE="$PANE_NAMES_FILE.tmp"
    
    # Remove any existing mapping for this pane or name
    if [ -f "$PANE_NAMES_FILE" ]; then
        grep -v "^${SESSION}:${WINDOW}\.${PANE}=" "$PANE_NAMES_FILE" 2>/dev/null | \
        grep -v "=${NAME}$" > "$TEMP_FILE" || true
        mv "$TEMP_FILE" "$PANE_NAMES_FILE"
    fi
    
    # Add the new mapping to file
    echo "${SESSION}:${WINDOW}.${PANE}=${NAME}" >> "$PANE_NAMES_FILE"
    
    # Set tmux property
    tmux set-option -p -t "${SESSION}:${WINDOW}.${PANE}" @pane_name "${NAME}"
    echo "Set name='${NAME}'"
fi

if [ -n "$SOURCE" ]; then
    # Validate source
    case "$SOURCE" in
        user|claude-desktop|claude-code|system)
            tmux set-option -p -t "${SESSION}:${WINDOW}.${PANE}" @source "${SOURCE}"
            echo "Set source='${SOURCE}'"
            ;;
        *)
            echo "Error: Invalid source. Must be: user, claude-desktop, claude-code, or system"
            exit 1
            ;;
    esac
fi

if [ -n "$ROLE" ]; then
    # Validate role
    case "$ROLE" in
        editor|terminal|logs|tests|debug|monitor)
            tmux set-option -p -t "${SESSION}:${WINDOW}.${PANE}" @role "${ROLE}"
            echo "Set role='${ROLE}'"
            ;;
        *)
            echo "Error: Invalid role. Must be: editor, terminal, logs, tests, debug, or monitor"
            exit 1
            ;;
    esac
fi

echo "Successfully assigned properties to pane ${SESSION}:${WINDOW}.${PANE}"
