#!/bin/bash
# get-pane-server.sh - Get the emacs server name for a tmux pane
# Usage: get-pane-server.sh <tmux-pane>

PANE="${1:-9:0.2}"
STATE_DIR="$HOME/.cafedelic/pane-servers"

# Set XDG_RUNTIME_DIR if not set
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

# Check state file first
STATE_FILE="$STATE_DIR/pane-$PANE.server"
if [ -f "$STATE_FILE" ]; then
    SERVER_NAME=$(cat "$STATE_FILE")
    
    # Verify server is still running
    if emacsclient --socket-name="$SERVER_NAME" --eval "t" &>/dev/null; then
        echo "$SERVER_NAME"
        exit 0
    fi
fi

# Check mappings file as fallback
MAPPING_FILE="$STATE_DIR/mappings.txt"
if [ -f "$MAPPING_FILE" ]; then
    # Get most recent mapping for this pane
    SERVER_NAME=$(grep "^$PANE:" "$MAPPING_FILE" | tail -1 | cut -d: -f2)
    
    if [ -n "$SERVER_NAME" ]; then
        # Verify server is running
        if emacsclient --socket-name="$SERVER_NAME" --eval "t" &>/dev/null; then
            echo "$SERVER_NAME"
            exit 0
        fi
    fi
fi

# No server found
echo "ERROR"
exit 1
