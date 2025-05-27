#!/bin/bash
# start-pane-emacs.sh - Start emacs with server in specific tmux pane
# Usage: start-pane-emacs.sh <tmux-pane> [server-suffix]

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Arguments
PANE="${1:-9:0.2}"  # Default to Sophie's pane
SUFFIX="${2:-}"     # Optional custom suffix

# Validate pane exists
if ! tmux list-panes -t "$PANE" &>/dev/null; then
    echo -e "${RED}Error: Pane $PANE does not exist${NC}"
    exit 1
fi

# Generate server name from pane ID
# Convert "9:0.2" to "pane-9-0-2" or "pane-9-0-2-custom"
SERVER_NAME="pane-$(echo $PANE | tr ':.' '-')"
if [ -n "$SUFFIX" ]; then
    SERVER_NAME="${SERVER_NAME}-${SUFFIX}"
fi

echo -e "${YELLOW}Starting emacs server in pane $PANE${NC}"
echo -e "Server name: ${BLUE}$SERVER_NAME${NC}"

# Check if emacs is already running in the pane
PANE_CONTENT=$(tmux capture-pane -t "$PANE" -p | head -5)
if echo "$PANE_CONTENT" | grep -q "GNU Emacs"; then
    echo -e "${YELLOW}Killing existing emacs in pane...${NC}"
    tmux send-keys -t "$PANE" C-x C-c
    sleep 1
fi

# Clear the pane
tmux send-keys -t "$PANE" C-c
tmux send-keys -t "$PANE" C-l

# Prepare init file path
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INIT_FILE="$SCRIPT_DIR/pane-emacs-init.el"

# Create pane state directory if it doesn't exist
STATE_DIR="$HOME/.cafedelic/pane-servers"
mkdir -p "$STATE_DIR"

# Start emacs with server in the pane
echo -e "${GREEN}Starting emacs with server '$SERVER_NAME'...${NC}"

# First, start emacs normally
tmux send-keys -t "$PANE" "emacs" Enter

# Wait for emacs to start
sleep 2

# Set up the server with the correct name
tmux send-keys -t "$PANE" Escape ':' "(progn (setq server-name \"$SERVER_NAME\") (server-force-delete) (server-start) (load \"$INIT_FILE\"))" Enter

# Store mapping (overwrite if exists)
MAPPING_FILE="$STATE_DIR/mappings.txt"
# Remove old mapping for this pane
grep -v "^$PANE:" "$MAPPING_FILE" 2>/dev/null > "$MAPPING_FILE.tmp" || true
echo "$PANE:$SERVER_NAME:$(date +%s)" >> "$MAPPING_FILE.tmp"
mv "$MAPPING_FILE.tmp" "$MAPPING_FILE"

# Wait a moment for emacs to start
sleep 3

# Set XDG_RUNTIME_DIR if not set
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

# Verify server is running
if emacsclient --socket-name="$SERVER_NAME" --eval "(emacs-version)" &>/dev/null; then
    echo -e "${GREEN}✓ Server '$SERVER_NAME' is running${NC}"
    echo -e "${GREEN}✓ Pane $PANE is ready for file operations${NC}"
else
    echo -e "${RED}✗ Failed to verify server startup${NC}"
    echo -e "${YELLOW}Server may still be starting, please wait...${NC}"
fi

# Store server info for other scripts
echo "$SERVER_NAME" > "$STATE_DIR/pane-$PANE.server"
