#!/bin/bash
# open-file-in-pane.sh - Open file in pane-specific emacs server
# Usage: open-file-in-pane.sh <file-path> [tmux-pane]

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Arguments
FILE_PATH="$1"
PANE="${2:-9:0.2}"  # Default to Sophie's pane

# Set XDG_RUNTIME_DIR if not set
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

if [ -z "$FILE_PATH" ]; then
    echo -e "${RED}Error: No file path provided${NC}"
    echo "Usage: $0 <file-path> [tmux-pane]"
    exit 1
fi

# Convert to absolute path if relative
if [[ ! "$FILE_PATH" = /* ]]; then
    FILE_PATH="$(pwd)/$FILE_PATH"
fi

# Check if file exists
if [ ! -f "$FILE_PATH" ]; then
    echo -e "${RED}Error: File not found: $FILE_PATH${NC}"
    exit 1
fi

# Get server name for pane
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVER_NAME=$("$SCRIPT_DIR/get-pane-server.sh" "$PANE")

if [ -z "$SERVER_NAME" ] || [ "$SERVER_NAME" = "ERROR" ]; then
    echo -e "${YELLOW}No server found for pane $PANE${NC}"
    echo -e "${YELLOW}Starting new server...${NC}"
    
    # Start server and wait
    "$SCRIPT_DIR/start-pane-emacs.sh" "$PANE"
    sleep 3
    
    # Try again
    SERVER_NAME=$("$SCRIPT_DIR/get-pane-server.sh" "$PANE")
    if [ -z "$SERVER_NAME" ] || [ "$SERVER_NAME" = "ERROR" ]; then
        echo -e "${RED}Failed to start server${NC}"
        exit 1
    fi
fi

echo -e "${BLUE}Opening file in pane $PANE (server: $SERVER_NAME)${NC}"
echo -e "File: ${GREEN}$FILE_PATH${NC}"

# Open file using emacsclient
RESULT=$(emacsclient --socket-name="$SERVER_NAME" \
         --eval "(cafedelic-pane-open-file \"$FILE_PATH\")" 2>&1)

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ File opened successfully${NC}"
    
    # Get file count
    COUNT=$(emacsclient --socket-name="$SERVER_NAME" \
            --eval "(length cafedelic-recent-files)" 2>/dev/null)
    echo "Files in context: $COUNT"
    
    # Focus the pane
    tmux select-pane -t "$PANE"
else
    echo -e "${RED}✗ Failed to open file${NC}"
    echo "Error: $RESULT"
    exit 1
fi
