#!/bin/bash
# open-directory-in-pane.sh - Open directory in pane-specific emacs dired
# Usage: open-directory-in-pane.sh <directory-path> [tmux-pane]

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Arguments
DIR_PATH="$1"
PANE="${2:-9:0.2}"  # Default to Sophie's pane

# Set XDG_RUNTIME_DIR if not set
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

if [ -z "$DIR_PATH" ]; then
    echo -e "${RED}Error: No directory path provided${NC}"
    echo "Usage: $0 <directory-path> [tmux-pane]"
    exit 1
fi

# Convert to absolute path if relative
if [[ ! "$DIR_PATH" = /* ]]; then
    DIR_PATH="$(pwd)/$DIR_PATH"
fi

# Check if directory exists
if [ ! -d "$DIR_PATH" ]; then
    echo -e "${RED}Error: Directory not found: $DIR_PATH${NC}"
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

echo -e "${BLUE}Opening directory in pane $PANE (server: $SERVER_NAME)${NC}"
echo -e "Directory: ${GREEN}$DIR_PATH${NC}"

# Open directory using emacsclient
RESULT=$(emacsclient --socket-name="$SERVER_NAME" \
         --eval "(cafedelic-pane-open-directory \"$DIR_PATH\")" 2>&1)

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Directory opened in dired${NC}"
    
    # Focus the pane
    tmux select-pane -t "$PANE"
else
    echo -e "${RED}✗ Failed to open directory${NC}"
    echo "Error: $RESULT"
    exit 1
fi
