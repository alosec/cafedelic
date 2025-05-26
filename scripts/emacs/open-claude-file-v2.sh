#!/bin/bash
# open-claude-file.sh - Open a file in Cafedelic frame

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

if [ $# -eq 0 ]; then
    echo -e "${RED}Error: No file path provided${NC}"
    echo "Usage: $0 <file-path>"
    exit 1
fi

FILE_PATH="$1"
# Convert to absolute path if relative
if [[ ! "$FILE_PATH" = /* ]]; then
    FILE_PATH="$(pwd)/$FILE_PATH"
fi

echo -e "${YELLOW}Opening file in Cafedelic...${NC}"
echo -e "File: ${BLUE}$FILE_PATH${NC}"

# Use socket name from environment if available
SOCKET_ARG=""
if [ -n "$CAFEDELIC_SOCKET_NAME" ]; then
    SOCKET_ARG="--socket-name=$CAFEDELIC_SOCKET_NAME"
fi

# Check emacs daemon
if ! emacsclient $SOCKET_ARG --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    exit 1
fi

# Ensure elisp is loaded
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
emacsclient $SOCKET_ARG --eval "(load \"$SCRIPT_DIR/cafedelic-editor.el\")" >/dev/null 2>&1

# Open the file
RESULT=$(emacsclient $SOCKET_ARG --eval "(cafedelic-open-file \"$FILE_PATH\")" 2>&1)
if [ $? -eq 0 ]; then
    echo -e "${GREEN}File opened successfully!${NC}"
    # Show current file count
    COUNT=$(emacsclient $SOCKET_ARG --eval "(length cafedelic-recent-files)" 2>/dev/null)
    echo "Total files in context: $COUNT"
else
    echo -e "${RED}Failed to open file${NC}"
    echo "Error: $RESULT"
    exit 1
fi
