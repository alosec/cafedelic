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

# Check emacs daemon
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    exit 1
fi

# Ensure elisp is loaded
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
emacsclient --eval "(load \"$SCRIPT_DIR/cafedelic-frame.el\")" >/dev/null 2>&1

# Add file to recent list and open it
RESULT=$(emacsclient --eval "(cafedelic-add-file \"$FILE_PATH\")" 2>&1)

if [ $? -eq 0 ]; then
    echo -e "${GREEN}File opened successfully!${NC}"
    
    # Show current file count
    COUNT=$(emacsclient --eval "(length cafedelic-recent-files)" 2>/dev/null)
    echo "Total files in context: $COUNT"
else
    echo -e "${RED}Failed to open file${NC}"
    echo "Error: $RESULT"
    exit 1
fi
