#!/bin/bash
# init-claude-frame.sh - Initialize full frame Cafedelic UI

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ELISP_FILE="$SCRIPT_DIR/cafedelic-frame.el"

echo -e "${BLUE}═══ Cafedelic Full Frame UI ═══${NC}\n"

# Check emacs daemon
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    echo "Start it with: emacs --daemon"
    exit 1
fi

echo -e "${YELLOW}Loading Cafedelic elisp functions...${NC}"

# Load our elisp file
LOAD_RESULT=$(emacsclient --eval "(load \"$ELISP_FILE\")" 2>&1)
if [ $? -ne 0 ]; then
    echo -e "${RED}Failed to load elisp file${NC}"
    echo "Error: $LOAD_RESULT"
    exit 1
fi

echo -e "${YELLOW}Initializing frame layout...${NC}"

# Initialize the frame
RESULT=$(emacsclient --eval "(cafedelic-init-frame)" 2>&1)

if [ $? -eq 0 ]; then
    echo -e "${GREEN}Success!${NC}"
    echo "Frame layout ready:"
    echo "  • Left sidebar: File tree (30 chars wide)"
    echo "  • Right area: Current file content"
    echo ""
    echo "Usage:"
    echo "  ./open-claude-file.sh <file>  - Open a file"
    echo "  ./show-claude-status.sh       - Show current state"
else
    echo -e "${RED}Failed to initialize frame${NC}"
    echo "Error: $RESULT"
    exit 1
fi
