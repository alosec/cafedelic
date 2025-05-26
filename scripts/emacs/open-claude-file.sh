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
emacsclient --eval "(load \"$SCRIPT_DIR/cafedelic-editor.el\")" >/dev/null 2>&1

# Open the file
TARGET_PANE="9:0.2"

# Check if target pane exists for output routing
if tmux list-panes -t "$TARGET_PANE" >/dev/null 2>&1; then
    echo -e "${YELLOW}Routing output to pane $TARGET_PANE${NC}"
    # Run emacsclient and display result in target pane
    RESULT=$(emacsclient --eval "(cafedelic-open-file \"$FILE_PATH\")" 2>&1)
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}File opened successfully!${NC}"
        # Show current file count
        COUNT=$(emacsclient --eval "(length cafedelic-recent-files)" 2>/dev/null)
        echo "Total files in context: $COUNT"
        # Display success in target pane
        tmux run-shell -t "$TARGET_PANE" "echo 'Opened file: $(basename \"$FILE_PATH\") ($COUNT files in context)'"
    else
        echo -e "${RED}Failed to open file${NC}"
        echo "Error: $RESULT"
        # Display error in target pane
        tmux run-shell -t "$TARGET_PANE" "echo 'Failed to open: $(basename \"$FILE_PATH\") - $RESULT'"
        exit 1
    fi
else
    # Fall back to direct output if pane doesn't exist
    RESULT=$(emacsclient --eval "(cafedelic-open-file \"$FILE_PATH\")" 2>&1)
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
fi
