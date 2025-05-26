#!/bin/bash
# show-claude-status.sh - Display current Cafedelic state

GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${CYAN}═══ Cafedelic Status ═══${NC}\n"

# Check emacs daemon
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    exit 1
fi

# Load elisp if needed
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
emacsclient --eval "(load \"$SCRIPT_DIR/cafedelic-frame.el\")" >/dev/null 2>&1

# Get recent files count
COUNT=$(emacsclient --eval "(length cafedelic-recent-files)" 2>/dev/null)
echo -e "${YELLOW}Files in context:${NC} $COUNT"

# Get current buffer
CURRENT=$(emacsclient --eval "(buffer-name)" 2>/dev/null | tr -d '"')
echo -e "${YELLOW}Current buffer:${NC} $CURRENT"

# List recent files
echo -e "\n${YELLOW}Recent files:${NC}"
FILES=$(emacsclient --eval "
(mapconcat 
  (lambda (entry) 
    (format \"  %s\" (file-name-nondirectory (car entry))))
  cafedelic-recent-files
  \"\\n\")" 2>/dev/null | tr -d '"' | sed 's/\\n/\n/g')

if [ -z "$FILES" ] || [ "$FILES" = "" ]; then
    echo "  (none)"
else
    echo "$FILES"
fi

echo -e "\n${GREEN}Use ./open-claude-file.sh <file> to add more files${NC}"
