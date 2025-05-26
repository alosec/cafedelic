#!/bin/bash
# open-right.sh - Open a file in Claude's buffer (right window)
# Maintains two-window layout and uses claude- prefix for buffer names

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
FILE_NAME=$(basename "$FILE_PATH")
BUFFER_NAME="claude-$FILE_NAME"

echo -e "${YELLOW}Opening file in Claude's buffer...${NC}"
echo -e "File: ${BLUE}$FILE_PATH${NC}"
echo -e "Buffer name: ${BLUE}$BUFFER_NAME${NC}"

# Check emacs daemon
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    exit 1
fi

# Open file in right window with claude- prefix
RESULT=$(emacsclient --eval "
(progn
  ;; Ensure we have two windows
  (when (= (count-windows) 1)
    (split-window-horizontally))
  
  ;; Save current window
  (let ((original-window (selected-window)))
    
    ;; Move to right window (or create it)
    (select-window (or (window-in-direction 'right)
                       (split-window-horizontally)))
    
    ;; Check if buffer already exists
    (let ((existing-buffer (get-buffer \"$BUFFER_NAME\")))
      (if existing-buffer
          (progn
            (switch-to-buffer existing-buffer)
            (format \"Switched to existing buffer: %s\" \"$BUFFER_NAME\"))
        (progn
          ;; Open the file
          (find-file \"$FILE_PATH\")
          ;; Rename buffer with claude- prefix
          (rename-buffer \"$BUFFER_NAME\" t)
          (format \"Opened new buffer: %s\" \"$BUFFER_NAME\"))))
    
    ;; Return focus to original window
    (select-window original-window)
    
    ;; Return final status
    (format \"File ready in right window: %s\" \"$BUFFER_NAME\")))
" 2>&1)
if [ $? -eq 0 ]; then
    echo -e "${GREEN}Success!${NC}"
    echo "$RESULT"
    
    # Show current window layout info
    LAYOUT_INFO=$(emacsclient --eval "
      (format \"Windows: %d, Current focus: %s\"
              (count-windows)
              (buffer-name))" 2>/dev/null)
    echo -e "\nLayout info: $LAYOUT_INFO"
else
    echo -e "${RED}Failed to open file${NC}"
    echo "Error: $RESULT"
    exit 1
fi
