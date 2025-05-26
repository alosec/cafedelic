#!/bin/bash
# init-claude-view.sh - Initialize the two-window layout for Cafedelic
# Left window: User's buffer (preserved)
# Right window: Claude's buffer (managed)

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${YELLOW}Initializing Cafedelic two-window view...${NC}"

# Check emacs daemon
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    exit 1
fi

# Save current buffer name to restore later
CURRENT_BUFFER=$(emacsclient --eval "(buffer-name)" 2>/dev/null | tr -d '"')
echo "Current buffer: $CURRENT_BUFFER"

# Set up the two-window layout
RESULT=$(emacsclient --eval "
(progn
  ;; Save current point and buffer
  (let ((current-buf (current-buffer))
        (current-point (point)))
    
    ;; Delete other windows and split horizontally
    (delete-other-windows)
    (split-window-horizontally)
    
    ;; Return to original buffer in left window
    (switch-to-buffer current-buf)
    (goto-char current-point)
    
    ;; Move to right window and create/show claude workspace
    (other-window 1)
    (switch-to-buffer (get-buffer-create \"*claude-workspace*\"))
    (insert \"Claude's Workspace\\n\\n\")
    (insert \"Files opened by Claude will appear here.\\n\")
    (insert \"Your left buffer remains untouched.\\n\")
    
    ;; Return focus to left window
    (other-window 1)
    
    ;; Return status
    (format \"Layout initialized. Left: %s, Right: *claude-workspace*\" 
            (buffer-name current-buf))))
" 2>&1)

if [ $? -eq 0 ]; then
    echo -e "${GREEN}Success!${NC}"
    echo "Result: $RESULT"
    echo ""
    echo "Layout ready:"
    echo "  • Left window: Your workspace (preserved)"
    echo "  • Right window: Claude's workspace"
else
    echo -e "${RED}Failed to initialize layout${NC}"
    echo "Error: $RESULT"
    exit 1
fi
