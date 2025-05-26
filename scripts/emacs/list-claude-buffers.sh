#!/bin/bash
# list-claude-buffers.sh - List all buffers managed by Claude (claude-* prefix)

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}Claude's Open Buffers:${NC}"

# Check emacs daemon
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    exit 1
fi

# Get list of claude-prefixed buffers
RESULT=$(emacsclient --eval "
(let ((claude-buffers 
       (seq-filter 
        (lambda (buf)
          (string-prefix-p \"claude-\" (buffer-name buf)))
        (buffer-list))))
  (if claude-buffers
      (mapconcat 
       (lambda (buf)
         (format \"  • %s → %s\"
                 (buffer-name buf)
                 (or (buffer-file-name buf) \"no file\")))
       claude-buffers
       \"\\n\")
    \"  No Claude buffers open\"))
" 2>&1)

if [ $? -eq 0 ]; then
    # Clean up the output (remove quotes)
    CLEANED=$(echo "$RESULT" | sed 's/^"//;s/"$//' | sed 's/\\n/\n/g')
    echo -e "$CLEANED"
    
    # Count buffers
    COUNT=$(emacsclient --eval "
      (length (seq-filter 
               (lambda (buf)
                 (string-prefix-p \"claude-\" (buffer-name buf)))
               (buffer-list)))" 2>/dev/null)
    
    echo -e "\n${GREEN}Total Claude buffers: $COUNT${NC}"
else
    echo -e "${RED}Failed to list buffers${NC}"
    echo "Error: $RESULT"
fi
