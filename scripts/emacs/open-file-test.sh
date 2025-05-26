#!/bin/bash
# open-file-test.sh - Hello World prototype for emacsclient integration
# Tests basic file opening via emacs daemon

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if file path provided
if [ $# -eq 0 ]; then
    echo -e "${RED}Error: No file path provided${NC}"
    echo "Usage: $0 <file-path>"
    exit 1
fi

FILE_PATH="$1"
echo -e "${YELLOW}Attempting to open file:${NC} $FILE_PATH"

# Test if emacs daemon is running
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${RED}Error: Emacs daemon is not running${NC}"
    echo "Start it with: emacs --daemon"
    exit 1
fi

# Attempt to open the file
echo -e "${YELLOW}Sending command to emacs...${NC}"
RESULT=$(emacsclient --eval "(progn
  (find-file \"$FILE_PATH\")
  (format \"Opened buffer: %s\" (buffer-name)))" 2>&1)

# Check if command succeeded
if [ $? -eq 0 ]; then
    echo -e "${GREEN}Success!${NC}"
    echo "Emacs response: $RESULT"
    
    # Also try to get current buffer info
    BUFFER_INFO=$(emacsclient --eval "(format \"Current buffer: %s, File: %s\" 
                                        (buffer-name) 
                                        (or (buffer-file-name) \"no file\"))")
    echo "Buffer info: $BUFFER_INFO"
else
    echo -e "${RED}Error executing emacsclient command${NC}"
    echo "Error output: $RESULT"
    exit 1
fi
