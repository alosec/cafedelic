#!/bin/bash
# test-stable-tree.sh - Test the stable left-side tree layout

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${CYAN}═══ Testing Stable Tree Layout ═══${NC}\n"

echo -e "${YELLOW}Initializing frame with left tree sidebar...${NC}"
./init-claude-frame.sh
echo ""

# Small pause to see initial state
sleep 1

echo -e "${YELLOW}Opening files to build tree...${NC}"

# Open files one by one to see tree build up
echo "Opening: test-file.md"
./open-claude-file.sh test-file.md
sleep 0.5

echo "Opening: README.md"
./open-claude-file.sh ../../README.md
sleep 0.5

echo "Opening: package.json"
./open-claude-file.sh ../../package.json
sleep 0.5

echo "Opening: src/index.ts"
./open-claude-file.sh ../../src/index.ts
sleep 0.5

echo "Opening: src/services/watcher.service.ts"
./open-claude-file.sh ../../src/services/watcher.service.ts
sleep 0.5

echo "Opening: memory-bank/activeContext.md"
./open-claude-file.sh ../../memory-bank/activeContext.md

echo -e "\n${GREEN}Test complete!${NC}"
echo "You should see:"
echo "  • Left sidebar with file tree structure"
echo "  • Files organized by directory"
echo "  • Right side showing current file"
echo "  • No extra windows or buffers"

# Show buffer count for diagnostics
echo -e "\n${YELLOW}Diagnostics:${NC}"
BUFFER_COUNT=$(emacsclient --eval "(length (buffer-list))" 2>/dev/null)
echo "Total buffers: $BUFFER_COUNT"

WINDOW_COUNT=$(emacsclient --eval "(count-windows)" 2>/dev/null)
echo "Total windows: $WINDOW_COUNT (should be 2)"
