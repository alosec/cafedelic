#!/bin/bash
# test-window-management.sh - Test the two-window Cafedelic setup

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}=== Cafedelic Window Management Test ===${NC}\n"

echo -e "${YELLOW}Step 1: Initialize two-window layout${NC}"
./init-claude-view.sh
echo ""

sleep 1

echo -e "${YELLOW}Step 2: Open README.md in Claude's buffer${NC}"
./open-right.sh ../../README.md
echo ""

sleep 1

echo -e "${YELLOW}Step 3: Open test-file.md in Claude's buffer${NC}"
./open-right.sh test-file.md
echo ""

sleep 1

echo -e "${YELLOW}Step 4: Try to re-open README.md (should switch to existing)${NC}"
./open-right.sh ../../README.md
echo ""

echo -e "${GREEN}Test complete!${NC}"
echo "Check your emacs:"
echo "  • Left window should show your original buffer"
echo "  • Right window should show claude-README.md"
echo "  • Both files should be accessible via buffer switching"
