#!/bin/bash
# test-full-frame.sh - Test the new full frame Cafedelic UI

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}═══ Testing Cafedelic Full Frame UI ═══${NC}\n"

echo -e "${YELLOW}Step 1: Initialize the frame${NC}"
./init-claude-frame.sh
echo ""

sleep 1

echo -e "${YELLOW}Step 2: Open several files in sequence${NC}"
echo "Opening test-file.md..."
./open-claude-file.sh test-file.md
sleep 1

echo -e "\nOpening README.md..."
./open-claude-file.sh ../../README.md
sleep 1

echo -e "\nOpening package.json..."
./open-claude-file.sh ../../package.json
sleep 1

echo -e "\nRe-opening test-file.md (should move to top)..."
./open-claude-file.sh test-file.md

echo -e "\n${GREEN}Test complete!${NC}"
echo "Check your emacs:"
echo "  • Top window shows list of accessed files"
echo "  • Bottom window shows current file (test-file.md)"
echo "  • Files are listed with timestamps"
echo "  • Most recent file is at the top"
