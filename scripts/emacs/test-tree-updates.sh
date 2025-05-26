#!/bin/bash
# test-tree-updates.sh - Test that tree updates dynamically

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${CYAN}═══ Testing Tree Dynamic Updates ═══${NC}\n"

echo -e "${YELLOW}Initializing frame...${NC}"
./init-claude-frame.sh
echo ""

sleep 1

echo -e "${YELLOW}Check Messages buffer for debug output${NC}"
echo "Opening emacsclient to show *Messages* buffer..."
emacsclient --eval "(switch-to-buffer \"*Messages*\")" >/dev/null 2>&1

echo -e "\n${YELLOW}Now opening files one by one...${NC}"
echo "Watch the tree update in the left sidebar!"
echo ""

# Open files with pauses to see updates
echo "1. Opening test-file.md"
./open-claude-file.sh test-file.md
sleep 2

echo "2. Opening README.md"  
./open-claude-file.sh ../../README.md
sleep 2

echo "3. Opening src/index.ts"
./open-claude-file.sh ../../src/index.ts
sleep 2

echo -e "\n${YELLOW}Running diagnostics...${NC}"
./diagnose-emacs.sh

echo -e "\n${GREEN}Check the *Messages* buffer for [Cafedelic] debug logs${NC}"
