#!/bin/bash
# test-rapid-flip.sh - Test rapid file flipping with tree view

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}═══ Testing Rapid File Context Window ═══${NC}\n"

echo -e "${YELLOW}Initializing frame...${NC}"
./init-claude-frame.sh
echo ""

# Small pause to see initial state
sleep 0.5

echo -e "${YELLOW}Rapidly accessing multiple files...${NC}"

# Access files rapidly to create flipping effect
./open-claude-file.sh test-file.md
./open-claude-file.sh ../../README.md
./open-claude-file.sh ../../package.json
./open-claude-file.sh ../../src/index.ts
./open-claude-file.sh ../../src/services/watcher.service.ts
./open-claude-file.sh ../../src/services/translator.service.ts
./open-claude-file.sh ../../src/tools/get-active-context.ts
./open-claude-file.sh ../../memory-bank/activeContext.md
./open-claude-file.sh ../../memory-bank/progress.md

# Go back to an earlier file
./open-claude-file.sh ../../README.md

echo -e "\n${GREEN}Test complete!${NC}"
echo "You should see:"
echo "  • A file tree structure in the top window"
echo "  • Files flipping rapidly in the main window"
echo "  • Tree organizing by directory structure"
