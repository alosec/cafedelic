#!/bin/bash
# emacs-health-check.sh - Check emacs daemon and integration status

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${YELLOW}Emacs Health Check${NC}"
echo "===================="

# Check daemon status
if emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${GREEN}✅ Emacs daemon is running${NC}"
    VERSION=$(emacsclient --eval "(emacs-version)" 2>/dev/null | tr -d '"')
    echo "   Version: $VERSION"
else
    echo -e "${RED}❌ Emacs daemon is not running${NC}"
    echo "   To fix: emacs --daemon"
fi

# Check cafedelic elisp
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/cafedelic-editor.el" ]; then
    echo -e "${GREEN}✅ Cafedelic elisp found${NC}"
    echo "   Path: $SCRIPT_DIR/cafedelic-editor.el"
else
    echo -e "${RED}❌ Cafedelic elisp not found${NC}"
fi

# Check open script
if [ -f "$SCRIPT_DIR/open-claude-file.sh" ]; then
    echo -e "${GREEN}✅ File opener script found${NC}"
    echo "   Path: $SCRIPT_DIR/open-claude-file.sh"
else
    echo -e "${RED}❌ File opener script not found${NC}"
fi

echo ""
echo -e "${YELLOW}Ready for auto-open integration!${NC}"
