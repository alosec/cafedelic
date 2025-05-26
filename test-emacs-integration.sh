#!/bin/bash
# test-emacs-integration.sh - Test the new emacs auto-open functionality

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}🧪 Testing Cafedelic Emacs Integration${NC}"
echo ""

# Check if emacs daemon is running
echo -e "${YELLOW}Checking emacs daemon status...${NC}"
if emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo -e "${GREEN}✅ Emacs daemon is running${NC}"
else
    echo -e "${RED}❌ Emacs daemon is not running${NC}"
    echo "Please start emacs daemon with: emacs --daemon"
    exit 1
fi

# Check if build completed
if [ ! -f "dist/index.js" ]; then
    echo -e "${RED}❌ Build not found. Running npm run build...${NC}"
    npm run build
fi

echo ""
echo -e "${YELLOW}Testing configuration system...${NC}"

# Test configuration by checking if the files exist
node -e "
import { configManager } from './dist/src/config/cafedelic.config.js';
import { emacsService } from './dist/src/services/emacs.service.js';

console.log('✅ Configuration loaded:', configManager.isAutoOpenEnabled() ? 'auto-open enabled' : 'auto-open disabled');
console.log('✅ EmacsService initialized');

// Test health check
emacsService.checkEmacsHealth().then(health => {
  console.log('✅ Health check:', health.message);
}).catch(err => {
  console.error('❌ Health check failed:', err.message);
});
"

echo ""
echo -e "${GREEN}🎉 Integration test completed!${NC}"
echo ""
echo -e "${BLUE}Next steps:${NC}"
echo "1. Start the MCP server: node dist/index.js"
echo "2. Test with Claude Desktop using new tools:"
echo "   - get_emacs_status"
echo "   - toggle_auto_open"
echo "3. Test auto-opening by having Claude read a file"
