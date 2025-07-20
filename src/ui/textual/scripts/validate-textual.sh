#!/bin/bash
#
# validate-textual.sh - Shell wrapper for Cafedelic TUI validation
# Provides a clean interface for error checking without GUI launch.
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TUI_DIR="$(dirname "$SCRIPT_DIR")"

echo -e "${BLUE}🔍 Cafedelic TUI Validation${NC}"
echo -e "${BLUE}==============================${NC}"
echo

# Check if we're in the right directory
if [[ ! -f "$TUI_DIR/cafedelic_app.py" ]]; then
    echo -e "${RED}❌ Error: Must be run from textual TUI directory${NC}"
    echo -e "   Expected to find: $TUI_DIR/cafedelic_app.py"
    exit 1
fi

# Change to TUI directory
cd "$TUI_DIR"

# Check for virtual environment
if [[ ! -d "venv" ]]; then
    echo -e "${YELLOW}⚠️  Virtual environment not found${NC}"
    echo -e "   Creating virtual environment..."
    python3 -m venv venv
    echo -e "${GREEN}   Virtual environment created${NC}"
fi

# Activate virtual environment
echo -e "${BLUE}🔧 Activating virtual environment...${NC}"
source venv/bin/activate

# Install/update dependencies if needed
if [[ ! -f "venv/.deps_installed" ]]; then
    echo -e "${BLUE}📦 Installing dependencies...${NC}"
    pip install -r requirements.txt > /dev/null 2>&1
    touch venv/.deps_installed
    echo -e "${GREEN}   Dependencies installed${NC}"
fi

# Run validation
echo -e "${BLUE}🧪 Running validation checks...${NC}"
echo

# Method 1: Use our custom validation script
if python validate_tui.py; then
    echo
    echo -e "${GREEN}✅ Validation successful!${NC}"
    echo -e "${GREEN}   TUI should launch without errors${NC}"
    echo
    echo -e "${BLUE}💡 Usage:${NC}"
    echo -e "   ${YELLOW}python run.py${NC}           - Launch TUI normally"
    echo -e "   ${YELLOW}python run.py --validate${NC} - Run validation only"
    echo -e "   ${YELLOW}textual run --dev cafedelic_app.py${NC} - Launch with Textual dev tools"
    exit 0
else
    echo
    echo -e "${RED}❌ Validation failed${NC}"
    echo -e "${RED}   Fix the errors above before launching TUI${NC}"
    exit 1
fi