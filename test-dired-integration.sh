#!/bin/bash
# test-dired-integration.sh - Test the dired integration feature

echo "Testing Cafedelic Dired Integration"
echo "==================================="

# Check if emacs daemon is running
if ! emacsclient --eval "(emacs-version)" >/dev/null 2>&1; then
    echo "❌ Emacs daemon is not running. Please start it first."
    exit 1
fi

echo "✅ Emacs daemon is running"

# Test the open-dired.sh script directly
echo ""
echo "Testing open-dired.sh script..."
echo "-------------------------------"

# Test with current directory
echo "1. Testing with current directory:"
./scripts/emacs/open-dired.sh . 2>&1

sleep 1

# Test with home directory
echo ""
echo "2. Testing with home directory:"
./scripts/emacs/open-dired.sh ~ 2>&1

sleep 1

# Test with project directory
echo ""
echo "3. Testing with project directory:"
./scripts/emacs/open-dired.sh /home/alex/code/cafedelic 2>&1

echo ""
echo "Test complete! Check your emacs to see if dired opened correctly."
echo ""
echo "To test the full integration:"
echo "1. Make sure Cafedelic MCP server is running with Claude"
echo "2. Ask Claude to list a directory (e.g., 'ls src/services')"
echo "3. The directory should automatically open in dired"