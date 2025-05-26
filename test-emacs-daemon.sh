#!/bin/bash
# Test script for Emacs daemon management

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=== Emacs Daemon Management Test ===${NC}"
echo

# Test 1: Check socket detection
echo -e "${YELLOW}Test 1: Socket Detection${NC}"
./scripts/emacs/detect-socket.sh && echo -e "${GREEN}✓ Found existing socket${NC}" || echo -e "${YELLOW}⚠ No existing socket found${NC}"
echo

# Test 2: Daemon health check
echo -e "${YELLOW}Test 2: Daemon Health Check${NC}"
./scripts/emacs/daemon-health.sh || echo -e "${YELLOW}⚠ Daemon not healthy or not running${NC}"
echo

# Test 3: Start minimal daemon (if needed)
echo -e "${YELLOW}Test 3: Minimal Daemon Start${NC}"
if ! ./scripts/emacs/detect-socket.sh cafedelic-test >/dev/null 2>&1; then
    echo "Starting test daemon..."
    ./scripts/emacs/minimal-daemon-start.sh cafedelic-test
else
    echo -e "${GREEN}Test daemon already running${NC}"
fi
echo

# Test 4: Test file open with socket
echo -e "${YELLOW}Test 4: File Open with Socket${NC}"
export CAFEDELIC_SOCKET_NAME="cafedelic-test"
echo "Testing with socket: $CAFEDELIC_SOCKET_NAME"

# Create a test file
TEST_FILE="/tmp/cafedelic-test-file.txt"
echo "This is a test file for Cafedelic" > "$TEST_FILE"

# Try to open it
./scripts/emacs/open-claude-file-v2.sh "$TEST_FILE"
echo

# Test 5: Directory open test
echo -e "${YELLOW}Test 5: Directory Open Test${NC}"
./scripts/emacs/open-dired-v2.sh "/tmp"
echo

# Test 6: Daemon health with JSON output
echo -e "${YELLOW}Test 6: JSON Health Check${NC}"
./scripts/emacs/daemon-health.sh cafedelic-test json | jq . || echo "jq not installed, showing raw output"
echo

# Cleanup
echo -e "${YELLOW}Cleanup: Stopping test daemon${NC}"
emacsclient --socket-name=cafedelic-test --eval "(kill-emacs)" 2>/dev/null || true
rm -f "$TEST_FILE"

echo -e "${GREEN}✓ All tests completed${NC}"