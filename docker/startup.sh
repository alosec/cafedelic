#!/bin/bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Server color configuration
case "$CAFEDELIC_SERVER_NAME" in
    red)    COLOR=$RED ;;
    blue)   COLOR=$BLUE ;;
    green)  COLOR=$GREEN ;;
    yellow) COLOR=$YELLOW ;;
    *)      COLOR=$NC ;;
esac

echo -e "${COLOR}Starting Cafedelic Server: $CAFEDELIC_SERVER_NAME${NC}"
echo -e "${COLOR}Port: $CAFEDELIC_PORT${NC}"
echo -e "${COLOR}Data Directory: $CAFEDELIC_DATA_DIR${NC}"
echo -e "${COLOR}Log Directory: $CAFEDELIC_LOG_DIR${NC}"

# Ensure directories exist
mkdir -p "$CAFEDELIC_DATA_DIR" "$CAFEDELIC_LOG_DIR"

# Check if we need to rebuild (for development)
if [ -f "/app/index.ts" ] && [ ! -f "/app/dist/index.js" ]; then
    echo -e "${COLOR}Building TypeScript files...${NC}"
    cd /app && npm run build
fi

# Set up environment for MCP server
export NODE_ENV=${NODE_ENV:-production}

# Future: Handle tmux pane output redirection
if [ -n "$CAFEDELIC_TERMINAL_PANE" ]; then
    echo -e "${COLOR}Terminal pane configured: $CAFEDELIC_TERMINAL_PANE${NC}"
fi

# Start the MCP server
echo -e "${COLOR}Starting Cafedelic MCP server...${NC}"
cd /app

# For Phase 1, just run the server normally
# In future phases, we'll add tmux integration here
exec node dist/index.js