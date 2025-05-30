#!/bin/bash
# Watch Claude Code logs in real-time with formatted output

cd "$(dirname "$0")/.." || exit 1

# Compile TypeScript if needed
if [ -f "tsconfig.json" ]; then
    echo "Compiling TypeScript..."
    npx tsc
fi

# Run the watcher
node dist/test/debug-claude-code.js