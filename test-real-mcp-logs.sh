#!/bin/bash
# Test script for real Claude Desktop MCP log discovery

echo "🔍 Testing Claude Desktop MCP Log Discovery"
echo "==========================================="

# First, check if the Claude logs directory exists
CLAUDE_LOGS_DIR="/home/alex/.config/Claude/logs"
echo "📁 Checking Claude logs directory: $CLAUDE_LOGS_DIR"

if [ ! -d "$CLAUDE_LOGS_DIR" ]; then
    echo "❌ ERROR: Claude logs directory not found!"
    echo "   Expected: $CLAUDE_LOGS_DIR"
    echo "   Make sure Claude Desktop is installed and has been used."
    exit 1
fi

echo "✅ Claude logs directory exists"

# List all files in the directory
echo ""
echo "📄 Files in Claude logs directory:"
ls -la "$CLAUDE_LOGS_DIR"

echo ""
echo "🔍 Looking for MCP server logs:"
find "$CLAUDE_LOGS_DIR" -name "mcp*.log" -type f 2>/dev/null | while read -r log_file; do
    echo "  📋 Found: $(basename "$log_file") ($(stat -c%s "$log_file") bytes, modified: $(stat -c%y "$log_file"))"
done

# Check specifically for desktop-commander logs
echo ""
echo "🎯 Looking for Desktop Commander logs:"
if [ -f "$CLAUDE_LOGS_DIR/mcp-server-desktop-commander.log" ]; then
    echo "✅ Found Desktop Commander log!"
    echo "   📏 Size: $(stat -c%s "$CLAUDE_LOGS_DIR/mcp-server-desktop-commander.log") bytes"
    echo "   📅 Modified: $(stat -c%y "$CLAUDE_LOGS_DIR/mcp-server-desktop-commander.log")"
    
    echo ""
    echo "📖 Last 10 lines of Desktop Commander log:"
    echo "----------------------------------------"
    tail -n 10 "$CLAUDE_LOGS_DIR/mcp-server-desktop-commander.log" || echo "   (Could not read log file)"
else
    echo "⚠️  Desktop Commander log not found"
    echo "   This may be normal if Desktop Commander hasn't been used recently"
fi

echo ""
echo "🧪 Testing Cafedelic log discovery..."

# Build the project first if needed
if [ ! -d "dist" ]; then
    echo "🔨 Building project..."
    npm run build
fi

# Test the discovery utility
node -e "
import { discoverDesktopMCPLogs } from './dist/src/utils/desktop-mcp-discovery.js';

(async () => {
  try {
    console.log('🔍 Running discovery...');
    const result = await discoverDesktopMCPLogs();
    
    console.log('📊 Discovery Results:');
    console.log('  Found:', result.found);
    console.log('  Log Files:', result.logFiles.length);
    console.log('  Active Count:', result.activeCount);
    
    if (result.error) {
      console.log('  Error:', result.error);
    }
    
    if (result.logFiles.length > 0) {
      console.log('');
      console.log('📋 Discovered Log Files:');
      result.logFiles.forEach((log, index) => {
        console.log(\`  \${index + 1}. \${log.serverName}\`);
        console.log(\`     Path: \${log.path}\`);
        console.log(\`     Size: \${log.size} bytes\`);
        console.log(\`     Modified: \${log.lastModified}\`);
        console.log(\`     Active: \${Date.now() - log.lastModified.getTime() < 10 * 60 * 1000 ? 'Yes' : 'No'}\`);
        console.log('');
      });
    }
    
    console.log('✅ Discovery test completed successfully!');
  } catch (error) {
    console.error('❌ Discovery test failed:', error);
    process.exit(1);
  }
})();
"

echo ""
echo "🎉 Real MCP log discovery test completed!"
