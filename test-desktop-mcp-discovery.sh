#!/bin/bash
# Test script for Desktop MCP log discovery

echo "Testing Desktop MCP Log Discovery"
echo "================================="

# Run discovery through node
node -e "
import { discoverDesktopMCPLogs, findActiveDesktopMCPLog } from './dist/src/utils/desktop-mcp-discovery.js';

(async () => {
  console.log('\nDiscovering all Desktop MCP logs...');
  const discovery = await discoverDesktopMCPLogs();
  
  console.log('\nDiscovery Result:');
  console.log('- Found:', discovery.found);
  console.log('- Total log files:', discovery.logFiles.length);
  console.log('- Active logs:', discovery.activeCount);
  
  if (discovery.error) {
    console.log('- Error:', discovery.error);
  }
  
  if (discovery.logFiles.length > 0) {
    console.log('\nLog Files:');
    discovery.logFiles.slice(0, 5).forEach((log, i) => {
      console.log(\`  \${i + 1}. \${log.serverName}\`);
      console.log(\`     Path: \${log.path}\`);
      console.log(\`     Window: \${log.windowId}\`);
      console.log(\`     Last Modified: \${log.lastModified}\`);
      console.log(\`     Size: \${log.size} bytes\`);
    });
  }
  
  console.log('\n\nFinding most active log...');
  const activeLog = await findActiveDesktopMCPLog();
  
  if (activeLog) {
    console.log('\nActive Log Found:');
    console.log('- Server:', activeLog.serverName);
    console.log('- Path:', activeLog.path);
    console.log('- Last Modified:', activeLog.lastModified);
  } else {
    console.log('\nNo active log found.');
  }
})();
" 2>&1

echo "\n\nChecking VS Code logs directory..."
if [ -d "$HOME/.config/Code/logs" ]; then
    echo "VS Code logs directory exists."
    echo "Recent log directories:"
    ls -lt "$HOME/.config/Code/logs" | head -5
else
    echo "VS Code logs directory not found at ~/.config/Code/logs"
fi
