#!/bin/bash
# Test script for Desktop MCP log parsing

echo "Testing Desktop MCP Log Parsing"
echo "==============================="

# Sample log lines for testing
cat > /tmp/test-mcp-log.txt << 'EOF'
2025-05-26T08:20:05.428Z [INFO] Server starting
2025-05-26T08:20:42.682Z [DEBUG] Tool execution: read_file with args: {"path": "/home/alex/project/README.md"}
2025-05-26T08:20:42.683Z [INFO] Tool completed: read_file (success)
2025-05-26T08:21:15.123Z [DEBUG] Received request: method=tools/call, params={"name":"write_file","arguments":{"path":"/home/alex/test.js","content":"console.log('test');"}}
2025-05-26T08:21:15.456Z [INFO] Tool completed: write_file (failed)
2025-05-26T08:22:00.789Z [ERROR] Error processing request: Connection timeout
Invalid log line without proper format
2025-05-26T08:23:10.111Z [DEBUG] Tool execution: list_directory with args: {"path": "/home/alex/code"}
EOF

echo "\nParsing test log lines..."
node -e "
import { DesktopMCPWatcherService } from './dist/src/services/desktop-mcp-watcher.service.js';
import fs from 'fs';

const watcher = new DesktopMCPWatcherService();

// Access private method through prototype
const parseLogLine = watcher.constructor.prototype.parseLogLine;
const extractToolCall = watcher.constructor.prototype.extractToolCall;

const lines = fs.readFileSync('/tmp/test-mcp-log.txt', 'utf8').split('\\n').filter(l => l.trim());

console.log('\\nParsing results:');
console.log('================\\n');

lines.forEach((line, i) => {
  console.log(\`Line \${i + 1}: \${line.substring(0, 80)}...\`);
  
  const result = parseLogLine.call(watcher, line);
  
  if (result.success && result.entry) {
    console.log('  ✓ Parsed successfully');
    console.log('    - Timestamp:', result.entry.timestamp);
    console.log('    - Level:', result.entry.level);
    console.log('    - Message:', result.entry.message.substring(0, 50) + '...');
    
    if (result.entry.toolCall) {
      console.log('    - Tool Call:');
      console.log('      • Name:', result.entry.toolCall.name);
      console.log('      • Status:', result.entry.toolCall.status);
      console.log('      • Args:', JSON.stringify(result.entry.toolCall.args).substring(0, 50) + '...');
    }
  } else {
    console.log('  ✗ Parse failed:', result.error);
  }
  console.log('');
});

// Test the emit functionality
console.log('\\nTesting event emission:');
console.log('======================\\n');

let emittedCount = 0;
watcher.on('log-entry', (entry) => {
  emittedCount++;
  console.log(\`Emitted compatible entry #\${emittedCount}:\`);
  console.log('  - Command:', entry.command);
  console.log('  - Timestamp:', entry.timestamp);
  console.log('  - Args:', JSON.stringify(entry.args).substring(0, 60) + '...');
});

watcher.on('mcp-entry', (entry) => {
  console.log('\\nEmitted rich MCP entry:');
  console.log('  - Tool:', entry.toolCall?.name);
  console.log('  - Status:', entry.toolCall?.status);
});

// Process buffer simulation
const processBuffer = watcher.constructor.prototype.processBuffer;
processBuffer.call(watcher, fs.readFileSync('/tmp/test-mcp-log.txt', 'utf8'));

console.log(\`\\nTotal compatible entries emitted: \${emittedCount}\`);
" 2>&1

echo "\n\nCleaning up..."
rm -f /tmp/test-mcp-log.txt
