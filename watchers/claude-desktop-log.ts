/**
 * Claude Desktop Log Watcher
 * Watches Claude Desktop MCP logs and emits parsed entries
 */

import { spawn } from 'child_process';
import { LogEntry } from './types.js';

export async function* watchClaudeDesktopLogs(): AsyncGenerator<LogEntry> {
  const logDir = `${process.env.HOME}/.config/Claude/logs`;
  const logFile = `${logDir}/mcp-server-desktop-commander.log`;
  // Watch the Desktop Commander log specifically
  const tail = spawn('tail', ['-f', '-n', '0', logFile]);
  
  console.log('[CLAUDE DESKTOP] Watching session:', logFile);
  
  const decoder = new TextDecoder();
  
  for await (const chunk of tail.stdout) {
    const lines = decoder.decode(chunk).trim().split('\n');
    
    for (const line of lines) {
      if (!line.trim()) continue;
      
      // Skip tail file headers (==> filename <==)
      if (line.startsWith('==>') && line.endsWith('<==')) {
        continue;
      }
      
      try {
        // Desktop Commander logs are in format: timestamp [component] [level] message
        const match = line.match(/^(\S+)\s+\[([^\]]+)\]\s+\[([^\]]+)\]\s+(.*)$/);
        if (!match) continue;
        
        const [, timestamp, component, level, message] = match;
        
        // Parse the JSON message content
        if (!message.startsWith('Message from')) continue;
        
        const jsonMatch = message.match(/Message from \w+: (.*)$/);
        if (!jsonMatch) continue;
        
        const content = JSON.parse(jsonMatch[1]);
        
        // Only log actual file operations, skip MCP protocol noise
        if (content.method === 'tools/call') {
          const toolName = content.params?.name;
          const args = content.params?.arguments;
          
          // Only log file-related operations
          if (toolName && ['read_file', 'write_file', 'edit_block', 'list_directory', 'search_files', 'search_code'].includes(toolName)) {
            const filePath = args?.path || args?.file_path || args?.pattern;
            if (filePath) {
              console.log(`[CLAUDE DESKTOP] ${toolName}(${filePath})`);
            } else {
              console.log(`[CLAUDE DESKTOP] ${toolName}()`);
            }
          }
        }
        // Skip logging responses and protocol messages entirely
        
        yield {
          timestamp,
          component,
          type: content.method ? 'request' : 'response',
          content,
          raw: line
        };
      } catch (error) {
        // Only log if it's not a header line
        if (!line.includes('mcp-server-') && !line.includes('.log')) {
          console.error('[PARSE ERROR]', line.substring(0, 50) + '...');
        }
      }
    }
  }
}