/**
 * MCP Log Watcher
 * Watches Claude Desktop logs and emits parsed entries
 */

import { spawn } from 'child_process';
import { LogEntry } from './types.js';

export async function* watchMCPLogs(): AsyncGenerator<LogEntry> {
  const logDir = `${process.env.HOME}/.config/Claude/logs`;
  // Watch the Desktop Commander log specifically
  const tail = spawn('tail', ['-f', '-n', '0', `${logDir}/mcp-server-desktop-commander.log`]);
  
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
        
        console.log(`[WATCH] ${timestamp} | ${content.method || 'response'}`);
        
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