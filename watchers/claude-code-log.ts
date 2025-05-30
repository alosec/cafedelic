/**
 * Claude Code Log Watcher
 * Watches Claude Code session logs and emits file operations
 */

import { spawn } from 'child_process';
import { readdir } from 'fs/promises';
import { join } from 'path';
import { LogEntry } from './types.js';

export interface ClaudeCodeLogEntry extends LogEntry {
  sessionId: string;
  messageRole: 'user' | 'assistant' | 'summary';
  toolUse?: {
    id: string;
    name: string;
    input: Record<string, any>;
  };
}

async function findActiveSession(): Promise<string | null> {
  const projectPath = process.cwd().replace(/\//g, '-');
  const sessionDir = `${process.env.HOME}/.claude/projects/${projectPath}`;
  
  try {
    const files = await readdir(sessionDir);
    const sessionFiles = files.filter(f => f.endsWith('.jsonl'));
    
    if (sessionFiles.length === 0) return null;
    
    // Get the most recently modified session
    const sessionPaths = sessionFiles.map(f => join(sessionDir, f));
    const stats = await Promise.all(
      sessionPaths.map(async path => ({
        path,
        mtime: (await import('fs')).statSync(path).mtime
      }))
    );
    
    const mostRecent = stats.sort((a, b) => b.mtime.getTime() - a.mtime.getTime())[0];
    return mostRecent.path;
  } catch (error) {
    console.error('[CLAUDE CODE] Error finding session:', error);
    return null;
  }
}

export async function* watchClaudeCodeLogs(): AsyncGenerator<ClaudeCodeLogEntry> {
  const sessionPath = await findActiveSession();
  
  if (!sessionPath) {
    console.log('[CLAUDE CODE] No active session found');
    return;
  }
  
  console.log('[CLAUDE CODE] Watching session:', sessionPath);
  
  const tail = spawn('tail', ['-f', '-n', '0', sessionPath]);
  const decoder = new TextDecoder();
  
  for await (const chunk of tail.stdout) {
    const lines = decoder.decode(chunk).trim().split('\n');
    
    for (const line of lines) {
      if (!line.trim()) continue;
      
      try {
        const entry = JSON.parse(line);
        
        // Only process assistant messages with tool use
        if (entry.type !== 'assistant' || !entry.message?.content) continue;
        
        // Extract tool use from content array
        const content = entry.message.content;
        if (!Array.isArray(content)) continue;
        
        for (const item of content) {
          if (item.type === 'tool_use' && item.name === 'Read') {
            console.log(`[CLAUDE CODE] File read: ${item.input.file_path}`);
            
            yield {
              timestamp: entry.timestamp || new Date().toISOString(),
              component: 'claude-code',
              type: 'file-read',
              sessionId: entry.sessionId || sessionPath,
              messageRole: 'assistant',
              toolUse: {
                id: item.id,
                name: item.name,
                input: item.input
              },
              content: {
                filePath: item.input.file_path,
                operation: 'read'
              },
              raw: line
            };
          }
        }
      } catch (error) {
        console.error('[CLAUDE CODE] Parse error:', error);
      }
    }
  }
}