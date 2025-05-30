/**
 * Claude Code to Emacs Pipeline
 * Watches Claude Code logs and opens files in Emacs with read-only mode
 */

import { watchClaudeCodeLogs, ClaudeCodeLogEntry } from '../watchers/claude-code-log.js';
import { extractClaudeCodeOperation } from '../transforms/claude-code-operations.js';
import { openInEmacs } from '../executors/emacs.js';
import { FileAction } from '../transforms/types.js';

// Simple pipeline that opens files immediately
export async function claudeCodeToEmacs() {
  console.log('[CLAUDE CODE] Starting simple pipeline');
  
  for await (const entry of watchClaudeCodeLogs()) {
    const action = extractClaudeCodeOperation(entry);
    if (action) {
      await openInEmacs(action, { readOnly: true });
    }
  }
}

// Debounced pipeline that batches file operations
export async function claudeCodeToEmacsDebounced() {
  console.log('[CLAUDE CODE] Starting debounced pipeline');
  
  const fileQueue = new Map<string, FileAction>();
  let debounceTimer: NodeJS.Timeout | null = null;
  
  async function flushQueue() {
    if (fileQueue.size === 0) return;
    
    const actions = Array.from(fileQueue.values());
    console.log(`[CLAUDE CODE] Opening ${actions.length} files`);
    
    // Open files sequentially with small delay
    for (const action of actions) {
      await openInEmacs(action, { readOnly: true });
      await new Promise(resolve => setTimeout(resolve, 200));
    }
    
    fileQueue.clear();
  }
  
  for await (const entry of watchClaudeCodeLogs()) {
    const action = extractClaudeCodeOperation(entry);
    if (!action) continue;
    
    // Add to queue
    fileQueue.set(action.path, action);
    
    // Reset debounce timer
    if (debounceTimer) {
      clearTimeout(debounceTimer);
    }
    
    // Set new timer
    debounceTimer = setTimeout(() => {
      flushQueue().catch(console.error);
      debounceTimer = null;
    }, 5000); // 5 second window
  }
  
  // Flush any remaining on exit
  await flushQueue();
}