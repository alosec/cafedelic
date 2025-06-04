import { WTE } from '../core/wte.js';
import { watchClaudeDesktopLogs } from '../watchers/claude-desktop-log.js';
import { extractFileOperation } from '../transforms/file-operations.js';
import { openInEmacs } from '../executors/emacs.js';
import { LogEntry } from '../watchers/types.js';
import { FileAction } from '../transforms/types.js';

// Wrapper to add console logging
async function* watchWithLogging() {
  console.log('[CLAUDE DESKTOP] Starting MCP log pipeline - file operations tracking');
  console.log('[CLAUDE DESKTOP] Files will open in Emacs as soon as Claude accesses them');
  
  for await (const entry of watchClaudeDesktopLogs()) {
    yield entry;
  }
}

// Execute with claude-desktop source
async function executeWithSource(action: FileAction): Promise<void> {
  await openInEmacs(action, {
    source: 'claude-desktop',
    role: 'editor'
  });
}

export const fileToEmacs: WTE<LogEntry, FileAction> = {
  watch: watchWithLogging,
  transform: extractFileOperation,
  execute: executeWithSource
};