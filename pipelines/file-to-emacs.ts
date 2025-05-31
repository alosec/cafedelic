/**
 * File to Emacs Pipeline
 * Watches MCP logs for file operations and opens them in Emacs
 */

import { WTE } from '../core/wte.js';
import { watchClaudeDesktopLogs } from '../watchers/claude-desktop-log.js';
import { extractFileOperation } from '../transforms/file-operations.js';
import { openInEmacs } from '../executors/emacs.js';
import { LogEntry } from '../watchers/types.js';
import { FileAction } from '../transforms/types.js';

// Execute with claude-desktop source
async function executeWithSource(action: FileAction): Promise<void> {
  await openInEmacs(action, {
    source: 'claude-desktop',
    role: 'editor'
  });
}

export const fileToEmacs: WTE<LogEntry, FileAction> = {
  watch: watchClaudeDesktopLogs,
  transform: extractFileOperation,
  execute: executeWithSource
};