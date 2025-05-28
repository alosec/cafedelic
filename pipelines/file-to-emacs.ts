/**
 * File to Emacs Pipeline
 * Watches MCP logs for file operations and opens them in Emacs
 */

import { WTE } from '../core/wte.js';
import { watchMCPLogs } from '../watchers/mcp-log.js';
import { extractFileOperation } from '../transforms/file-operations.js';
import { openInEmacs } from '../executors/emacs.js';
import { LogEntry } from '../watchers/types.js';
import { FileAction } from '../transforms/types.js';

export const fileToEmacs: WTE<LogEntry, FileAction> = {
  watch: watchMCPLogs,
  transform: extractFileOperation,
  execute: openInEmacs
};