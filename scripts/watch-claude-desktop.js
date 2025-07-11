#!/usr/bin/env node
/**
 * Standalone Claude Desktop Log Watcher
 * Simple wrapper to run the log watcher directly
 */

import { watchClaudeDesktopLogs } from '../dist/src/watchers/claude-desktop-log.js';

async function main() {
  console.log('[CLAUDE DESKTOP] Starting log watcher...');
  
  try {
    for await (const entry of watchClaudeDesktopLogs()) {
      // The watcher already logs file operations to console
      // This loop just keeps the process running
    }
  } catch (error) {
    console.error('[ERROR]', error.message);
    process.exit(1);
  }
}

main().catch(console.error);
