/**
 * Claude Code Log Watcher Debug Mode
 * Shows all Claude Code file operations with clear formatting
 */

import { watchClaudeCodeLogs } from '../watchers/claude-code-log.js';

const DIVIDER = '─'.repeat(60);
const COLORS = {
  read: '\x1b[36m',    // Cyan
  edit: '\x1b[33m',    // Yellow
  write: '\x1b[32m',   // Green
  reset: '\x1b[0m'
};

function formatTimestamp(ts: string): string {
  const date = new Date(ts);
  return date.toLocaleTimeString('en-US', { 
    hour12: false,
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit'
  });
}

function formatOperation(op: string): string {
  const color = COLORS[op as keyof typeof COLORS] || COLORS.reset;
  return `${color}${op.toUpperCase().padEnd(5)}${COLORS.reset}`;
}

async function main() {
  console.log(DIVIDER);
  console.log('🔍 Claude Code Log Watcher');
  console.log(DIVIDER);
  console.log('📂 Session directory:');
  console.log(`   ${process.env.HOME}/.claude/projects/`);
  console.log(DIVIDER);
  console.log('🎯 Watching for file operations:');
  console.log('   • READ  - File opened by Claude');
  console.log('   • EDIT  - File modified by Claude');
  console.log('   • WRITE - New file created by Claude');
  console.log(DIVIDER);
  console.log('Press Ctrl+C to stop');
  console.log(DIVIDER + '\n');
  
  try {
    let operationCount = 0;
    
    for await (const entry of watchClaudeCodeLogs()) {
      operationCount++;
      
      const time = formatTimestamp(entry.timestamp);
      const op = formatOperation(entry.content.operation);
      const path = entry.content.filePath;
      
      // Main output line
      console.log(`[${time}] ${op} ${path}`);
      
      // Additional details for edits/writes
      if (entry.toolUse && entry.toolUse.name === 'Edit') {
        const { old_string, new_string } = entry.toolUse.input;
        if (old_string && new_string) {
          const oldPreview = old_string.split('\n')[0].substring(0, 50);
          const newPreview = new_string.split('\n')[0].substring(0, 50);
          console.log(`           └─ "${oldPreview}..." → "${newPreview}..."`);
        }
      }
    }
  } catch (error) {
    console.error('\n❌ Watcher error:', error);
    process.exit(1);
  }
}

// Handle graceful shutdown
process.on('SIGINT', () => {
  console.log('\n' + DIVIDER);
  console.log('👋 Shutting down gracefully...');
  process.exit(0);
});

main();