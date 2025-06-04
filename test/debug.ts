/**
 * Debug Test Runner with Enhanced Visibility
 * Shows all pipeline activity with clear formatting
 */

import { runPipeline } from '../src/core/runner.js';
import { fileToEmacs } from '../src/pipelines/index.js';

const DIVIDER = '─'.repeat(60);

async function main() {
  console.log(DIVIDER);
  console.log('🚀 Cafedelic v2 Debug Mode');
  console.log(DIVIDER);
  console.log('📍 Log output: Current pane (0:1.1)');
  console.log('📍 Emacs target: Pane 0:1.2');
  console.log(DIVIDER);
  console.log('👀 Watching Desktop Commander logs at:');
  console.log(`   ${process.env.HOME}/.config/Claude/logs/mcp-server-desktop-commander.log`);
  console.log(DIVIDER);
  console.log('🔄 Pipeline: file operations → emacs');
  console.log('');
  console.log('Press Ctrl+C to stop');
  console.log(DIVIDER + '\n');
  
  try {
    await runPipeline(fileToEmacs);
  } catch (error) {
    console.error('\n❌ Pipeline error:', error);
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