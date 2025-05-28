/**
 * Simple Test Harness
 * Run the file-to-emacs pipeline with debugging
 */

import { runPipeline } from '../core/runner.js';
import { fileToEmacs } from '../pipelines/index.js';

async function main() {
  console.log('Starting cafedelic v2 test...');
  console.log('Watching MCP logs for file operations...');
  console.log('Press Ctrl+C to stop\n');
  
  try {
    await runPipeline(fileToEmacs);
  } catch (error) {
    console.error('Pipeline error:', error);
    process.exit(1);
  }
}

main();