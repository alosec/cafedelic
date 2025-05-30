/**
 * Cafedelic v2 Main Entry Point
 */

import { runPipeline } from './core/runner.js';
import { fileToEmacs, claudeCodeToEmacs } from './pipelines/index.js';

console.log('Cafedelic v2 - Watch-Transform-Execute');
console.log('Starting pipelines...\n');

// Run both pipelines in parallel
Promise.all([
  runPipeline(fileToEmacs).catch(err => {
    console.error('[MCP Pipeline Error]', err);
  }),
  runPipeline(claudeCodeToEmacs).catch(err => {
    console.error('[Claude Code Pipeline Error]', err);
  })
]).catch(console.error);