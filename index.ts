/**
 * Cafedelic v2 Main Entry Point
 */

import { runPipeline } from './core/runner.js';
import { fileToEmacs } from './pipelines/index.js';

console.log('Cafedelic v2 - Watch-Transform-Execute');
console.log('Starting file-to-emacs pipeline...\n');

runPipeline(fileToEmacs).catch(console.error);