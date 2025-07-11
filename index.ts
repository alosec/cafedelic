/**
 * Cafedelic v2 Main Entry Point
 */

import { runPipeline } from './src/core/runner.js';
import { fileToEmacs, claudeCodeToEmacs } from './src/pipelines/index.js';
import { paneStartupService } from './src/services/pane-startup.service.js';

console.log('Cafedelic v2 - Watch-Transform-Execute');
console.log('Starting pipelines...\n');

// Start pane monitoring service for automatic emacs server startup
console.log('[PANE MONITOR] Starting automatic emacs server management...');
paneStartupService.startMonitoring().catch(err => {
  console.error('[PANE MONITOR ERROR]', err);
});

// Log pane monitoring events
paneStartupService.on('emacs-server-started', ({ paneId, source }) => {
  console.log(`[PANE MONITOR] ✓ Emacs server started for ${paneId} (source: ${source})`);
});

paneStartupService.on('monitoring-started', () => {
  console.log('[PANE MONITOR] ✓ Monitoring active - will auto-start emacs for editor panes');
});

// Run both pipelines in parallel
Promise.all([
  runPipeline(fileToEmacs).catch(err => {
    console.error('[MCP Pipeline Error]', err);
  }),
  runPipeline(claudeCodeToEmacs).catch(err => {
    console.error('[Claude Code Pipeline Error]', err);
  })
]).catch(console.error);

// Graceful shutdown
process.on('SIGINT', async () => {
  console.log('\n[SHUTDOWN] Gracefully shutting down...');
  await paneStartupService.shutdown();
  process.exit(0);
});

process.on('SIGTERM', async () => {
  console.log('\n[SHUTDOWN] Gracefully shutting down...');
  await paneStartupService.shutdown();
  process.exit(0);
});