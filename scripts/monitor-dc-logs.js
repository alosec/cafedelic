#!/usr/bin/env node

// Standalone DC log monitor for testing
import { WatcherService } from '../dist/src/services/watcher.service.js';
import { TranslatorService } from '../dist/src/services/translator.service.js';

console.log('Starting Cafedelic DC Log Monitor...\n');

const watcher = new WatcherService();
const translator = new TranslatorService();

// Set up activity display
watcher.on('log-entry', (entry) => {
  const translated = translator.translate(entry);
  console.log(translated);
});

// Start watching
watcher.start().then(() => {
  console.log('Watching Desktop Commander logs...');
  console.log('Press Ctrl+C to stop.\n');
}).catch(error => {
  console.error('Failed to start watcher:', error);
  process.exit(1);
});

// Handle graceful shutdown
process.on('SIGINT', () => {
  console.log('\nStopping monitor...');
  watcher.stop();
  process.exit(0);
});
