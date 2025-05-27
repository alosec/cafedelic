import { DesktopMCPWatcherService } from './dist/src/services/desktop-mcp-watcher.service.js';
import { TranslatorService } from './dist/src/services/translator.service.js';
import { routingManager } from './dist/src/services/routing-manager.service.js';

console.log('🔍 Starting Cafedelic MCP Log Watcher');
console.log('====================================');

(async () => {
  try {
    // Initialize services
    const watcher = new DesktopMCPWatcherService();
    const translator = new TranslatorService();
    
    // Show current routing configuration
    console.log('📋 Current routing assignments:');
    const assignments = routingManager.getAssignments();
    if (Object.keys(assignments).length === 0) {
      console.log('   ⚠️  No routing assignments found');
      console.log('   💡 Try running: setEditorDestination("0:0.0")');
    } else {
      for (const [role, assignment] of Object.entries(assignments)) {
        console.log(`   ✅ ${role}: ${assignment.destination} (${assignment.active ? 'active' : 'inactive'})`);
      }
    }
    
    console.log('');
    console.log('🎬 Starting MCP log monitoring...');
    
    // Set up event handling
    watcher.on('log-entry', (entry) => {
      const activity = {
        raw: entry,
        translated: translator.translate(entry),
        timestamp: new Date().toISOString()
      };
      
      console.log(`🔍 [${activity.timestamp.substring(11, 19)}] ${activity.translated}`);
      
      // Route output to designated pane if configured
      routingManager.routeOutput(activity.translated, 'editor').catch(err => {
        console.log(`   ⚠️  Routing failed: ${err.message}`);
      });
      
      // Show file access for debugging
      if (entry.command === 'read_file' && entry.args?.path) {
        console.log(`   📂 File accessed: ${entry.args.path}`);
      }
    });
    
    watcher.on('error', (error) => {
      console.error('❌ Watcher error:', error.message);
    });
    
    // Start watching
    await watcher.start();
    console.log('✅ MCP log watcher started successfully');
    console.log('📊 Monitoring Claude Desktop MCP logs...');
    console.log('');
    console.log('💡 Usage:');
    console.log('   - Use Claude Desktop tools to generate activity');
    console.log('   - Watch for real-time activity translations here');
    console.log('   - Check pane 0:0.0 for automatic file opening');
    console.log('   - Press Ctrl+C to stop monitoring');
    console.log('');
    
    // Graceful shutdown
    process.on('SIGINT', async () => {
      console.log('\n🔴 Stopping MCP log watcher...');
      await watcher.stop();
      console.log('👋 Goodbye!');
      process.exit(0);
    });
    
  } catch (error) {
    console.error('❌ Failed to start MCP watcher:', error.message);
    console.error('🔧 Stack trace:', error.stack);
    process.exit(1);
  }
})();
