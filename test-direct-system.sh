#!/bin/bash
# Direct System Test Script
# Tests the refactored MCP log watching system

echo "🧪 CAFEDELIC REFACTOR DIRECT SYSTEM TEST"
echo "========================================"

echo ""
echo "📋 Current Environment:"
echo "  - Session: 0"
echo "  - Pane 000 (0:0.0): Editor view (emacs)"
echo "  - Pane 001 (0:0.1): MCP watcher"

echo ""
echo "🔧 Step 1: Configure Editor Destination"
echo "Setting editor destination to pane 0:0.0..."

# Test the routing system by sending a direct command
cd /home/alex/code/cafedelic

# Create a test script to set editor destination
cat > test_set_destination.js << 'EOF'
import { routingManager } from './dist/src/services/routing-manager.service.js';

(async () => {
  try {
    console.log('🎯 Setting editor destination to pane 0:0.0...');
    const result = await routingManager.setEditorDestination('0:0.0');
    
    console.log('📊 Result:', JSON.stringify(result, null, 2));
    
    if (result.success) {
      console.log('✅ Editor destination set successfully!');
      console.log('🔧 Emacs server started:', result.serverStarted ? 'Yes' : 'No');
    } else {
      console.log('❌ Failed to set editor destination:', result.message);
    }
    
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
})();
EOF

echo "Executing editor destination setup..."
node test_set_destination.js

echo ""
echo "🔍 Step 2: Verify Current Assignments"

# Create a test script to check assignments
cat > test_get_assignments.js << 'EOF'
import { routingManager } from './dist/src/services/routing-manager.service.js';

(async () => {
  try {
    console.log('📋 Current routing assignments:');
    const assignments = routingManager.getAssignments();
    
    if (Object.keys(assignments).length === 0) {
      console.log('   No assignments configured');
    } else {
      for (const [role, assignment] of Object.entries(assignments)) {
        console.log(`   ${role}: ${assignment.destination} (${assignment.active ? 'active' : 'inactive'})`);
      }
    }
    
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
})();
EOF

echo "Checking current assignments..."
node test_get_assignments.js

echo ""
echo "🎬 Step 3: Start MCP Log Watcher"
echo "Starting MCP log watcher in background..."

# Create a simple MCP log watcher test
cat > test_mcp_watcher.js << 'EOF'
import { DesktopMCPWatcherService } from './dist/src/services/desktop-mcp-watcher.service.js';
import { TranslatorService } from './dist/src/services/translator.service.js';

(async () => {
  try {
    console.log('🔍 Starting Desktop MCP log watcher...');
    
    const watcher = new DesktopMCPWatcherService();
    const translator = new TranslatorService();
    
    // Set up event handling
    watcher.on('log-entry', (entry) => {
      const activity = {
        raw: entry,
        translated: translator.translate(entry),
        timestamp: new Date().toISOString()
      };
      
      console.log(`[${activity.timestamp}] ${activity.translated}`);
    });
    
    watcher.on('error', (error) => {
      console.error('❌ Watcher error:', error.message);
    });
    
    // Start watching
    await watcher.start();
    console.log('✅ MCP log watcher started successfully');
    console.log('📊 Monitoring for Claude Desktop activity...');
    
    // Keep the process running
    process.on('SIGINT', async () => {
      console.log('\n🔴 Stopping MCP log watcher...');
      await watcher.stop();
      process.exit(0);
    });
    
    // Let it run for a while
    setTimeout(() => {
      console.log('🔄 Watcher running... Press Ctrl+C to stop');
    }, 1000);
    
  } catch (error) {
    console.error('❌ Failed to start MCP watcher:', error.message);
    process.exit(1);
  }
})();
EOF

echo "✅ Test scripts prepared"
echo ""
echo "🚀 READY FOR TESTING"
echo "===================="
echo ""
echo "Next steps:"
echo "1. The editor destination has been set to pane 0:0.0"
echo "2. Run 'node test_mcp_watcher.js' in pane 001 to start monitoring"
echo "3. Test activity by using Claude Desktop tools"
echo "4. Verify that activities appear in the watcher output"
echo "5. Verify that files open in pane 0:0.0 (emacs)"
echo ""
echo "Manual testing commands:"
echo "  - Start watcher: 'node test_mcp_watcher.js'"
echo "  - Test routing: 'node test_set_destination.js'"
echo "  - Check assignments: 'node test_get_assignments.js'"

# Clean up test files
rm -f test_set_destination.js test_get_assignments.js test_mcp_watcher.js

echo ""
echo "🎉 Direct system test preparation complete!"
