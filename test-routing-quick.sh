#!/bin/bash
# Test routing functionality by sending a test message

echo "🧪 Testing Output Routing"
echo "========================"

cd /home/alex/code/cafedelic

# Create a simple routing test
cat > test_routing.js << 'EOF'
import { routingManager } from './dist/src/services/routing-manager.service.js';

(async () => {
  try {
    console.log('🎯 Testing output routing...');
    
    // Set editor destination
    const result = await routingManager.setEditorDestination('0:0.0');
    console.log('📊 Set destination result:', result.success ? '✅ Success' : '❌ Failed');
    
    // Test routing a message
    await routingManager.routeOutput('🎉 Test message from Cafedelic routing system!', 'editor');
    console.log('✅ Test message sent to editor pane');
    
    // Show current assignments
    const assignments = routingManager.getAssignments();
    console.log('📋 Current assignments:', Object.keys(assignments).length);
    
  } catch (error) {
    console.error('❌ Routing test failed:', error.message);
  }
})();
EOF

echo "Running routing test..."
node test_routing.js

echo ""
echo "✅ Routing test completed!"
echo "Check pane 0:0.0 for the test message"

# Clean up
rm -f test_routing.js
