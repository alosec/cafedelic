#!/bin/bash
# Test script for new routing functionality

echo "🔧 Testing Cafedelic Routing System"
echo "==================================="

# First ensure project is built
echo "🔨 Building project..."
npm run build

echo ""
echo "🧪 Testing MCP server startup..."

# Start the MCP server in background with timeout
timeout 10s node dist/index.js <<EOF &
{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}
EOF

# Wait a moment for startup
sleep 2

echo ""
echo "📋 Testing routing tools availability..."

# Test that we can list tools (this verifies the server starts)
echo '{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}' | timeout 5s node dist/index.js 2>/dev/null | grep -q "set_editor_destination"

if [ $? -eq 0 ]; then
    echo "✅ New routing tools are available"
else
    echo "❌ New routing tools not found"
fi

echo ""
echo "🎯 Testing tmux pane detection..."

# Check current tmux session and panes
if command -v tmux &> /dev/null; then
    echo "✅ tmux is available"
    
    # List current sessions
    echo "📊 Current tmux sessions:"
    tmux list-sessions 2>/dev/null || echo "   No active sessions"
    
    echo ""
    echo "🔍 Current panes in session 0 (if exists):"
    tmux list-panes -t 0 -F '#{session_name}:#{window_index}.#{pane_index} - #{pane_title}' 2>/dev/null || echo "   Session 0 not found"
    
else
    echo "❌ tmux not available - routing system requires tmux"
fi

echo ""
echo "📖 Usage Examples:"
echo "=================="
echo "To set editor destination to pane 0:0.1:"
echo '  {"jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": {"name": "set_editor_destination", "arguments": {"paneSpec": "0:0.1"}}}'
echo ""
echo "To get routing assignments:"
echo '  {"jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": {"name": "get_routing_assignments", "arguments": {}}}'
echo ""
echo "To clear editor assignment:"
echo '  {"jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": {"name": "clear_routing_assignment", "arguments": {"role": "editor"}}}'

echo ""
echo "🎉 Routing system test completed!"
echo ""
echo "💡 Next steps:"
echo "1. Start tmux if not running: tmux new-session -d -s main"
echo "2. Create panes: tmux split-window -t main"
echo "3. Test: setEditorDestination('0:0.1')"
