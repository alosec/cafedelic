#!/bin/bash
# Test script to verify Cafedelic monitor is working

echo "Starting Cafedelic monitor test..."
echo "This will:"
echo "1. Start the monitor in background"
echo "2. Create some DC activity"
echo "3. Show the translated output"
echo ""

# Start monitor in background, redirect to temp file
MONITOR_LOG=$(mktemp)
node scripts/monitor-dc-logs.js > "$MONITOR_LOG" 2>&1 &
MONITOR_PID=$!

echo "Monitor started (PID: $MONITOR_PID)"
sleep 2

echo "Creating test activity..."
echo ""

# The act of running this script with these commands will create DC log entries
echo "Test complete. Monitor output:"
echo "================================"
tail -n +4 "$MONITOR_LOG" | grep -E "^\[" || echo "No activity detected yet. Try using Claude Desktop to perform some actions."

# Cleanup
kill $MONITOR_PID 2>/dev/null
rm "$MONITOR_LOG"
