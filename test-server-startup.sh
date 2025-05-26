#!/bin/bash
# Quick test to verify server starts without errors

cd /home/alex/code/cafedelic

echo "Starting Cafedelic server..."
timeout 3 npm start 2>&1 | tee startup.log &
PID=$!

sleep 2

# Check if process is still running
if ps -p $PID > /dev/null; then
    echo "✓ Server started successfully"
    kill $PID 2>/dev/null
else
    echo "✗ Server failed to start"
    cat startup.log
fi

# Check for errors in log
if grep -i "error" startup.log > /dev/null; then
    echo "⚠ Errors found in startup log:"
    grep -i "error" startup.log
else
    echo "✓ No errors in startup log"
fi

# Check latest log file
LOG_FILE=$(ls -t logs/cafedelic-*.log 2>/dev/null | head -1)
if [ -n "$LOG_FILE" ]; then
    echo "Recent log entries:"
    tail -10 "$LOG_FILE"
fi

rm -f startup.log