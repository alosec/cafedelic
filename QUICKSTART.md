# Cafedelic Quick Start Guide

## Prerequisites

1. Claude Desktop with Desktop Commander installed
2. Node.js 18+
3. Unix-like environment (Linux/macOS)

## Setup

1. Clone and build:
```bash
git clone <repo>
cd cafedelic
npm install
npm run build
```

2. Configure Claude Desktop:
Add to your MCP settings:
```json
{
  "cafedelic": {
    "command": "node",
    "args": ["/absolute/path/to/cafedelic/dist/index.js"]
  }
}
```

3. Restart Claude Desktop

## Testing

### Option 1: Standalone Monitor
```bash
# In one terminal, start the monitor
node scripts/monitor-dc-logs.js

# In Claude Desktop, perform some actions like:
# - Read a file
# - Search for text
# - Execute a command
# Watch the translations appear in real-time!
```

**Note**: The monitor only shows NEW activity. If Claude Desktop isn't actively using Desktop Commander, you won't see any output.

### Option 2: MCP Tool
In Claude Desktop, ask:
"Use the cafedelic get_active_context tool to show me recent activity"

## What You'll See

Real-time translations of Claude's actions:
```
[2:15pm] Claude is reading package.json
[2:15pm] Claude searched for "router" in src/
[2:16pm] Claude is updating app.ts
[2:16pm] Claude executed: npm run build
```

Activity summaries:
```
Files Under Review:
- package.json
- app.ts
- router.ts

Detected Pattern: Heavy file editing

Activity Summary:
- read_file: 5 times
- write_file: 3 times
- search_code: 2 times
```

## Troubleshooting

### No activity showing
- Check if DC log exists: `ls ~/.claude-server-commander/`
- Ensure Claude Desktop is using Desktop Commander
- Try performing a file operation in Claude

### Build errors
- Run `npm install` to ensure dependencies
- Check Node.js version: `node --version` (need 18+)

## Next Steps

- Experiment with different `lookback_minutes` values
- Watch for patterns in your development workflow
- Check memory-bank/ for deeper documentation
