# Claude Desktop MCP Log System Documentation

## Overview

Claude Desktop generates comprehensive logs for MCP (Model Context Protocol) servers and general application activity. These logs provide visibility into AI-assisted development workflows and tool executions.

## Primary Log Locations

### 1. Claude Desktop MCP Logs (Primary)
**Location**: `/home/alex/.config/Claude/logs/`
**Format**: Structured log files with timestamps and JSON payloads
**Content**: MCP server communications, tool executions, and application events

#### Available Log Files

**MCP Server Logs:**
- `mcp-server-cafedelic.log` - Cafedelic intelligence framework server
- `mcp-server-browser-use.log` - Browser automation server  
- `mcp-server-github.log` - GitHub integration server
- `mcp-server-notion.log` - Notion workspace integration
- `mcp-server-playwright-*.log` - Various Playwright automation servers
- `mcp-server-puppeteer.log` - Puppeteer web automation
- `mcp-server-supabase.log` - Supabase database integration
- `mcp-server-webresearch.log` - Web research capabilities
- `mcp-server-desktop-commander.log` - Desktop Commander tool server
- `mcp-server-firecrawl-mcp.log` - Firecrawl web scraping server

**General MCP Logs:**
- `mcp.log` - Main MCP protocol communication log
- `mcp1.log` - Additional MCP log (likely rotated)

### 2. Desktop Commander Legacy Logs (Secondary)
**Location**: `~/.claude-server-commander/claude_tool_call.log`
**Format**: Pipe-delimited with JSON-like structure  
**Content**: Desktop Commander tool calls and results (legacy format)

#### Sample DC Log Entry
```
2025-05-26T15:42:33.921Z|read_file|{"path":"/home/alex/project/README.md"}|success
```

## MCP Log Analysis

### Log Format (MCP Server Logs)
```
2025-05-26T08:20:05.428Z [INFO] Server starting: mcp-server-cafedelic
2025-05-26T08:20:05.430Z [DEBUG] Tool registered: get_active_context
2025-05-26T08:20:42.682Z [DEBUG] Tool execution: read_file with args: {"path": "/home/alex/code/project/README.md"}
2025-05-26T08:20:42.685Z [INFO] Tool completed: read_file (success)
```

### Common Log Patterns

#### Server Initialization
```
[INFO] Starting [server-name] MCP server...
[INFO] [server-name] MCP server started successfully  
[DEBUG] Registered tools: [tool1, tool2, tool3]
```

#### Tool Execution Lifecycle
```
[DEBUG] Received request: method=tools/call, params={"name":"read_file","arguments":{"path":"..."}}
[DEBUG] Tool execution: read_file with args: {...}
[INFO] Tool completed: read_file (success)
[ERROR] Tool execution failed: read_file - Error message
```

## Monitoring Implementation

### Claude Desktop MCP Log Watcher
```typescript
import { EventEmitter } from 'events';
import * as fs from 'fs';

class ClaudeDesktopMCPWatcher extends EventEmitter {
  private logDir = '/home/alex/.config/Claude/logs';
  
  async discoverLogs() {
    const files = await fs.promises.readdir(this.logDir);
    return {
      mcpServerLogs: files.filter(f => f.startsWith('mcp-server-')),
      generalMcpLogs: files.filter(f => f.match(/^mcp\d*\.log$/)),
      targetLogs: ['mcp-server-desktop-commander.log', 'mcp.log']
    };
  }
  
  watchLog(logPath: string) {
    const watcher = fs.watchFile(logPath, (curr, prev) => {
      if (curr.mtime > prev.mtime) {
        this.processNewLogEntries(logPath, prev.size, curr.size);
      }
    });
  }
}
```

### Activity Correlation
The MCP logs provide richer context than legacy DC logs:
1. **MCP Protocol Layer** → Tool requests and responses with full context
2. **Server Execution Layer** → Individual tool execution with timing
3. **Application Layer** → High-level activity patterns and workflows

## Integration with Cafedelic Intelligence Framework

### Primary Targets for Monitoring
1. **mcp-server-desktop-commander.log** - File operations, system commands
2. **mcp.log** - General MCP protocol activity and coordination  
3. **mcp-server-cafedelic.log** - Our own server's activity and performance

### Activity Translation
```typescript
// Translate MCP logs to human-readable insights
const patterns = {
  'read_file': (args) => `Claude is reading ${path.basename(args.path)}`,
  'write_file': (args) => `Claude is updating ${path.basename(args.path)}`, 
  'list_directory': (args) => `Claude is exploring ${args.path}`,
  'execute_command': (args) => `Claude executed: ${args.command}`
};
```

### Benefits Over Legacy DC Logs
- **Structured JSON Arguments** - No parsing of pipe-delimited text
- **Rich Context** - Full MCP protocol context available
- **Multiple Servers** - Monitor all MCP integrations simultaneously  
- **Better Timing** - Request/response correlation with precise timing
- **Error Details** - Detailed error context and stack traces

## Configuration for Cafedelic

### Updated Configuration
```typescript
export const config = {
  desktopMCP: {
    enabled: true,
    logBaseDir: '/home/alex/.config/Claude/logs',  // CORRECTED PATH
    targetLogs: [
      'mcp-server-desktop-commander.log',  // Primary target
      'mcp.log'                           // General activity
    ],
    watchInterval: 500,
    discoveryInterval: 30000,
    fallbackToDC: true  // Use legacy DC logs as backup
  }
};
```

## Limitations and Considerations

### What's Available
- Complete MCP tool execution lifecycle
- Structured arguments and return values
- Server health and performance metrics
- Protocol-level error details
- Multi-server activity coordination

### Performance Notes  
- Log files accumulate based on usage patterns
- Multiple active MCP servers generate concurrent logs
- File watching should use efficient polling intervals
- Consider log rotation for long-running sessions

## Migration from VS Code Logs

**Previous Incorrect Configuration:**
```typescript
// WRONG - This was based on incorrect documentation
logBaseDir: '~/.config/Code/logs'
```

**Correct Configuration:**
```typescript  
// CORRECT - Actual Claude Desktop MCP log location
logBaseDir: '/home/alex/.config/Claude/logs'
```

This correction addresses the root cause of non-functional MCP log discovery in the current implementation.
