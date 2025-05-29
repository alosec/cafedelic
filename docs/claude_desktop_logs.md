# Claude Desktop Log System Documentation

## Overview

Claude Desktop generates multiple types of logs for MCP servers, VS Code extensions, and general application activity.

## Primary Log Locations

### 1. MCP Server Logs
**Location**: `~/.config/Code/logs/[date]/window[N]/mcpServer.claude-desktop.null.[server-name].log`
**Format**: Plain text with timestamps
**Content**: MCP server stdout/stderr output and communication logs

### 2. VS Code Extension Logs
**Location**: `~/.config/Code/logs/[date]/window[N]/exthost/Anthropic.claude-code/`

### 3. Application Logs
**Location**: `~/.config/Claude/logs/`

### 4. User Configuration
**Location**: `~/.config/Claude/`

## MCP Server Log Analysis

### Log Format
```
2025-05-26T08:20:05.428Z [INFO] Server starting
2025-05-26T08:20:05.430Z [DEBUG] Tool registered: get_active_context
2025-05-26T08:20:42.682Z [ERROR] Tool execution failed: SyntaxError
```

### Common Log Patterns

#### Server Initialization
```
[INFO] Starting [server-name] MCP server...
[INFO] [server-name] MCP server started successfully
[DEBUG] Registered tools: [tool1, tool2, tool3]
```

#### Tool Execution
```
[DEBUG] Executing tool: tool_name with args: {...}
[INFO] Tool execution completed: tool_name
[ERROR] Tool execution failed: tool_name - Error message
```

## Desktop Commander Integration

### DC Log Location
**Primary**: `~/.claude-server-commander/claude_tool_call.log`
**Format**: Pipe-delimited with JSON-like structure
**Content**: All Desktop Commander tool calls and results

#### Sample DC Log Entry
```
2025-05-26T15:42:33.921Z|read_file|{"path":"/home/alex/project/README.md"}|success
```

### Cross-System Correlation
Desktop Commander logs can be correlated with Claude Desktop MCP logs to understand the full activity flow:

1. **Claude Desktop** → Uses MCP tool
2. **MCP Server** → Calls Desktop Commander
3. **Desktop Commander** → Logs tool execution
4. **Results** → Flow back through MCP to Claude Desktop

## Monitoring Implementation

### Log Watcher for MCP Servers
```javascript
import chokidar from 'chokidar';

class ClaudeDesktopLogWatcher {
  constructor() {
    this.logDir = `${os.homedir()}/.config/Code/logs`;
  }
  
  watch() {
    // Watch for new log directories (new sessions)
    const watcher = chokidar.watch(`${this.logDir}/*/window*/mcpServer.*.log`);
    watcher.on('change', (path) => this.handleMCPLogUpdate(path));
  }
  
  async handleMCPLogUpdate(filePath) {
    const content = await readFile(filePath, 'utf8');
    const lines = content.trim().split('\n');
    const lastLine = lines[lines.length - 1];
    
    if (lastLine.includes('[ERROR]')) {
      this.emit('mcp-error', { server: this.extractServerName(filePath), error: lastLine });
    }
  }
}
```

## Integration with Cafedelic

### Combined Activity Stream
```javascript
// Combine Claude Desktop MCP logs with Desktop Commander logs
class UnifiedActivityWatcher {
  constructor() {
    this.desktopWatcher = new ClaudeDesktopLogWatcher();
    this.dcWatcher = new DesktopCommanderWatcher();
  }
  
  start() {
    this.desktopWatcher.on('mcp-tool-call', (event) => {
      this.correlateActivity('claude-desktop', event);
    });
    
    this.dcWatcher.on('tool-execution', (event) => {
      this.correlateActivity('desktop-commander', event);
    });
  }
}
```

## Limitations and Considerations

### What's Available
- MCP server communication logs (stdout/stderr)
- Tool execution timing and success/failure
- Server startup and shutdown events
- Protocol-level errors

### What's Missing
- Claude Desktop's internal reasoning process
- User interaction details
- Token usage and cost tracking
- Session persistence across restarts

### Performance Notes
- Log files can accumulate quickly
- Daily rotation helps manage disk space
- MCP error logs are separate from activity logs

## Future Enhancements

### Planned Integrations
1. **Unified Activity Dashboard** - Combine all log sources into single view
2. **Real-time Error Monitoring** - Alert on MCP server failures
3. **Performance Analytics** - Track tool execution times and patterns
4. **Cost Correlation** - Link Desktop Commander usage to Claude Desktop activity
