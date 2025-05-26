# Issue #8: Claude Desktop MCP Log Integration Refactor

## Priority: High
## Status: Open  
## Created: 2025-05-26
## Assignee: TBD

## Problem Statement

The current implementation attempts to parse Desktop Commander (DC) logs which have a complex, non-standard format:
```
2025-05-26T22:58:28.906Z | read_file           \t| Arguments: {"length":3,"path":"/home/alex/code/cafedelic/package.json"}
```

This approach is **problematic** because:
1. **Complex Parsing**: Requires custom parsing logic for pipe-delimited, tab-separated format
2. **Error-Prone**: Tab characters and mixed separators make parsing fragile  
3. **Indirect Data**: DC logs are a transformation of the original structured data
4. **Maintenance Burden**: Custom parsing logic needs ongoing maintenance

## Preferred Solution

**Use Claude Desktop MCP logs directly** which have clean, standard formatting:

### Log Location
`~/.config/Code/logs/[date]/window[N]/mcpServer.claude-desktop.null.[server-name].log`

### Log Format  
```
2025-05-26T08:20:05.428Z [INFO] Server starting
2025-05-26T08:20:42.682Z [DEBUG] Tool execution: read_file with args: {"path": "/home/alex/project/README.md"}
2025-05-26T08:20:42.683Z [INFO] Tool completed: read_file (success)
```

### Benefits
- **Standard Format**: Consistent timestamp + level + message structure
- **Native JSON**: Arguments already properly formatted as JSON
- **Reliable**: Generated directly by Claude Desktop's MCP system
- **Rich Context**: Includes execution timing, success/failure status
- **Future-Proof**: Stable format maintained by Anthropic

## Technical Requirements

### Core Changes Needed

1. **Replace WatcherService**
   - Remove complex DC log parsing logic  
   - Implement Claude Desktop MCP log monitoring
   - Watch multiple log files (new sessions create new logs)

2. **Update Log Discovery**
   - Find active MCP log files in `~/.config/Code/logs/*/window*/`
   - Handle log rotation (daily directories)
   - Support multiple concurrent Claude Desktop windows

3. **Implement Standard Parsing**
   - Parse timestamp + level + message format
   - Extract tool calls from log messages
   - Handle multi-line JSON arguments cleanly

4. **Enhanced Activity Translation**
   - Leverage richer context from MCP logs
   - Include execution timing information
   - Distinguish between tool start, progress, completion, and errors

### Implementation Plan

#### Phase 1: MCP Log Discovery
```typescript
class ClaudeDesktopLogWatcher {
  private logBaseDir = `${os.homedir()}/.config/Code/logs`;
  
  async findActiveLogs(): Promise<string[]> {
    // Find all active MCP server logs
    const pattern = `${this.logBaseDir}/*/window*/mcpServer.claude-desktop.null.*.log`;
    return glob(pattern);
  }
  
  watchForNewLogs(): void {
    // Monitor for new session directories and log files
    chokidar.watch(`${this.logBaseDir}/**/mcpServer.*.log`)
      .on('add', (path) => this.startWatchingLog(path));
  }
}
```

#### Phase 2: Standard Log Parsing
```typescript
interface MCPLogEntry {
  timestamp: string;
  level: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';
  message: string;
  toolCall?: {
    name: string;
    args: any;
    status?: 'started' | 'completed' | 'failed';
  };
}

parseLogLine(line: string): MCPLogEntry | null {
  // Parse: "2025-05-26T08:20:42.682Z [DEBUG] Tool execution: read_file with args: {...}"
  const match = line.match(/^(\d{4}-\d{2}-\d{2}T[\d:\.]+Z) \[(\w+)\] (.+)$/);
  if (!match) return null;
  
  const [, timestamp, level, message] = match;
  
  // Extract tool call details from message if present
  const toolCall = this.extractToolCall(message);
  
  return { timestamp, level, message, toolCall };
}
```

#### Phase 3: Integration with Existing System
- **Keep TranslatorService**: Adapt to work with MCP log entries
- **Update ActivityStore**: Handle richer MCP log data
- **Enhance EmacsService**: Use reliable tool call detection for auto-file-opening

### File Changes Required

#### New Files
- `src/services/claude-desktop-log-watcher.service.ts` - MCP log monitoring
- `src/types/mcp-log.types.ts` - Type definitions for MCP logs
- `src/utils/log-discovery.ts` - Find active Claude Desktop log files

#### Modified Files  
- `src/services/watcher.service.ts` - Replace with MCP log watcher or remove
- `src/services/translator.service.ts` - Adapt for MCP log format
- `index.ts` - Wire up new MCP log watcher
- `src/config/cafedelic.config.ts` - Add MCP log configuration

#### Removed Files
- Complex DC log parsing logic (can be safely removed)

## Testing Strategy

### Verification Steps
1. **Log Discovery**: Verify active MCP logs are found correctly
2. **Real-time Monitoring**: Confirm new tool calls are detected immediately  
3. **Parsing Accuracy**: Test with various tool calls and argument types
4. **Error Handling**: Verify graceful handling of malformed log entries
5. **Multi-Window Support**: Test with multiple Claude Desktop windows open

### Test Scenarios
- Single Claude Desktop window with active MCP servers
- Multiple windows with different MCP server configurations  
- Log rotation (crossing midnight boundary)
- Large JSON arguments in tool calls
- Error conditions and malformed log entries

## Success Criteria

### Functional Requirements
- [x] **Real-time Detection**: New tool calls detected within 500ms
- [x] **Accurate Parsing**: 100% success rate on well-formed MCP log entries
- [x] **Multi-Session Support**: Handle multiple Claude Desktop windows simultaneously
- [x] **Error Resilience**: Graceful handling of parsing errors without system failure

### Quality Requirements  
- [x] **Simplified Codebase**: Remove complex DC parsing logic
- [x] **Maintainable**: Standard parsing approach using established libraries
- [x] **Reliable**: No parsing errors on standard MCP log format
- [x] **Performance**: Minimal CPU usage during log monitoring

## Context for Implementation

### Current System Architecture
The existing Cafedelic system has these working components:
- **ActivityStore**: Stores and retrieves activity history  
- **TranslatorService**: Converts log entries to human-readable format
- **EmacsService**: Auto-opens files when Claude accesses them (half-functional)
- **MCP Tools**: `get_active_context`, `get_emacs_status`, etc.

### Integration Points
The new MCP log watcher should integrate at the **WatcherService** level:
```
MCP Logs → MCPLogWatcher → TranslatorService → ActivityStore → MCP Tools
                ↓
            EmacsService (auto-file-opening)
```

### Configuration
Existing config structure in `src/config/cafedelic.config.ts` should be extended:
```typescript
export interface CafedelicConfig {
  // ... existing config
  mcpLogs: {
    enabled: boolean;
    logBaseDir: string;
    watchInterval: number;
    maxLogAge: number; // days
  };
}
```

## Expected Outcomes

After successful implementation:
1. **Robust Parsing**: No more parsing errors from complex DC log format
2. **Real-time Activity**: Immediate detection of Claude Desktop tool usage  
3. **Better Context**: Richer information about tool execution (timing, status)
4. **Maintainable Code**: Standard log parsing using proven libraries
5. **Future-Ready**: Compatible with Claude Desktop's native log format

## Notes

- **Backwards Compatibility**: Consider keeping DC log support as fallback
- **Performance**: MCP logs may have higher volume than DC logs
- **Privacy**: MCP logs may contain more sensitive information - handle appropriately
