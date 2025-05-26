# Claude Code Log System Documentation

## Overview

Claude Code maintains comprehensive logs of all session activity in JSON Lines format, providing rich data for activity monitoring, file operation tracking, and development intelligence gathering.

## Primary Log Locations

### 1. Session Logs (Primary Data Source)
**Location**: `~/.claude/projects/[project-path]/[session-uuid].jsonl`

**Format**: JSON Lines (one JSON object per line)

**Content**: Complete conversation history with metadata

#### Session Log Structure
```jsonl
{"type":"summary","summary":"Session Summary","leafUuid":"..."}
{"type":"user","message":{"role":"user","content":"command"},"uuid":"...","timestamp":"..."}
{"type":"assistant","message":{"role":"assistant","content":[...],"cost_usd":0.123},"uuid":"..."}
```

#### Key Fields
- **type**: `"user"` | `"assistant"` | `"summary"`
- **message**: Complete message object with role and content
- **uuid**: Unique message identifier
- **timestamp**: ISO 8601 timestamp
- **sessionId**: Session identifier
- **cwd**: Working directory
- **costUSD**: Token cost tracking (assistant messages)
- **parentUuid**: Message threading information

### 2. MCP Protocol Error Logs
**Location**: `~/.cache/claude-cli-nodejs/[project-path]/mcp-logs-[server-name]/`

**Format**: Timestamped JSON files (e.g., `2025-05-26T09-10-50-935Z.txt`)

**Content**: MCP protocol errors and debugging information

#### MCP Error Log Structure
```json
[{
  "error": "SyntaxError: Unexpected token...",
  "timestamp": "2025-05-26T09:31:00.602Z",
  "sessionId": "0d5cc8b6-df80-4c95-ad60-143d8fb05309",
  "cwd": "/home/alex/code/cafedelic"
}]
```

## File Operation Detection

### Direct Tool Usage (High Confidence)
Claude Code tool usage appears in assistant messages as `tool_use` blocks:

```json
{
  "type": "tool_use",
  "id": "toolu_01...",
  "name": "Read",
  "input": {"file_path": "/home/alex/project/README.md"}
}
```

**Detectable Tools**:
- `Read` - File reading operations
- `Write` - File creation/modification
- `Edit` - File editing operations
- `LS` - Directory listing
- `Grep` - File searching
- `Move` - File moving/renaming

### Shell Command Analysis (Medium Confidence)
Shell commands appear in user messages with special tags:

#### Input Commands
```json
{
  "content": "<bash-input>cat README.md</bash-input>"
}
```

#### Command Output
```json
{
  "content": "<bash-stdout># File contents here...</bash-stdout>"
}
```

**Detectable Operations**:
- **Read**: `cat`, `less`, `head`, `tail`, `grep`
- **Write**: `echo >`, `tee`, `>`, `>>`
- **Create**: `touch`, `mkdir`, `cp`
- **Modify**: `sed`, `awk`, `vim`, `nano`
- **Delete**: `rm`, `rmdir`
- **Move**: `mv`, `cp`

### File Path Extraction Patterns

#### From Tool Usage
```javascript
function extractFilePathsFromTools(message) {
  const paths = [];
  if (message.content && Array.isArray(message.content)) {
    for (const item of message.content) {
      if (item.type === 'tool_use' && item.input) {
        if (item.input.file_path) paths.push(item.input.file_path);
        if (item.input.source) paths.push(item.input.source);
        if (item.input.destination) paths.push(item.input.destination);
      }
    }
  }
  return paths;
}
```

#### From Shell Commands
```javascript
function extractFilePathsFromBash(content) {
  const paths = [];
  const bashInput = content.match(/<bash-input>(.*?)<\/bash-input>/g);
  if (bashInput) {
    for (const cmd of bashInput) {
      const command = cmd.replace(/<\/?bash-input>/g, '');
      // Extract file paths from common commands
      const catMatch = command.match(/cat\s+([^\s]+)/);
      if (catMatch) paths.push(catMatch[1]);
      
      const viMatch = command.match(/(?:vim|nano|emacs)\s+([^\s]+)/);
      if (viMatch) paths.push(viMatch[1]);
      
      // Add more command patterns as needed
    }
  }
  return paths;
}
```

## Monitoring Implementation

### File Watcher Setup
```javascript
import chokidar from 'chokidar';

class ClaudeCodeLogWatcher {
  constructor(projectPath) {
    this.projectPath = projectPath;
    this.sessionDir = `${os.homedir()}/.claude/projects/${projectPath.replace(/\//g, '-')}`;
  }
  
  watch() {
    const watcher = chokidar.watch(`${this.sessionDir}/*.jsonl`);
    watcher.on('change', (path) => this.handleLogUpdate(path));
    watcher.on('add', (path) => this.handleNewSession(path));
  }
  
  async handleLogUpdate(filePath) {
    const content = await readFile(filePath, 'utf8');
    const lines = content.trim().split('\n');
    const lastEntry = JSON.parse(lines[lines.length - 1]);
    
    if (lastEntry.type === 'assistant') {
      const filePaths = this.extractFileOperations(lastEntry);
      if (filePaths.length > 0) {
        this.emit('file-operations', { filePaths, session: filePath });
      }
    }
  }
}
```

## Integration with Cafedelic

### Database Schema for Claude Code Activity
```sql
ALTER TABLE file_accesses ADD COLUMN claude_code_session TEXT;
ALTER TABLE activities ADD COLUMN cost_usd DECIMAL(10,6);

CREATE TABLE claude_code_sessions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_uuid TEXT UNIQUE,
  project_path TEXT,
  start_time DATETIME,
  last_activity DATETIME,
  total_cost DECIMAL(10,4),
  message_count INTEGER,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

## Limitations and Future Enhancements

### What We Can't Detect
- File operations that don't generate visible commands or tool usage
- Internal Claude Code file system operations
- Operations in detached processes

### Performance Considerations
- JSONL files can become large (100MB+)
- Parse incrementally, don't load entire files
- Cache parsed sessions to avoid re-processing

### Future Features
1. **Real-time file change correlation** - Match Claude Code operations with filesystem events
2. **Cost tracking integration** - Monitor token usage per project
3. **Session analytics** - Productivity metrics and insights
4. **Cross-session file tracking** - Understand file relationships across sessions
