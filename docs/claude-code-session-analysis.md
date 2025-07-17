# Claude Code CLI Session Discovery & Management Analysis

**Analysis Date**: July 16, 2025  
**Claude Code Version**: 1.0.53  
**Repository**: https://github.com/anthropics/claude-code  

## Executive Summary

This analysis reveals Claude Code's hybrid session management architecture combining filesystem-based storage with SQLite database indexing. Sessions are stored as JSONL files organized by project directories, with fast metadata access through a local database. This discovery informs Cafedelic's integration strategy for real-time Claude Code session monitoring and coordination.

## 1. Session Discovery Mechanism

### Primary Data Storage Locations

```
~/.claude/                           # Main configuration directory
├── __store.db                       # SQLite database for fast metadata access
├── .credentials.json                # OAuth tokens and API keys
├── settings.json                    # User-level settings
├── settings.local.json              # Local setting overrides
└── projects/                        # Session storage organized by project
    ├── -home-alex-code-cafedelic/   # Normalized project path
    │   ├── 2d4e90bd-88d3-41fc-aec7-adc5d7089a7a.jsonl
    │   ├── 31101eea-fc5a-4d30-a078-9f4ac2ed7503.jsonl
    │   └── [other sessions...]
    └── [other projects...]

.claude/                             # Project-specific configuration
├── settings.json                    # Project settings
└── settings.local.json              # Project local overrides
```

### Discovery Process

Claude Code uses a **hybrid discovery approach**:

1. **Directory-based discovery**: Scans `~/.claude/projects/` for session files
2. **SQLite database indexing**: Uses `~/.claude/__store.db` for metadata queries
3. **Path normalization**: Converts project paths to directory names
4. **Real-time access**: Direct JSONL file reading for conversation history

### Path Normalization Algorithm

```typescript
// Project path transformation
function normalizePath(projectPath: string): string {
  // /home/alex/code/cafedelic → -home-alex-code-cafedelic
  return projectPath.replace(/\//g, '-');
}

function denormalizePath(dirName: string): string {
  // -home-alex-code-cafedelic → /home/alex/code/cafedelic
  return dirName.replace(/^-/, '').replace(/-/g, '/');
}
```

## 2. Session Data Storage

### JSONL Format Structure

Each session is stored as a **JSONL file** (JSON Lines):
- **One JSON object per line** representing messages or events
- **UUID-based filenames** for unique session identification  
- **Append-only format** for conversation history preservation

### Sample Session File Structure

```jsonl
{"parentUuid":"f47ac10b-58cc-4372-a567-0e02b2c3d479","isSidechain":false,"userType":"external","cwd":"/home/alex/code/cafedelic","sessionId":"2d4e90bd-88d3-41fc-aec7-adc5d7089a7a","version":"1.0.53","type":"user","message":{"text":"Help me understand the codebase structure"},"uuid":"550e8400-e29b-41d4-a716-446655440000","timestamp":"2025-07-16T22:54:54.453Z"}
{"parentUuid":"550e8400-e29b-41d4-a716-446655440000","isSidechain":false,"userType":"external","cwd":"/home/alex/code/cafedelic","sessionId":"2d4e90bd-88d3-41fc-aec7-adc5d7089a7a","version":"1.0.53","type":"assistant","message":{"text":"I'll analyze your codebase structure..."},"uuid":"6ba7b810-9dad-11d1-80b4-00c04fd430c8","timestamp":"2025-07-16T22:55:12.789Z"}
```

### Message Schema

```typescript
interface ClaudeCodeMessage {
  parentUuid: string;              // Links to previous message for threading
  isSidechain: boolean;            // Indicates branched conversations
  userType: "external" | string;   // User type classification
  cwd: string;                     // Current working directory (project path)
  sessionId: string;               // UUID for session identification
  version: string;                 // Claude Code version
  type: "user" | "assistant";      // Message originator
  message: {                       // Message content
    text?: string;
    tool_calls?: ToolCall[];
    tool_results?: ToolResult[];
  };
  uuid: string;                    // Unique message identifier
  timestamp: string;               // ISO 8601 timestamp
}
```

## 3. SQLite Database Schema

### Database Location
`~/.claude/__store.db` - Local SQLite database for fast metadata access

### Key Tables

```sql
-- Core message metadata
CREATE TABLE base_messages (
  uuid TEXT PRIMARY KEY,
  session_id TEXT,
  parent_uuid TEXT,
  timestamp TEXT,
  type TEXT,
  cwd TEXT
);

-- Assistant response details
CREATE TABLE assistant_messages (
  uuid TEXT PRIMARY KEY,
  cost REAL,
  duration INTEGER,
  model TEXT,
  content TEXT
);

-- User input and tool results
CREATE TABLE user_messages (
  uuid TEXT PRIMARY KEY,
  content TEXT,
  tool_results TEXT
);

-- Session summaries for quick reference
CREATE TABLE conversation_summaries (
  session_id TEXT PRIMARY KEY,
  summary TEXT,
  last_updated TEXT
);
```

## 4. Session Management Architecture

### Core Discovery Pattern

```typescript
class ClaudeCodeSessionDiscovery {
  private claudeDir: string;
  private projectsDir: string;
  private dbPath: string;

  constructor() {
    this.claudeDir = path.join(os.homedir(), '.claude');
    this.projectsDir = path.join(this.claudeDir, 'projects');
    this.dbPath = path.join(this.claudeDir, '__store.db');
  }

  async discoverSessions(): Promise<SessionInfo[]> {
    const sessions: SessionInfo[] = [];
    const projectDirs = await fs.readdir(this.projectsDir);
    
    for (const projectDir of projectDirs) {
      const projectPath = this.denormalizePath(projectDir);
      const sessionFiles = await this.getSessionFiles(projectDir);
      
      for (const sessionFile of sessionFiles) {
        const sessionId = path.basename(sessionFile, '.jsonl');
        const metadata = await this.getSessionMetadata(sessionId);
        
        sessions.push({
          id: sessionId,
          projectPath,
          file: path.join(this.projectsDir, projectDir, sessionFile),
          lastActivity: metadata.lastActivity,
          messageCount: metadata.messageCount
        });
      }
    }
    
    return sessions;
  }

  private async getSessionFiles(projectDir: string): Promise<string[]> {
    const projectPath = path.join(this.projectsDir, projectDir);
    const files = await fs.readdir(projectPath);
    return files.filter(file => file.endsWith('.jsonl'));
  }

  private denormalizePath(dirName: string): string {
    return dirName.replace(/^-/, '').replace(/-/g, '/');
  }
}
```

### Session Lifecycle Operations

1. **Session Creation**
   - Generate UUID for session ID
   - Create JSONL file in appropriate project directory
   - Initialize with session metadata

2. **Message Appending**
   - Append new JSON line to existing session file
   - Update SQLite database with message metadata
   - Maintain parent-child message relationships

3. **Session Querying**
   - Fast metadata queries via SQLite database
   - Full conversation history from JSONL files
   - Cross-session analysis capabilities

## 5. Integration Opportunities for Cafedelic

### Real-Time Monitoring Strategy

```typescript
// Filesystem watcher for session activity
class ClaudeCodeWatcher {
  private watcher: FSWatcher;
  private projectsDir: string;

  startWatching(callback: (event: SessionEvent) => void) {
    this.watcher = chokidar.watch(this.projectsDir, {
      ignored: /^\./, // Ignore hidden files
      persistent: true,
      ignoreInitial: false
    });

    this.watcher
      .on('add', (path) => this.handleSessionFileChange('add', path, callback))
      .on('change', (path) => this.handleSessionFileChange('change', path, callback))
      .on('unlink', (path) => this.handleSessionFileChange('delete', path, callback));
  }

  private handleSessionFileChange(
    eventType: string, 
    filePath: string, 
    callback: (event: SessionEvent) => void
  ) {
    if (filePath.endsWith('.jsonl')) {
      const sessionId = path.basename(filePath, '.jsonl');
      const projectPath = this.extractProjectPath(filePath);
      
      callback({
        type: eventType,
        sessionId,
        projectPath,
        timestamp: new Date().toISOString()
      });
    }
  }
}
```

### Database Integration Pattern

```typescript
// Cafedelic database integration with Claude Code data
class CafedelicClaudeCodeIntegration {
  async populateFromClaudeCode() {
    const sessions = await this.discoverClaudeCodeSessions();
    
    for (const session of sessions) {
      // Insert/update project in Cafedelic database
      await this.upsertProject({
        path: session.projectPath,
        name: path.basename(session.projectPath),
        status: 'active'
      });

      // Insert/update session in Cafedelic database
      await this.upsertSession({
        id: session.id,
        project_path: session.projectPath,
        assistant_type: 'claude-code',
        last_activity: session.lastActivity,
        message_count: session.messageCount
      });

      // Parse and store activities from JSONL
      const activities = await this.parseSessionActivities(session.file);
      await this.bulkInsertActivities(activities);
    }
  }

  private async parseSessionActivities(sessionFile: string): Promise<Activity[]> {
    const activities: Activity[] = [];
    const lines = await this.readJSONLFile(sessionFile);
    
    for (const line of lines) {
      const message = JSON.parse(line);
      
      activities.push({
        session_id: message.sessionId,
        type: this.classifyMessageType(message),
        content: message.message.text || '',
        timestamp: message.timestamp,
        metadata: JSON.stringify({
          uuid: message.uuid,
          parentUuid: message.parentUuid,
          messageType: message.type
        })
      });
    }
    
    return activities;
  }
}
```

## 6. Configuration Management

### Settings Hierarchy

Claude Code follows a **hierarchical configuration system**:

1. **User-level**: `~/.claude/settings.json`
2. **User-local**: `~/.claude/settings.local.json` (overrides user-level)
3. **Project-level**: `.claude/settings.json` (project-specific)
4. **Project-local**: `.claude/settings.local.json` (overrides project-level)

### Sample Configuration Structure

```json
{
  "model": "claude-3-5-sonnet-20241022",
  "editor": "vscode",
  "git": {
    "autoCommit": false,
    "pushOnCommit": false
  },
  "ui": {
    "theme": "dark",
    "lineNumbers": true
  },
  "experimental": {
    "features": []
  }
}
```

## 7. Security & Credentials

### Credential Storage
- **Location**: `~/.claude/.credentials.json`
- **Format**: Encrypted JSON with OAuth tokens
- **Platform Integration**: Keychain services on macOS, similar on other platforms

### Access Patterns
- Sessions only access files within their project directory
- Credentials isolated from session data
- No cross-project data leakage

## 8. Recommendations for Cafedelic Implementation

### Phase 1: Foundation
1. **Direct File System Integration**: Read from `~/.claude/projects/` directly
2. **SQLite Database Connection**: Query `__store.db` for fast metadata access
3. **Path Normalization**: Implement project path conversion utilities
4. **JSONL Parser**: Build robust parser for Claude Code session files

### Phase 2: Real-Time Monitoring
1. **Filesystem Watcher**: Monitor `~/.claude/projects/` for changes
2. **Incremental Processing**: Process only new messages in existing sessions
3. **Database Synchronization**: Keep Cafedelic database in sync with Claude Code data
4. **Event Broadcasting**: Notify Cafedelic components of session changes

### Phase 3: Advanced Integration
1. **Session Health Monitoring**: Analyze conversation patterns for stuck sessions
2. **Cross-Session Intelligence**: Identify related work across sessions
3. **Context Preservation**: Extract and preserve session context for handoffs
4. **Automated Coordination**: Suggest session coordination opportunities

## 9. Key Technical Insights

### Performance Characteristics
- **Session Discovery**: O(n) filesystem scan, cached in SQLite
- **Message Access**: Direct file reading, no API overhead
- **Real-time Updates**: Filesystem events provide immediate notification
- **Metadata Queries**: Fast SQLite queries for session filtering

### Data Consistency
- **Append-Only Design**: JSONL files never modified, only appended
- **Atomic Operations**: Message writes are atomic at filesystem level
- **Eventual Consistency**: SQLite metadata eventually consistent with JSONL files

### Scalability Considerations
- **File Size Growth**: JSONL files grow linearly with conversation length
- **Directory Structure**: Flat directory structure within each project
- **Database Size**: SQLite database remains relatively small (metadata only)

## Conclusion

Claude Code's session management architecture provides multiple integration points for Cafedelic:

1. **Filesystem-based discovery** enables real-time session monitoring
2. **JSONL format** allows detailed conversation analysis  
3. **SQLite metadata** provides fast session queries
4. **Project organization** aligns with Cafedelic's project-centric approach

This analysis provides the foundation for implementing native Claude Code integration in Cafedelic, enabling the intelligence layer to work with real session data rather than mock data, ultimately achieving the vision of an async task delegation platform for AI-assisted development.