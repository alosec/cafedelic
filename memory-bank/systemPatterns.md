# System Patterns

## Core Architecture: Task Delegation Platform

Cafedelic is built on a three-layer task delegation platform that prioritizes async Claude Code orchestration and contextual session management over traditional terminal manipulation.

```
┌─────────────────────────────────────┐
│     Quick-Chat Delegation           │ ← Task assignment interface
│   + find_relevant_chats()           │
├─────────────────────────────────────┤
│        SQLite Database              │ ← Single source of truth
│   (Task State Management)           │
├─────────────────────────────────────┤
│    Claude Code Session Pool         │ ← claude --resume integration
│   + ~/.claude/commands/             │
└─────────────────────────────────────┘
```

## The Database-First Pattern

### Single Source of Truth
All intelligence flows through SQLite rather than files or memory:

```sql
-- Core entities
CREATE TABLE projects (id, name, repo_path, description, status, created_at);
CREATE TABLE sessions (id, project_id, name, assistant_type, branch, status);
CREATE TABLE activities (id, session_id, type, content, timestamp, files_affected);
CREATE TABLE context (id, session_id, open_files, current_task, summary);
```

### Intelligence Through Relationships
```sql
-- Cross-session insights
SELECT p.name as project, COUNT(s.id) as active_sessions
FROM projects p
JOIN sessions s ON p.id = s.project_id
WHERE s.status = 'active'
GROUP BY p.id;

-- Activity patterns
SELECT session_name, COUNT(*) as file_operations
FROM activities a
JOIN sessions s ON a.session_id = s.id
WHERE a.type = 'file_read' AND a.timestamp > datetime('now', '-1 hour')
GROUP BY s.id;
```

## MCP-Native Interface Pattern

### Conversational Operations
All functionality exposed through natural language MCP tools:

```typescript
// Project management
interface ProjectTools {
  create_project(name: string, repo_path: string, description?: string)
  read_project_status(project_id: string)
  update_project_metadata(project_id: string, updates: ProjectUpdate)
  list_active_projects()
  archive_project(project_id: string)
}

// Session orchestration  
interface SessionTools {
  create_session(project_id: string, options: SessionOptions)
  read_session_context(session_id: string, include_summary?: boolean)
  update_session_status(session_id: string, status: SessionStatus)
  list_project_sessions(project_id: string)
  terminate_session(session_id: string)
}

// Context intelligence
interface ContextTools {
  analyze_current_activity(session_id: string)
  generate_task_summary(session_id: string) // Uses claude -p
  track_file_access(session_id: string, file_path: string)
  update_session_context(session_id: string, context: ContextUpdate)
}
```

### Tool Safety Pattern
All MCP tools follow CRUD principles with built-in safety:

```typescript
// Safe creation - validates before inserting
async function create_project(name: string, repo_path: string) {
  // Validate git repository exists
  if (!fs.existsSync(path.join(repo_path, '.git'))) {
    throw new Error('Not a git repository');
  }
  
  // Check for existing project
  const existing = await db.get('SELECT id FROM projects WHERE repo_path = ?', repo_path);
  if (existing) {
    throw new Error('Project already exists');
  }
  
  // Safe insertion
  const result = await db.run(
    'INSERT INTO projects (name, repo_path, status, created_at) VALUES (?, ?, ?, ?)',
    [name, repo_path, 'active', new Date().toISOString()]
  );
  
  return { project_id: result.lastID, name, repo_path };
}

// Safe reading - always returns valid data structure
async function read_project_status(project_id: string) {
  const project = await db.get('SELECT * FROM projects WHERE id = ?', project_id);
  if (!project) {
    return { error: 'Project not found', project_id };
  }
  
  const sessions = await db.all('SELECT * FROM sessions WHERE project_id = ?', project_id);
  const recent_activity = await db.all(
    'SELECT COUNT(*) as count FROM activities WHERE session_id IN (SELECT id FROM sessions WHERE project_id = ?) AND timestamp > datetime("now", "-1 hour")',
    project_id
  );
  
  return {
    project,
    active_sessions: sessions.filter(s => s.status === 'active').length,
    total_sessions: sessions.length,
    recent_activity: recent_activity[0].count
  };
}
```

## Display Adapter Pattern

### Plugin Architecture
Display adapters connect the intelligence database to various frontends:

```typescript
interface DisplayAdapter {
  name: string;
  supports: string[]; // ['terminal', 'gui', 'web']
  initialize(database: Database): Promise<void>;
  displayProjectStatus(project_id: string): Promise<void>;
  showSessionList(project_id?: string): Promise<void>;
  updateActivityFeed(activities: Activity[]): Promise<void>;
}

// Terminal adapter for tmux integration
class TerminalAdapter implements DisplayAdapter {
  name = 'terminal';
  supports = ['tmux', 'terminal-multiplexer'];
  
  async displayProjectStatus(project_id: string) {
    const status = await this.database.getProjectStatus(project_id);
    const pane = await findPaneByRole('status');
    await sendToPane(pane, this.formatProjectStatus(status));
  }
}

// VS Code extension adapter
class VSCodeAdapter implements DisplayAdapter {
  name = 'vscode';
  supports = ['gui', 'editor-integration'];
  
  async showSessionList(project_id?: string) {
    const sessions = await this.database.getProjectSessions(project_id);
    vscode.window.showQuickPick(
      sessions.map(s => ({ label: s.name, session_id: s.id }))
    );
  }
}
```

### Adapter Registration
```typescript
// Adapters register with the core system
const adapterRegistry = new Map<string, DisplayAdapter>();

function registerAdapter(adapter: DisplayAdapter) {
  adapterRegistry.set(adapter.name, adapter);
  adapter.initialize(cafedelicDatabase);
}

// User can choose active adapters
const activeAdapters = ['terminal', 'vscode']; // From config
```

## Intelligence Pipeline Pattern

### Real-Time Context Analysis
The system maintains dynamic understanding of AI work through continuous analysis:

```typescript
// Context processor pipeline
async function processSessionActivity(session_id: string) {
  // 1. Gather raw activity data
  const activities = await getRecentActivities(session_id, '15 minutes');
  const openFiles = await getSessionOpenFiles(session_id);
  const chatHistory = await getRecentChatMessages(session_id);
  
  // 2. Generate intelligent summary via claude -p
  const contextPrompt = `
    Analyze this Claude Code session activity:
    
    Recent file operations: ${JSON.stringify(activities)}
    Currently open files: ${JSON.stringify(openFiles)}
    Recent chat: ${chatHistory}
    
    Provide a concise summary of:
    1. What Claude is currently working on
    2. The main task or problem being addressed
    3. Current progress/status
    4. Any apparent blockers or issues
  `;
  
  const summary = await callClaudeP(contextPrompt);
  
  // 3. Update database with intelligence
  await updateSessionContext(session_id, {
    current_task: summary.task,
    progress_status: summary.progress,
    open_files: openFiles,
    last_analysis: new Date().toISOString()
  });
  
  // 4. Trigger display updates
  await notifyDisplayAdapters('session_context_updated', {
    session_id,
    summary: summary.task
  });
}
```

### Cross-Session Intelligence
```typescript
// Analyze patterns across multiple sessions
async function analyzeProjectIntelligence(project_id: string) {
  const sessions = await getActiveSessions(project_id);
  
  const insights = [];
  
  // Look for related work across sessions
  for (const sessionA of sessions) {
    for (const sessionB of sessions) {
      if (sessionA.id !== sessionB.id) {
        const filesA = await getSessionFiles(sessionA.id);
        const filesB = await getSessionFiles(sessionB.id);
        const overlap = findFileOverlap(filesA, filesB);
        
        if (overlap.length > 0) {
          insights.push({
            type: 'related_sessions',
            sessions: [sessionA.name, sessionB.name],
            shared_files: overlap,
            recommendation: 'Consider coordinating these sessions - they\'re working on related files'
          });
        }
      }
    }
  }
  
  return insights;
}
```

## Event-Driven Updates Pattern

### Database Triggers
Use SQLite triggers for automatic intelligence updates:

```sql
-- Auto-update project activity timestamp
CREATE TRIGGER update_project_activity 
AFTER INSERT ON activities
BEGIN
  UPDATE projects 
  SET last_activity = datetime('now')
  WHERE id = (
    SELECT project_id FROM sessions 
    WHERE id = NEW.session_id
  );
END;

-- Auto-archive idle sessions
CREATE TRIGGER archive_idle_sessions
AFTER UPDATE ON sessions
WHEN NEW.last_activity < datetime('now', '-24 hours')
BEGIN
  UPDATE sessions
  SET status = 'idle'
  WHERE id = NEW.id AND status = 'active';
END;
```

### Reactive Display Updates
```typescript
// Database change listener
async function watchDatabaseChanges() {
  // SQLite change notifications (via polling or triggers)
  setInterval(async () => {
    const changes = await getRecentChanges();
    
    for (const change of changes) {
      // Notify all active display adapters
      for (const adapter of activeAdapters) {
        await adapter.handleDatabaseChange(change);
      }
    }
  }, 1000); // 1 second polling
}
```

## Claude Code Integration Pattern

### Session Lifecycle Management
```typescript
interface ClaudeCodeSession {
  id: string;
  name: string;
  project_id: string;
  process_id: number;
  working_directory: string;
  branch: string;
  worktree?: string;
  yolo_mode: boolean;
  status: 'starting' | 'active' | 'idle' | 'error' | 'terminated';
  created_at: string;
  last_activity: string;
}

async function createClaudeCodeSession(options: SessionOptions) {
  // 1. Validate project exists
  const project = await getProject(options.project_id);
  if (!project) throw new Error('Project not found');
  
  // 2. Setup working directory (worktree if specified)
  const workingDir = await setupWorkingDirectory(project, options);
  
  // 3. Launch Claude Code process
  const process = await launchClaudeCode({
    directory: workingDir,
    yolo: options.yolo_mode || false,
    session_name: options.name
  });
  
  // 4. Register in database
  const session = await db.run(
    'INSERT INTO sessions (name, project_id, process_id, working_directory, branch, status, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)',
    [options.name, options.project_id, process.pid, workingDir, options.branch, 'starting', new Date().toISOString()]
  );
  
  // 5. Setup activity monitoring
  await setupSessionMonitoring(session.lastID, process.pid);
  
  return { session_id: session.lastID, process_id: process.pid };
}
```

### Activity Monitoring
```typescript
// Monitor Claude Code logs for this session
async function setupSessionMonitoring(session_id: string, process_id: number) {
  // Watch MCP logs for this Claude Code instance
  const logWatcher = createLogWatcher(process_id);
  
  for await (const logEntry of logWatcher) {
    // Parse activity from logs
    const activity = parseClaudeCodeActivity(logEntry);
    
    if (activity) {
      // Store in database
      await db.run(
        'INSERT INTO activities (session_id, type, content, timestamp, files_affected) VALUES (?, ?, ?, ?, ?)',
        [session_id, activity.type, activity.content, activity.timestamp, JSON.stringify(activity.files)]
      );
      
      // Update session context if it's a significant activity
      if (activity.type === 'file_read' || activity.type === 'file_write') {
        await updateSessionContext(session_id, activity);
      }
    }
  }
}
```

This intelligence-first architecture ensures that all AI development work becomes visible, manageable, and coordinatable through a single, powerful database-driven system.