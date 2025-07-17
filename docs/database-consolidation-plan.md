# Database Consolidation & Claude Code Integration Plan

**Analysis Date**: July 16, 2025  
**Current Situation**: Two competing databases (cafedelic.db, schema_v3.sql) + Claude Code's store.db integration needed  
**Goal**: Single unified database with API interface for TUI integration  

## Current Database State Analysis

### Database Files Found
```
/home/alex/code/cafedelic/cafedelic.db                   # Existing V2 database (2778 activities, 30 sessions)
/home/alex/code/cafedelic/database/schema_v3.sql        # V3 intelligence schema (comprehensive)  
/home/alex/code/cafedelic/src/database/fresh_schema.sql  # Focused project/session schema
~/.claude/__store.db                                     # Claude Code's native database
```

### Schema Comparison

**V3 Intelligence Schema (schema_v3.sql)**:
- **Comprehensive**: 12 tables with full intelligence features
- **Advanced**: Health monitoring, coordination opportunities, insights
- **Complex**: Full-text search, triggers, derived data maintenance
- **Future-focused**: Built for Phase 2/3 intelligence features

**Fresh Schema (fresh_schema.sql)**:
- **Focused**: 5 core tables (projects, sessions, activities, file_context, scan_paths)
- **Practical**: Direct mapping to Claude Code concepts
- **Simple**: Minimal triggers, straightforward relationships
- **Current-focused**: Built for immediate Claude Code integration

**Claude Code's store.db**:
- **Native**: Direct integration with Claude Code session data
- **Authoritative**: Single source of truth for session metadata
- **Real-time**: Updated by Claude Code automatically
- **Essential**: Contains session UUIDs, project paths, message metadata

## Consolidation Strategy

### Phase 1: Unified Schema Design

Create a **hybrid schema** that combines the best of both approaches:

```sql
-- Core tables from fresh_schema (immediate functionality)
projects, sessions, activities, file_context, project_scan_paths

-- Enhanced with V3 intelligence tables (future expansion)
session_context, coordination_opportunities, session_health, project_insights

-- Integration tables (Claude Code connectivity)
claude_code_sync, external_sessions
```

### Phase 2: Database Location Strategy

**Single Database Location**: `/home/alex/code/cafedelic/cafedelic.db`
- **Consolidate**: Merge existing cafedelic.db data
- **Migrate**: Apply unified schema to existing database
- **Preserve**: Keep 2778 activities and 30 sessions from V2

### Phase 3: Claude Code Integration Pattern

**Three-Database Architecture**:
1. **Primary**: `cafedelic.db` - Our enhanced intelligence database
2. **Mirror**: `~/.claude/__store.db` - Claude Code's native database (read-only)
3. **Sync**: Regular synchronization between the two

```typescript
interface ClaudeCodeIntegration {
  // Read from Claude Code's database
  readClaudeCodeSessions(): Promise<SessionMetadata[]>;
  readClaudeCodeMessages(sessionId: string): Promise<Message[]>;
  
  // Sync to our database
  syncSessionMetadata(sessions: SessionMetadata[]): Promise<void>;
  syncActivities(sessionId: string, messages: Message[]): Promise<void>;
  
  // Enhanced data (only in our database)
  addIntelligenceContext(sessionId: string, context: SessionContext): Promise<void>;
  trackCoordinationOpportunities(projectId: string): Promise<Opportunity[]>;
}
```

## API Interface Design

### Database Adapter Architecture

```typescript
// High-level API for TUI consumption
interface CafedelicDatabaseAPI {
  // Project management
  projects: {
    list(): Promise<ProjectSummary[]>;
    get(id: string): Promise<ProjectDetails>;
    create(project: CreateProjectRequest): Promise<Project>;
    update(id: string, updates: ProjectUpdate): Promise<Project>;
  };
  
  // Session management  
  sessions: {
    list(projectId?: string): Promise<SessionSummary[]>;
    get(id: string): Promise<SessionDetails>;
    create(session: CreateSessionRequest): Promise<Session>;
    updateStatus(id: string, status: SessionStatus): Promise<Session>;
  };
  
  // Activity tracking
  activities: {
    recent(limit?: number): Promise<Activity[]>;
    forSession(sessionId: string): Promise<Activity[]>;
    forProject(projectId: string): Promise<Activity[]>;
  };
  
  // Intelligence features
  intelligence: {
    getSessionContext(sessionId: string): Promise<SessionContext>;
    getCoordinationOpportunities(projectId: string): Promise<Opportunity[]>;
    generateInsights(projectId: string): Promise<Insight[]>;
  };
  
  // Claude Code integration
  claudeCode: {
    syncSessions(): Promise<SyncResult>;
    importSession(sessionUuid: string): Promise<Session>;
    getSessionHealth(sessionId: string): Promise<HealthMetrics>;
  };
}
```

### Implementation Architecture

```typescript
// Database service layer
class CafedelicDatabase {
  private db: Database;
  private claudeCodeDb: Database;
  
  constructor() {
    this.db = new Database('/home/alex/code/cafedelic/cafedelic.db');
    this.claudeCodeDb = new Database(path.join(os.homedir(), '.claude/__store.db'));
  }
  
  // High-level API methods
  async getProjects(): Promise<ProjectSummary[]> {
    return this.db.all(`
      SELECT p.*, 
             COUNT(s.id) as session_count,
             COUNT(CASE WHEN s.status = 'active' THEN 1 END) as active_sessions,
             MAX(s.last_activity) as last_activity
      FROM projects p
      LEFT JOIN sessions s ON p.id = s.project_id
      WHERE p.status = 'active'
      GROUP BY p.id
      ORDER BY last_activity DESC
    `);
  }
  
  async getSessions(projectId?: string): Promise<SessionSummary[]> {
    const query = projectId 
      ? 'SELECT * FROM active_sessions WHERE project_id = ? ORDER BY last_activity DESC'
      : 'SELECT * FROM active_sessions ORDER BY last_activity DESC';
    const params = projectId ? [projectId] : [];
    return this.db.all(query, params);
  }
  
  // Claude Code integration
  async syncWithClaudeCode(): Promise<SyncResult> {
    const claudeSessions = await this.readClaudeCodeSessions();
    const syncedSessions = [];
    
    for (const claudeSession of claudeSessions) {
      const existingSession = await this.findSessionByUuid(claudeSession.sessionId);
      
      if (!existingSession) {
        // Import new session
        const newSession = await this.importClaudeSession(claudeSession);
        syncedSessions.push(newSession);
      } else {
        // Update existing session
        await this.updateSessionFromClaude(existingSession.id, claudeSession);
      }
    }
    
    return {
      sessionsImported: syncedSessions.length,
      sessionsUpdated: claudeSessions.length - syncedSessions.length,
      lastSync: new Date().toISOString()
    };
  }
  
  private async readClaudeCodeSessions(): Promise<ClaudeSessionMetadata[]> {
    return this.claudeCodeDb.all(`
      SELECT DISTINCT 
        bm.session_id,
        bm.cwd as project_path,
        COUNT(bm.uuid) as message_count,
        MAX(bm.timestamp) as last_activity,
        MIN(bm.timestamp) as created_at
      FROM base_messages bm
      GROUP BY bm.session_id, bm.cwd
      ORDER BY last_activity DESC
    `);
  }
}
```

## Unified Schema Specification

### Core Tables (Enhanced from fresh_schema)

```sql
-- Projects with enhanced Claude Code integration
CREATE TABLE projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    short_id TEXT UNIQUE NOT NULL,
    name TEXT NOT NULL,
    path TEXT NOT NULL UNIQUE,
    normalized_path TEXT UNIQUE, -- Claude Code's path normalization
    description TEXT,
    status TEXT DEFAULT 'active',
    claude_code_project BOOLEAN DEFAULT FALSE,
    discovered_from TEXT DEFAULT 'manual',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Sessions with full Claude Code integration
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    short_id TEXT UNIQUE NOT NULL,
    project_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    claude_session_uuid TEXT UNIQUE, -- From Claude Code JSONL
    claude_store_sync DATETIME, -- Last sync with store.db
    status TEXT DEFAULT 'available',
    task_description TEXT,
    conversation_turns INTEGER DEFAULT 0,
    total_cost_usd REAL DEFAULT 0.0,
    jsonl_file_path TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_activity DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
);

-- Activities enhanced with Claude Code message correlation
CREATE TABLE activities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    claude_message_uuid TEXT, -- Correlation with Claude Code messages
    activity_type TEXT NOT NULL,
    description TEXT NOT NULL,
    file_path TEXT,
    metadata JSON DEFAULT '{}',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES sessions (id) ON DELETE CASCADE
);

-- Claude Code synchronization tracking
CREATE TABLE claude_code_sync (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    sync_type TEXT NOT NULL, -- 'full', 'incremental', 'session'
    sessions_imported INTEGER DEFAULT 0,
    sessions_updated INTEGER DEFAULT 0,
    activities_imported INTEGER DEFAULT 0,
    sync_duration_ms INTEGER,
    last_message_timestamp DATETIME,
    errors JSON DEFAULT '[]',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Intelligence Tables (Selective from V3)

```sql
-- Session context (simplified from V3)
CREATE TABLE session_context (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    current_task TEXT,
    task_description TEXT,
    files_in_context JSON DEFAULT '[]',
    generated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES sessions (id) ON DELETE CASCADE
);

-- Coordination opportunities (essential for delegation platform)
CREATE TABLE coordination_opportunities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_1_id INTEGER NOT NULL,
    session_2_id INTEGER NOT NULL,
    project_id INTEGER NOT NULL,
    opportunity_type TEXT NOT NULL,
    description TEXT,
    shared_files JSON DEFAULT '[]',
    priority_score REAL DEFAULT 0.0,
    status TEXT DEFAULT 'identified',
    identified_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_1_id) REFERENCES sessions (id),
    FOREIGN KEY (session_2_id) REFERENCES sessions (id),
    FOREIGN KEY (project_id) REFERENCES projects (id)
);
```

## Implementation Plan

### Step 1: Database Consolidation
```bash
# Backup existing database
cp cafedelic.db cafedelic.db.backup

# Apply unified schema migration
sqlite3 cafedelic.db < unified_schema.sql

# Migrate existing data to new structure
python scripts/migrate_database.py
```

### Step 2: API Service Creation
```typescript
// src/services/database-api.ts
export class CafedelicDatabaseAPI {
  // Implementation of interface defined above
}

// src/services/claude-code-sync.ts  
export class ClaudeCodeSyncService {
  // Implementation of Claude Code integration
}
```

### Step 3: TUI Integration
```python
# src/ui/textual/services/database_client.py
class DatabaseClient:
    def __init__(self, api_base_url: str = "http://localhost:3001"):
        self.api_base = api_base_url
    
    async def get_projects(self) -> List[ProjectSummary]:
        async with aiohttp.ClientSession() as session:
            async with session.get(f"{self.api_base}/projects") as resp:
                return await resp.json()
    
    async def get_sessions(self, project_id: str = None) -> List[SessionSummary]:
        url = f"{self.api_base}/sessions"
        if project_id:
            url += f"?project_id={project_id}"
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as resp:
                return await resp.json()
```

### Step 4: API Server Setup
```typescript
// src/api/server.ts
import express from 'express';
import { CafedelicDatabaseAPI } from '../services/database-api';

const app = express();
const db = new CafedelicDatabaseAPI();

app.get('/projects', async (req, res) => {
  const projects = await db.projects.list();
  res.json(projects);
});

app.get('/sessions', async (req, res) => {
  const { project_id } = req.query;
  const sessions = await db.sessions.list(project_id);
  res.json(sessions);
});

app.post('/claude-code/sync', async (req, res) => {
  const result = await db.claudeCode.syncSessions();
  res.json(result);
});

app.listen(3001, () => {
  console.log('Cafedelic API server running on port 3001');
});
```

## Key Benefits of This Approach

### 1. Separation of Concerns
- **TUI**: Focuses on UI logic, consumes clean API
- **Database**: Handles data persistence and Claude Code integration
- **API**: Provides stable interface between TUI and database

### 2. Claude Code Integration
- **Read-only access** to Claude Code's store.db (no modification risk)
- **Enhanced data** stored in our database (intelligence, coordination)
- **Regular synchronization** keeps data fresh and accurate

### 3. Scalability
- **API can be extended** for web interface, VS Code extension
- **Database schema supports** future intelligence features
- **Synchronization service** can handle multiple Claude Code instances

### 4. Data Integrity
- **Single source of truth** for our enhanced data
- **Preserved existing data** (2778 activities, 30 sessions)
- **Atomic operations** ensure consistency during sync

## Next Implementation Steps

1. **Create unified schema** by merging fresh_schema + selective V3 features
2. **Build database API service** with Claude Code sync capabilities  
3. **Create HTTP API server** for TUI consumption
4. **Update TUI** to use API instead of direct database access
5. **Implement sync service** to populate real Claude Code data
6. **Test integration** with existing Claude Code sessions

This approach transforms our TUI from mock data to real Claude Code integration while maintaining clean architecture and enabling future expansion to the full intelligence platform vision.