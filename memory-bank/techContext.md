# Technical Context

## Technology Stack (Intelligence-First Architecture)

### Core Database Layer
- **SQLite3**: Single source of truth for all intelligence data
- **Node.js**: v20+ for MCP server and intelligence processing
- **TypeScript**: Strict typing for database operations and MCP tools
- **No external dependencies**: Built on platform standard libraries

### MCP Server Architecture
```typescript
// Core MCP server providing conversational interface
interface CafedelicMCPServer {
  // Project management tools
  create_project, read_project_status, update_project_metadata, 
  list_active_projects, archive_project,
  
  // Session orchestration tools  
  create_session, read_session_context, update_session_status,
  list_project_sessions, terminate_session,
  
  // Intelligence analysis tools
  analyze_current_activity, generate_task_summary, track_file_access,
  identify_coordination_opportunities, preserve_session_context
}
```

### Display Adapter Plugin System
```typescript
interface DisplayAdapter {
  name: string;
  supports: string[]; // ['terminal', 'vscode', 'web']
  initialize(database: Database): Promise<void>;
  displayProjectStatus(project_id: string): Promise<void>;
  showSessionList(project_id?: string): Promise<void>;
  updateActivityFeed(activities: Activity[]): Promise<void>;
}
```

## Project Structure (V3 Intelligence Layer)

```
cafedelic/
├── src/
│   ├── mcp-server.ts                  # Primary HTTP server with MCP tools
│   ├── mcp-tools/                     # Individual MCP tools
│   │   ├── project-tools.ts           # Project management
│   │   ├── session-tools.ts           # Session orchestration  
│   │   └── pane-tools.ts              # Tmux pane management
│   ├── services/                      # Service layer
│   │   ├── claude-code-service.ts     # HTTP API for TUI (IMPLEMENTED)
│   │   ├── claude-discovery.ts        # Claude Code filesystem access
│   │   ├── project-service.ts         # Project management
│   │   └── session-service.ts         # Session management
│   ├── intelligence/                  # AI analysis engine (PLANNED)
│   │   ├── context-analyzer.ts        # claude -p integration
│   │   ├── pattern-detector.ts        # Cross-session analysis
│   │   ├── session-health.ts          # Health monitoring
│   │   └── insight-generator.ts       # Automatic insights
│   ├── claude-code/                   # Claude Code integration (PLANNED)
│   │   ├── session-manager.ts         # Process lifecycle
│   │   ├── activity-monitor.ts        # Log watching
│   │   └── log-parser.ts              # Activity extraction
│   └── ui/                            # User interface layer
│       ├── textual/                   # Python TUI implementation (WORKING)
│       │   ├── hello_world.py         # Complete delegation platform main app
│       │   ├── run.py                 # Entry point script
│       │   ├── cafe_ui/
│       │   │   ├── components/        # Working UI components
│       │   │   │   ├── quick_chat.py  # Task delegation widget
│       │   │   │   ├── session_tabs.py # Session management with task states
│       │   │   │   └── task_feed.py   # Real-time delegation status
│       │   │   ├── screens/
│       │   │   │   └── main_dashboard.py # Complete integrated interface
│       │   │   ├── data/
│       │   │   │   └── claude_code_data.py # Real Claude Code data access (IMPLEMENTED)
│       │   │   └── adapter.py         # HTTP API interface (IMPLEMENTED)
│       │   ├── requirements.txt       # Python dependencies (textual, aiofiles, aiosqlite)
│       │   ├── README.md              # Setup instructions
│       │   └── venv/                  # Virtual environment (auto-created)
│       ├── terminal-adapter.ts        # Tmux integration (legacy)
│       ├── vscode-adapter.ts          # VS Code extension
│       └── cli-adapter.ts             # Command line interface
├── database/                          # Intelligence database (NEW)
│   ├── schema_v3.sql                  # Enhanced SQLite schema
│   ├── migrations/                    # Database migration system
│   └── cafedelic.db                   # Current V2 database (2778 activities, 30 sessions)
├── docs/                              # Comprehensive documentation (NEW)
│   ├── PRD.md                         # Product Requirements Document
│   ├── IMPLEMENTATION_ROADMAP.md      # 16-week phased development plan
│   └── UI_MOCKUPS.md                  # Detailed ASCII interface mockups
├── scripts/                           # Legacy bash scripts (V2)
│   ├── emacs/                         # Emacs integration
│   └── pane-management/               # Tmux operations
└── config/
    ├── default.json                   # Default configuration
    └── adapters/                      # Adapter configurations
```

## Architecture Decisions

### Database-First Design
**Rationale**: SQLite as single source of truth enables rich analysis and coordination
- All project/session/activity data flows through database
- Complex queries for pattern recognition and insights
- Atomic operations with transaction support
- No in-memory state - database is the state

### MCP-Native Interface
**Rationale**: Natural language conversation is the primary interface
- All operations accessible through conversational tools
- Rich context in every response for intelligent analysis
- Error handling that explains problems in human terms
- Designed for orchestrator Claude instance interaction

### Plugin Architecture for Display
**Rationale**: Separate intelligence layer from presentation concerns
- Users can choose terminal, VS Code, web, or custom displays
- Intelligence layer remains agnostic to presentation
- Easy to add new display options without touching core logic
- Clean separation enables independent development

### Assistant-Agnostic Design
**Rationale**: Support future expansion beyond Claude Code
- Database schema includes `assistant_type` field
- Activity parsing abstracted from specific log formats
- Session management supports different process types
- Foundation for Cline, Cursor, future AI assistants

## Core Technical Components

### Intelligence Processing Pipeline
```typescript
// Real-time intelligence generation
async function processSessionActivity(session_id: string) {
  // 1. Ingest raw activity from Claude Code logs
  const rawActivity = await parseClaudeCodeLogs(session_id);
  
  // 2. Store structured activity in database
  await storeActivity(session_id, rawActivity);
  
  // 3. Generate intelligence via claude -p analysis
  const intelligence = await analyzeSessionContext(session_id);
  
  // 4. Update session context with insights
  await updateSessionContext(session_id, intelligence);
  
  // 5. Trigger display adapter updates
  await notifyDisplayAdapters('session_updated', { session_id });
}
```

### Claude Code Integration
```typescript
// Session lifecycle management
interface ClaudeCodeIntegration {
  createSession(options: SessionOptions): Promise<SessionInfo>;
  monitorSession(session_id: string): AsyncIterator<Activity>;
  terminateSession(session_id: string): Promise<void>;
  getSessionHealth(session_id: string): Promise<HealthMetrics>;
}

// Activity monitoring from logs
async function* monitorClaudeCodeActivity(session_id: string) {
  const logPath = getClaudeCodeLogPath(session_id);
  const logWatcher = createLogWatcher(logPath);
  
  for await (const logEntry of logWatcher) {
    const activity = parseLogEntry(logEntry);
    if (activity) {
      yield activity;
    }
  }
}
```

### Cross-Session Intelligence
```typescript
// Pattern recognition across sessions
async function analyzeProjectPatterns(project_id: string) {
  const sessions = await getActiveSessions(project_id);
  const activities = await getRecentActivities(project_id, '24 hours');
  
  return {
    file_overlaps: findFileOverlaps(sessions),
    concept_clusters: extractConceptClusters(activities),
    coordination_opportunities: identifyCoordination(sessions),
    potential_conflicts: detectConflicts(sessions),
    knowledge_gaps: findKnowledgeGaps(activities)
  };
}
```

## Configuration & Deployment

### Database Configuration
```typescript
// Database initialization
interface DatabaseConfig {
  path: string;                    // Default: ~/.cafedelic/intelligence.db
  backup_interval: string;         // Default: "1 hour"
  retention_days: number;          // Default: 90
  auto_vacuum: boolean;            // Default: true
}
```

### MCP Server Configuration
```json
{
  "mcp_server": {
    "host": "localhost",
    "port": 3000,
    "max_connections": 10,
    "request_timeout": 30000
  },
  "intelligence": {
    "claude_p_enabled": true,
    "analysis_interval": "5 minutes",
    "insight_generation": true,
    "cross_session_analysis": true
  },
  "display_adapters": {
    "enabled": ["terminal", "cli"],
    "terminal": {
      "tmux_integration": true,
      "default_session": "main"
    }
  }
}
```

### Claude Code Integration Setup
```bash
# Environment setup for Claude Code monitoring
export CLAUDE_CODE_LOG_PATH="~/.claude/logs"
export CAFEDELIC_DB_PATH="~/.cafedelic/intelligence.db"
export CAFEDELIC_MCP_PORT="3000"

# TUI-first approach (PRIMARY INTERFACE)
cafe open textual               # Launch main TUI delegation platform

# CLI for debugging/utility (Claude Code can use these for debugging)
cafe sessions load              # Load sessions from Claude Code into database
cafe sessions list              # List sessions with filtering options
cafe sessions list --project myproject    # Filter by project
cafe sessions list --since "2 days ago"   # Filter by date range
cafe projects scan ~/code       # Scan directory for projects
cafe projects list              # List tracked projects

# MCP server (secondary)
npm run start:mcp-server

# In separate terminal, test MCP connection
npx @anthropic-ai/mcp-client connect http://localhost:3000
```

## Performance Characteristics

### Database Performance
- **Query Response**: <50ms for typical project/session queries
- **Activity Ingestion**: 1000+ activities/second
- **Intelligence Generation**: 2-5 seconds for claude -p analysis
- **Cross-Session Analysis**: <10 seconds for projects with <100 sessions

### MCP Server Performance
- **Tool Execution**: <100ms for simple CRUD operations
- **Complex Analysis**: 2-10 seconds for intelligence tools
- **Concurrent Sessions**: Supports 10+ simultaneous MCP connections
- **Memory Usage**: ~100MB baseline + ~10MB per active project

### Intelligence Processing
- **Real-time Analysis**: <1 second activity processing latency
- **Context Updates**: Updated within 5 seconds of activity
- **Pattern Recognition**: Runs every 5 minutes for active projects
- **Insight Generation**: Triggered by significant activity patterns

## Security & Data Handling

### Data Privacy
- All data stored locally in SQLite database
- No external API calls except for claude -p analysis
- Claude -p receives only structured activity data, not raw code
- User controls all data retention and deletion

### Process Isolation
- MCP server runs as user process
- Claude Code sessions sandboxed to project directories
- No elevated privileges required
- File system access limited to project paths and logs

### Database Security
- SQLite database secured with file system permissions
- No network database access
- Automatic backups to prevent data loss
- Schema migrations handle version upgrades safely

## Integration Points

### With Existing V2 Foundation
- Retains proven WTE pipeline for activity monitoring
- Bash scripts remain available for direct operations
- Property-based pane management system unchanged
- Existing tmux integration scripts still functional

### With Claude Code Ecosystem
- Integrates with Claude Code's native project system
- Respects Claude Code's session and workspace management
- Enhances rather than replaces Claude Code functionality
- Compatible with Claude Code SDK and CLI

### With User Development Environment
- Works with any terminal multiplexer (tmux, screen, etc.)
- Supports any editor through display adapters
- Integrates with existing git workflows
- No changes required to development processes

## Future Technical Directions

### Phase 1: Core Intelligence (Current Focus)
- SQLite database with comprehensive schema
- MCP toolset for project/session management
- Basic claude -p integration for context analysis
- Terminal display adapter

### Phase 2: Advanced Intelligence
- Real-time cross-session coordination
- Predictive session health monitoring
- Advanced pattern recognition and insights
- Multi-project intelligence analysis

### Phase 3: Multi-Assistant Platform
- Support for Cline, Cursor, other AI assistants
- Universal intelligence layer across tools
- Advanced workflow automation
- Enterprise features and scaling

### Maintaining Technical Simplicity
All enhancements must:
- Use SQLite as single source of truth
- Maintain MCP-first interface design
- Follow display adapter separation pattern
- Keep core intelligence processing performant
- Preserve local-first, privacy-focused approach

The technical foundation prioritizes intelligence generation and natural language interaction while maintaining the proven simplicity and reliability of the V2 architecture.