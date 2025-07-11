# Product Requirements Document: Cafedelic Intelligence Platform

## Executive Summary

**Product Name**: Cafedelic Intelligence Platform  
**Version**: V3  
**Document Version**: 1.0  
**Date**: July 11, 2025  
**Status**: Draft - Implementation Ready

## Product Vision

### Mission Statement
Transform AI-assisted development into an asynchronous task delegation platform that enables developers to orchestrate, coordinate, and manage Claude Code conversations through intelligent session management, quick-chat interfaces, and automated task handoffs.

### Vision Statement
Create the definitive task delegation platform for AI development - a rich terminal application that transforms Claude Code from isolated conversations into an orchestrated workflow engine, enabling asynchronous task management, intelligent session coordination, and automated handoffs between AI instances.

### Product Goals
1. **Delegation**: Enable quick task assignment to Claude Code sessions via natural language
2. **Orchestration**: Coordinate multiple AI conversations with task-state management
3. **Automation**: Intelligent handoffs and context preservation between sessions
4. **Intelligence**: Proactive suggestions for task routing and session optimization
5. **Integration**: Seamless `claude --resume` integration for async communication

## User Personas

### Primary Persona: Delegation Manager (Alex)
- **Background**: Technical lead orchestrating AI-assisted development across multiple projects
- **Pain Points**: 
  - Can't delegate tasks to Claude Code sessions asynchronously
  - No way to monitor task progress across multiple AI conversations
  - Manual context switching between sessions breaks workflow
- **Goals**: 
  - Quick-delegate tasks to appropriate Claude Code sessions
  - Monitor task states and completion status in real-time
  - Automate handoffs between sessions for complex workflows
- **Use Cases**: Task delegation, workflow orchestration, async AI coordination

### Secondary Persona: Solo Developer (Sam)
- **Background**: Individual contributor using AI delegation for complex feature development
- **Pain Points**:
  - Switching between planning, analysis, and implementation modes manually
  - Can't find relevant previous conversations for similar tasks
  - No way to queue multiple tasks for sequential AI processing
- **Goals**:
  - Use quick-chat interface for rapid task assignment
  - Leverage find_relevant_chats() to resume context-rich sessions
  - Manage task states with /plan, /analyze workflow commands
- **Use Cases**: Task queuing, context discovery, workflow automation

## Core Features

### 1. Quick-Chat Delegation Widget

**Overview**: Primary interface for rapid task assignment to Claude Code sessions via natural language

**User Stories**:
- As a delegation manager, I want to type a task and assign it to a specific session instantly
- As a developer, I want to see task status updates in real-time without switching contexts
- As a user, I want find_relevant_chats(query) to suggest existing sessions with relevant context
- As a developer, I want to use /plan, /analyze workflow commands for task state management

**Acceptance Criteria**:
- Prominent quick-chat input field always accessible in main interface
- Session selection dropdown with status indicators and task descriptions
- Auto-completion for task types and session targeting
- Real-time task progress updates and completion notifications
- Integration with `claude --resume {session_id}` for async communication
- find_relevant_chats() function returns contextually similar conversations

**Technical Requirements**:
- Natural language processing for task categorization
- MCP tool integration for session orchestration
- Database tracking of task assignments and completions
- WebSocket communication for real-time updates

**Workflow Commands**:
- `/plan` - Initiate planning phase for task
- `/keep-planning` - Continue planning without execution
- `/analyze` - Begin analysis of requirements or codebase
- `/keep-analyzing` - Extend analysis phase
- `/act` - Execute planned tasks
- `/review` - Review completed work
- `/coordinate` - Identify cross-session dependencies
- `/handoff` - Transfer task to different session with context

### 2. Project Management Dashboard

**Overview**: Central hub displaying all active projects with task delegation status

**User Stories**:
- As a developer, I want to see all my projects in a grid view so I can quickly assess current status
- As a technical lead, I want to click on a project to see its active Claude Code sessions
- As a developer, I want to create new projects and associate them with git repositories

**Acceptance Criteria**:
- Display projects in a responsive grid layout with status indicators
- Show project metadata: name, path, last activity, session count, health status
- Support project creation, editing, and archival
- Integrate with ~/.claude/projects while using SQLite as source of truth
- Real-time updates when project status changes

**Technical Requirements**:
- Use Textual's `DataTable` widget for project listing
- Implement async database queries for real-time updates
- Support both grid and list view modes
- Include search and filtering capabilities

### 3. Task-State Session Management

**Overview**: Multi-tab interface for managing Claude Code sessions with task-state awareness

**User Stories**:
- As a developer, I want to see session task states: "Planning... feature xyz" or "Analyzing... feature abc"
- As a delegation manager, I want to see which sessions are available for new task assignment
- As a developer, I want to monitor task progress without interrupting AI workflows
- As a user, I want to resume relevant sessions based on task similarity

**Acceptance Criteria**:
- Use Textual's `TabbedContent` widget with task-state indicators
- Display session status: Planning, Analyzing, Implementing, Reviewing, Stuck, Available
- Show current task description and progress indicators
- Support task handoffs between sessions with context preservation
- Integrate find_relevant_chats() for session discovery and resumption
- Quick-delegate tasks directly from tab interface

**Technical Requirements**:
- Integrate with Claude Code process management
- Real-time session health monitoring
- Database persistence of tab state and ordering
- Keyboard shortcuts for tab navigation (Ctrl+T, Ctrl+W, Ctrl+Tab)

### 4. File Tree Integration

**Overview**: Context-aware file browser showing session-relevant files

**User Stories**:
- As a developer, I want to see which files are currently open in each session
- As a developer, I want to browse project files with indicators showing recent AI activity
- As a technical lead, I want to see file overlap between sessions for coordination
- As a developer, I want to click on files to see them in the session context

**Acceptance Criteria**:
- Use Textual's `DirectoryTree` widget with custom styling
- Highlight files with recent AI activity (read, write, create, delete)
- Show file status indicators: modified, created, referenced, ignored
- Support file opening in associated sessions or external editors
- Display file metadata: last modified, session association, activity count

**Technical Requirements**:
- Integration with git file status
- Real-time file system monitoring
- Database tracking of file access patterns
- Efficient tree rendering for large codebases

### 5. Task Delegation Queue

**Overview**: Visual queue management for pending, active, and completed task delegations

**User Stories**:
- As a delegation manager, I want to see all pending tasks waiting for session assignment
- As a developer, I want to track task completion status across multiple sessions
- As a user, I want to reorder task priorities and reassign to different sessions
- As a developer, I want automated task routing based on session expertise and availability

**Acceptance Criteria**:
- Display task queue with priority indicators and estimated completion times
- Support drag-and-drop task reordering and session reassignment
- Show task dependencies and coordination requirements
- Automated suggestions for optimal task-to-session matching
- Integration with find_relevant_chats() for context-aware assignment

**Technical Requirements**:
- Database persistence of task queue state and priorities
- Real-time updates via WebSocket for collaborative workflows
- Machine learning for session expertise modeling
- Integration with Claude Code session health monitoring

### 6. Intelligence Feed with Delegation Insights

**Overview**: Real-time activity stream focused on task delegation and coordination opportunities

**User Stories**:
- As a developer, I want to see which sessions are actively working on delegated tasks
- As a delegation manager, I want AI-generated suggestions for task handoffs
- As a developer, I want notifications when sessions complete delegated work
- As a user, I want to see coordination opportunities between related tasks

**Acceptance Criteria**:
- Display real-time task delegation status with session assignments
- Generate AI suggestions for optimal task routing and coordination
- Highlight task delegation opportunities and session coordination needs
- Support activity filtering by session, project, file, or activity type
- Provide activity search and historical browsing

**Technical Requirements**:
- WebSocket or polling for real-time updates
- Integration with claude -p for intelligent analysis
- Database indexing for efficient activity queries
- Configurable notification thresholds and preferences

### 5. Session Health Monitoring

**Overview**: Visual indicators and alerts for session performance and health

**User Stories**:
- As a developer, I want to know when a session appears stuck or unresponsive
- As a technical lead, I want metrics on session productivity and effectiveness
- As a developer, I want recommendations for optimizing session performance
- As a technical lead, I want alerts when sessions need attention or intervention

**Acceptance Criteria**:
- Display health metrics: responsiveness, activity rate, error frequency, progress velocity
- Visual health indicators using color coding and status badges
- Automated detection of stuck, idle, or struggling sessions
- Recommendations for session optimization and intervention
- Historical health trends and performance analytics

**Technical Requirements**:
- Real-time process monitoring and health checks
- Machine learning models for anomaly detection
- Configurable alerting thresholds and notification preferences
- Database storage of health metrics and trends

## User Interface Specifications

### Layout Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Cafedelic Intelligence Platform                              │ Header
├─────────────────────────────────────────────────────────────┤
│ Projects │ Session: auth-refactor │ Session: ui-update │ +  │ Tab Bar
├──────────┼─────────────────────────┼──────────────────────────┤
│          │ ┌─────────────────────┐ │ ┌──────────────────────┐ │
│ Project  │ │ Current Task:       │ │ │ Current Task:        │ │
│ Browser  │ │ Implementing OAuth  │ │ │ Adding dark theme    │ │
│          │ │                     │ │ │                      │ │
│ • proj-1 │ │ Files in Context:   │ │ │ Files in Context:    │ │
│ • proj-2 │ │ ├── auth/oauth.js   │ │ │ ├── styles/theme.css │ │
│ • proj-3 │ │ ├── middleware.js   │ │ │ ├── components/ui.js │ │
│          │ │ └── tests/auth.test │ │ │ └── utils/colors.js  │ │
│          │ │                     │ │ │                      │ │
│          │ │ Recent Activity:    │ │ │ Status: ● Active     │ │
│          │ │ Reading OAuth docs  │ │ │ Duration: 1h 23m     │ │
│          │ └─────────────────────┘ │ └──────────────────────┘ │
├──────────┴─────────────────────────┴──────────────────────────┤
│ Intelligence Feed:                                           │
│ [15:42] auth-refactor: Analyzing authentication patterns... │
│ [15:41] ui-update: Modified styles/theme.css               │
│ [15:40] Coordination: Both sessions editing shared utils/   │
├─────────────────────────────────────────────────────────────┤
│ Status: 2 active sessions │ Health: ● Good │ Q:Quit D:Dark │ Footer
└─────────────────────────────────────────────────────────────┘
```

### Widget Specifications

**Primary Widgets**:
- `TabbedContent`: Session tab management with custom tab styling
- `DirectoryTree`: Project browser with activity indicators
- `DataTable`: Project listing with sortable columns
- `ScrollView`: Intelligence feed with infinite scroll
- `Grid`: Project dashboard with responsive layout

**Custom Components**:
- `SessionCard`: Individual session display with health indicators
- `ActivityItem`: Intelligence feed entries with timestamp and context
- `HealthIndicator`: Visual status display with color coding
- `ProjectTile`: Grid item for project dashboard

## Technical Architecture

### Technology Stack

**Frontend**: Python + Textual Framework
- Rich terminal UI with web-capable output
- Async/await support for real-time updates
- Built-in widgets: tabs, trees, tables, grids
- CSS-like styling with responsive layouts

**Backend**: TypeScript + Node.js
- MCP (Model Context Protocol) server for AI integration
- SQLite database for intelligence storage
- Express HTTP server for API endpoints
- Real-time activity monitoring and analysis

**Integration Layer**:
- Claude Code SDK for session management
- File system watching for activity detection
- Git integration for repository status
- claude -p integration for intelligent analysis

### Database Schema (Enhanced V3)

```sql
-- Core project management
CREATE TABLE projects (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    path TEXT NOT NULL UNIQUE,
    status TEXT DEFAULT 'active',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_activity TIMESTAMP,
    config_hash TEXT,
    metadata JSON
);

-- Enhanced session tracking
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY,
    project_id INTEGER REFERENCES projects(id),
    process_id INTEGER,
    name TEXT,
    status TEXT DEFAULT 'starting',
    assistant_type TEXT DEFAULT 'claude-code',
    working_directory TEXT,
    branch TEXT,
    tab_order INTEGER,
    ui_state JSON,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_activity TIMESTAMP,
    ended_at TIMESTAMP
);

-- Session context and intelligence
CREATE TABLE session_context (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id),
    context_type TEXT,
    current_task TEXT,
    files_in_context JSON,
    progress_summary TEXT,
    confidence_score REAL,
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- File access tracking with session correlation
CREATE TABLE file_access (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id),
    file_path TEXT NOT NULL,
    access_type TEXT NOT NULL,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    content_preview TEXT,
    line_numbers TEXT
);

-- Activity stream with intelligent analysis
CREATE TABLE activities (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id),
    activity_type TEXT NOT NULL,
    raw_content TEXT,
    processed_content TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    metadata JSON
);

-- Cross-session intelligence and coordination
CREATE TABLE coordination_opportunities (
    id INTEGER PRIMARY KEY,
    session_1_id INTEGER REFERENCES sessions(id),
    session_2_id INTEGER REFERENCES sessions(id),
    opportunity_type TEXT,
    description TEXT,
    confidence_score REAL,
    status TEXT DEFAULT 'identified',
    identified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Health monitoring and metrics
CREATE TABLE session_health (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id),
    health_metric TEXT,
    value REAL,
    threshold_status TEXT,
    measured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### API Specifications

**MCP Tools (Primary Interface)**:
```typescript
// Project management
create_project(name: string, path: string, config?: object): Project
read_project_status(project_id: string): ProjectStatus
list_active_projects(): Project[]
update_project_metadata(project_id: string, metadata: object): boolean
archive_project(project_id: string): boolean

// Session orchestration
create_session(project_id: string, options?: SessionOptions): Session
read_session_context(session_id: string): SessionContext
update_session_status(session_id: string, status: string): boolean
list_project_sessions(project_id: string): Session[]
terminate_session(session_id: string): boolean

// Intelligence and analysis
analyze_current_activity(session_id: string): ActivityAnalysis
generate_task_summary(session_id: string): TaskSummary
track_coordination_opportunities(project_id?: string): CoordinationOpportunity[]
get_session_health_metrics(session_id: string): HealthMetrics[]
```

## Implementation Strategy

### Phase 1: Foundation (2-3 weeks)
**Goal**: Basic infrastructure and database enhancement

**Deliverables**:
- Enhanced SQLite schema with migration from V2
- Basic TUI skeleton with project listing
- MCP server with core CRUD operations
- Database connection and basic queries

**Success Criteria**:
- Can display projects from database in TUI
- MCP tools work for basic project operations
- Database migration preserves existing data (2778 activities, 30 sessions)

### Phase 2: Core Interface (3-4 weeks)
**Goal**: Tabbed session management and basic intelligence

**Deliverables**:
- Tabbed interface for session management
- Basic file tree integration
- Real-time activity monitoring
- Session health indicators

**Success Criteria**:
- Can open sessions in tabs with real-time status
- File tree shows session-relevant files
- Activity feed displays real-time session events
- Health indicators work for basic session monitoring

### Phase 3: Intelligence Layer (4-5 weeks)
**Goal**: AI-powered insights and coordination

**Deliverables**:
- claude -p integration for session analysis
- Cross-session coordination detection
- Enhanced intelligence feed with AI summaries
- Advanced health monitoring with alerts

**Success Criteria**:
- AI generates meaningful session summaries
- System identifies coordination opportunities
- Intelligence feed provides actionable insights
- Health monitoring detects stuck/idle sessions

### Phase 4: Polish and Scale (2-3 weeks)
**Goal**: Production readiness and advanced features

**Deliverables**:
- Performance optimization for large projects
- Advanced configuration and customization
- Export/import functionality
- Documentation and onboarding

**Success Criteria**:
- Handles projects with 100+ files efficiently
- Customizable UI themes and layouts
- Can export session data and insights
- Complete user documentation and tutorials

## Success Metrics

### User Adoption Metrics
- **Time to First Value**: < 5 minutes from installation to useful project view
- **Daily Active Usage**: 80% of users use the tool daily during development
- **Session Creation Rate**: Average 3+ sessions per user per day
- **Feature Adoption**: 70% of users regularly use tabbed interface

### Product Performance Metrics
- **Response Time**: < 100ms for UI interactions, < 500ms for database queries
- **Session Health Accuracy**: 90% accuracy in detecting stuck/idle sessions
- **Intelligence Quality**: 85% of AI-generated summaries rated useful by users
- **Coordination Detection**: 75% accuracy in identifying cross-session opportunities

### Technical Quality Metrics
- **Uptime**: 99.5% availability during active development hours
- **Data Integrity**: Zero data loss incidents during normal operation
- **Performance**: Support 50+ concurrent sessions without degradation
- **Compatibility**: Works on Linux, macOS, Windows with Python 3.8+

## Risk Assessment

### Technical Risks
- **Complexity Risk**: TUI development may be more complex than anticipated
  - *Mitigation*: Use proven Textual framework, start with simple widgets
- **Performance Risk**: Real-time updates may impact responsiveness
  - *Mitigation*: Implement efficient polling, async database operations
- **Integration Risk**: Claude Code SDK may have limitations
  - *Mitigation*: Fallback to log parsing, process monitoring

### Product Risks
- **User Adoption Risk**: Terminal UI may not appeal to all developers
  - *Mitigation*: Focus on power users first, ensure excellent performance
- **Feature Creep Risk**: Ambitious scope may delay core functionality
  - *Mitigation*: Strict phase-based development, MVP-first approach
- **Maintenance Risk**: Complex codebase may be hard to maintain
  - *Mitigation*: Strong documentation, modular architecture, comprehensive testing

## Appendices

### A. User Journey Maps
*[Detailed user journey maps would be included here for each persona]*

### B. Competitive Analysis
*[Analysis of existing tools like tmux, screen, VS Code, etc.]*

### C. Technical Deep Dives
*[Detailed technical specifications for complex features]*

### D. Market Research
*[User interviews, surveys, and market analysis]*

---

**Document Approval**:
- Product Owner: [Pending]
- Technical Lead: [Pending]
- Design Lead: [Pending]

**Next Steps**:
1. Review and approve PRD
2. Begin Phase 1 implementation
3. Set up development infrastructure
4. Create detailed technical specifications