# Product Requirements Document: Cafedelic Intelligence Platform

## Executive Summary

**Product Name**: Cafedelic Intelligence Platform  
**Version**: V3  
**Document Version**: 1.0  
**Date**: July 11, 2025  
**Status**: Draft - Implementation Ready

## Product Vision

### Mission Statement
Transform AI-assisted development from a black box into a transparent, manageable, and coordinatable experience through an intelligent terminal user interface that makes Claude Code sessions visible, trackable, and optimizable.

### Vision Statement
Create the definitive "glass box mission control" for AI development - a rich terminal application that serves as the intelligence layer between developers and their AI assistants, providing real-time visibility into AI work, cross-session coordination, and project-level intelligence insights.

### Product Goals
1. **Visibility**: Make AI development work transparent and comprehensible
2. **Coordination**: Enable management of multiple Claude Code sessions across projects
3. **Intelligence**: Provide actionable insights from AI session data and patterns
4. **Integration**: Seamlessly integrate with existing development workflows
5. **Scalability**: Support individual developers through enterprise team coordination

## User Personas

### Primary Persona: Technical Lead (Alex)
- **Background**: Senior developer managing multiple AI-assisted projects
- **Pain Points**: 
  - Can't see what Claude Code is doing across different sessions
  - Loses context when switching between projects
  - No visibility into AI session health or productivity patterns
- **Goals**: 
  - Monitor AI work across multiple concurrent sessions
  - Quickly switch context between projects
  - Identify coordination opportunities between sessions
- **Use Cases**: Project management, session orchestration, team coordination

### Secondary Persona: Solo Developer (Sam)
- **Background**: Individual contributor using Claude Code for focused development
- **Pain Points**:
  - Forgets which sessions are working on what
  - Difficulty tracking progress across complex features
  - No way to preserve or share session context
- **Goals**:
  - Track personal development sessions
  - Maintain context across work sessions
  - Monitor progress and identify blockers
- **Use Cases**: Personal productivity, session management, progress tracking

## Core Features

### 1. Project Management Dashboard

**Overview**: Central hub displaying all active projects with real-time status

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

### 2. Tabbed Session Management

**Overview**: Multi-tab interface for managing Claude Code sessions within projects

**User Stories**:
- As a developer, I want to click on a Claude Code session to open it in a new tab
- As a technical lead, I want to see session status (active, idle, stuck) at a glance
- As a developer, I want to close, restart, or duplicate sessions from the tab interface
- As a developer, I want to rearrange tabs to organize my workflow

**Acceptance Criteria**:
- Use Textual's `TabbedContent` widget for session tabs
- Display session status with color-coded indicators (green=active, yellow=idle, red=stuck)
- Support tab reordering, closing, and context menu actions
- Show session metadata: project, duration, current task, file context
- Persist tab order and state in database

**Technical Requirements**:
- Integrate with Claude Code process management
- Real-time session health monitoring
- Database persistence of tab state and ordering
- Keyboard shortcuts for tab navigation (Ctrl+T, Ctrl+W, Ctrl+Tab)

### 3. File Tree Integration

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

### 4. Intelligence Feed

**Overview**: Real-time activity stream with AI-generated insights

**User Stories**:
- As a developer, I want to see what Claude Code is currently doing across all sessions
- As a technical lead, I want AI-generated summaries of session progress and blockers
- As a developer, I want to receive notifications when coordination opportunities arise
- As a developer, I want to see historical activity patterns and trends

**Acceptance Criteria**:
- Display real-time activity stream with timestamps and session context
- Generate AI summaries using claude -p integration for session analysis
- Highlight coordination opportunities between sessions
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