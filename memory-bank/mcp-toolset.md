# MCP Toolset

## Overview

The MCP toolset provides a natural language interface for all cafedelic operations. These tools enable conversational management of projects, sessions, and intelligence analysis through an orchestrator Claude instance.

## Tool Design Principles

### 1. Safety First
All tools follow CRUD principles with comprehensive validation:
- **Create**: Validate inputs and prevent duplicates
- **Read**: Always return consistent data structures, never fail silently
- **Update**: Atomic operations with rollback on failure
- **Delete**: Soft deletes with audit trails

### 2. Conversational Interface
Tools are designed for natural language interaction:
- Clear, descriptive names
- Rich return data that can be summarized conversationally
- Error messages that explain what went wrong and how to fix it

### 3. Comprehensive Context
Every tool provides enough context for intelligent responses:
- Related data included in responses
- Metadata for analysis and insights
- Cross-references to related entities

## Project Management Tools

### create_project
**Purpose**: Register a new project in the intelligence database

```typescript
interface CreateProjectParams {
  name: string;                    // Human-friendly project name
  repo_path: string;              // Absolute path to git repository
  description?: string;           // Optional project description
  default_branch?: string;        // Default branch for new sessions (default: 'main')
}

interface CreateProjectResponse {
  project_id: string;
  name: string;
  repo_path: string;
  status: 'active';
  created_at: string;
  git_info: {
    current_branch: string;
    has_worktrees: boolean;
    remote_url?: string;
  };
}
```

**Validation**:
- Verify repo_path exists and contains .git directory
- Check for existing project with same repo_path
- Validate git repository is accessible

**Example Usage**:
```
Human: "Add my auth-service project at /home/alex/code/auth-service"
Assistant: create_project("auth-service", "/home/alex/code/auth-service", "OAuth2 authentication microservice")
```

### read_project_status
**Purpose**: Get comprehensive project information and current activity

```typescript
interface ReadProjectStatusParams {
  project_id: string;
  include_sessions?: boolean;     // Include active session details
  include_recent_activity?: boolean; // Include last 24h activity summary
  activity_hours?: number;        // Hours of activity to include (default: 24)
}

interface ReadProjectStatusResponse {
  project: {
    id: string;
    name: string;
    repo_path: string;
    description: string;
    status: 'active' | 'archived' | 'paused';
    created_at: string;
    last_activity: string;
  };
  sessions: {
    active: SessionSummary[];
    idle: SessionSummary[];
    total_count: number;
  };
  activity_summary: {
    total_operations: number;
    files_accessed: number;
    most_active_session: string;
    time_range: string;
  };
  git_status: {
    current_branch: string;
    uncommitted_changes: boolean;
    worktrees: WorktreeInfo[];
  };
  insights: ProjectInsight[];
}
```

**Example Usage**:
```
Human: "What's the status of my auth-service project?"
Assistant: read_project_status("auth-service-123", true, true)
```

### update_project_metadata
**Purpose**: Modify project information and settings

```typescript
interface UpdateProjectMetadataParams {
  project_id: string;
  updates: {
    name?: string;
    description?: string;
    status?: 'active' | 'archived' | 'paused';
    default_branch?: string;
  };
}
```

**Example Usage**:
```
Human: "Pause the auth-service project for now"
Assistant: update_project_metadata("auth-service-123", { status: "paused" })
```

### list_active_projects
**Purpose**: Overview of all current projects

```typescript
interface ListActiveProjectsResponse {
  projects: ProjectSummary[];
  totals: {
    active_projects: number;
    total_sessions: number;
    active_sessions: number;
  };
  recent_activity: {
    most_active_project: string;
    total_operations_today: number;
  };
}
```

## Session Management Tools

### create_session
**Purpose**: Start a new Claude Code session for a project

```typescript
interface CreateSessionParams {
  project_id: string;
  name: string;                   // Human-friendly session name
  branch?: string;                // Git branch (default: project default)
  worktree?: string;             // Optional worktree path
  yolo_mode?: boolean;           // Enable yolo mode (default: false)
  description?: string;          // Session purpose/goal
  tags?: string[];              // Categorization tags
}

interface CreateSessionResponse {
  session_id: string;
  name: string;
  project_name: string;
  process_id: number;            // Claude Code process PID
  working_directory: string;
  git_context: {
    branch: string;
    worktree?: string;
    commit_hash: string;
  };
  status: 'starting' | 'active';
  created_at: string;
  estimated_startup_time: string; // "~5 seconds"
}
```

**Validation**:
- Verify project exists and is active
- Check branch exists if specified
- Validate worktree path if provided
- Ensure session name is unique within project

**Example Usage**:
```
Human: "Start a new session called 'oauth-refactor' for auth-service on the feature/oauth-v2 branch"
Assistant: create_session("auth-service-123", "oauth-refactor", "feature/oauth-v2", null, false, "Refactoring OAuth implementation for v2 API")
```

### read_session_context
**Purpose**: Get detailed session information and current activity

```typescript
interface ReadSessionContextParams {
  session_id: string;
  include_summary?: boolean;      // Include AI-generated summary via claude -p
  include_activity?: boolean;     // Include recent activity details
  activity_hours?: number;        // Hours of activity to include
  include_files?: boolean;        // Include currently open/accessed files
}

interface ReadSessionContextResponse {
  session: {
    id: string;
    name: string;
    project_name: string;
    status: SessionStatus;
    created_at: string;
    last_activity: string;
    working_directory: string;
    git_context: GitContext;
  };
  current_context: {
    task_summary: string;         // "Debugging OAuth token expiration logic"
    open_files: string[];        // Files currently being worked on
    confidence_level: number;    // 0-1, how well session is progressing
    apparent_blockers: string[]; // Detected issues or stuck points
  };
  recent_activity: Activity[];
  intelligence_summary: string;  // claude -p generated summary
  related_sessions: RelatedSession[]; // Other sessions working on similar code
  recommendations: string[];     // Suggested actions or next steps
}
```

### update_session_status
**Purpose**: Modify session state and metadata

```typescript
interface UpdateSessionStatusParams {
  session_id: string;
  updates: {
    status?: SessionStatus;
    name?: string;
    description?: string;
    tags?: string[];
    notes?: string;              // Human notes about session progress
  };
}
```

### list_project_sessions
**Purpose**: Overview of all sessions for a project

```typescript
interface ListProjectSessionsParams {
  project_id: string;
  status_filter?: SessionStatus; // Filter by status
  include_inactive?: boolean;    // Include terminated/archived sessions
}

interface ListProjectSessionsResponse {
  active_sessions: SessionSummary[];
  idle_sessions: SessionSummary[];
  recent_sessions: SessionSummary[]; // Recently terminated
  session_insights: {
    most_productive: string;     // Session name with most activity
    longest_running: string;     // Session running longest
    coordination_opportunities: CoordinationSuggestion[];
  };
}
```

### terminate_session
**Purpose**: Safely shut down a Claude Code session

```typescript
interface TerminateSessionParams {
  session_id: string;
  reason?: string;               // Why session is being terminated
  save_context?: boolean;        // Preserve context for future restoration
}

interface TerminateSessionResponse {
  success: boolean;
  final_status: {
    duration: string;            // "2h 34m"
    total_operations: number;
    files_accessed: number;
    final_task: string;         // Last known task
  };
  context_snapshot_id?: string; // If save_context was true
}
```

## Intelligence & Analysis Tools

### analyze_current_activity
**Purpose**: Real-time analysis of what's happening across sessions

```typescript
interface AnalyzeCurrentActivityParams {
  session_id?: string;           // Specific session, or all if omitted
  timeframe?: string;            // "15m", "1h", "today" (default: "15m")
  include_predictions?: boolean; // Include predictive analysis
}

interface AnalyzeCurrentActivityResponse {
  current_focus: {
    primary_task: string;        // High-level description
    files_in_focus: string[];   // Most accessed files
    complexity_level: 'low' | 'medium' | 'high';
    progress_confidence: number; // 0-1
  };
  session_health: {
    status: 'healthy' | 'stuck' | 'unfocused' | 'needs_guidance';
    indicators: string[];       // Evidence for the status
    recommendations: string[];  // Suggested interventions
  };
  cross_session_insights?: {
    coordination_opportunities: string[];
    potential_conflicts: string[];
    knowledge_sharing: string[];
  };
}
```

### generate_task_summary
**Purpose**: AI-powered summary of session activity using claude -p

```typescript
interface GenerateTaskSummaryParams {
  session_id: string;
  context_depth?: 'shallow' | 'medium' | 'deep'; // How much context to analyze
  focus?: 'current' | 'progress' | 'blockers';   // What to emphasize
}

interface GenerateTaskSummaryResponse {
  summary: {
    current_task: string;        // "Debugging OAuth middleware token handling"
    key_activities: string[];    // Recent significant actions
    progress_assessment: string; // How well things are going
    identified_blockers: string[]; // Current challenges
    suggested_next_steps: string[]; // Recommended actions
  };
  context_analysis: {
    files_examined: number;
    code_patterns_identified: string[];
    external_dependencies: string[];
    testing_status: string;
  };
  confidence_metrics: {
    summary_confidence: number;  // 0-1, how reliable this summary is
    activity_recency: string;   // "5 minutes ago"
    data_completeness: number;  // 0-1, how much data was available
  };
}
```

### track_file_access
**Purpose**: Log when Claude accesses files for context building

```typescript
interface TrackFileAccessParams {
  session_id: string;
  file_path: string;
  access_type: 'read' | 'write' | 'search' | 'reference';
  context?: string;              // Why the file was accessed
}
```

### identify_coordination_opportunities
**Purpose**: Find ways multiple sessions could work together better

```typescript
interface IdentifyCoordinationOpportunitiesParams {
  project_id: string;
  timeframe?: string;            // How far back to analyze
}

interface IdentifyCoordinationOpportunitiesResponse {
  opportunities: {
    type: 'shared_files' | 'related_concepts' | 'complementary_work';
    sessions_involved: string[];
    description: string;
    potential_benefit: string;
    suggested_action: string;
  }[];
  conflicts: {
    type: 'file_collision' | 'contradictory_approaches';
    sessions_involved: string[];
    description: string;
    risk_level: 'low' | 'medium' | 'high';
    suggested_resolution: string;
  }[];
}
```

## Context Management Tools

### preserve_session_context
**Purpose**: Save session state for later restoration

```typescript
interface PreserveSessionContextParams {
  session_id: string;
  snapshot_name?: string;        // Human-friendly name
  include_files?: boolean;       // Include file contents (default: false)
}

interface PreserveSessionContextResponse {
  snapshot_id: string;
  snapshot_name: string;
  captured_at: string;
  context_size: {
    activities: number;
    files_tracked: number;
    conversation_length: number;
  };
}
```

### restore_session_context
**Purpose**: Restore previously saved context to a session

```typescript
interface RestoreSessionContextParams {
  session_id: string;
  snapshot_id: string;
  restore_mode: 'full' | 'files_only' | 'summary_only';
}
```

### share_context_between_sessions
**Purpose**: Transfer relevant context from one session to another

```typescript
interface ShareContextBetweenSessionsParams {
  from_session_id: string;
  to_session_id: string;
  context_types: ('files' | 'concepts' | 'blockers' | 'solutions')[];
  relevance_threshold?: number;  // 0-1, how related content must be
}
```

## Usage Patterns

### Orchestrator Conversation Flow
```
Human: "What's happening with my projects?"

Orchestrator Claude uses:
1. list_active_projects() - Get overview
2. For each active project:
   - read_project_status(project_id, true, true) - Get detailed status
   - analyze_current_activity() - Check what's happening now
3. identify_coordination_opportunities() - Look for cross-project insights

Response: "You have 3 active projects. auth-service has 2 active sessions - Marcel is debugging OAuth middleware and seems stuck (45min no progress), while Sandra just finished component refactoring. ui-library has 1 active session working steadily. db-migration is paused. 

I notice Marcel's OAuth work might help Sandra's auth components - want me to share context between their sessions?"
```

### Session Management Flow
```
Human: "Start a new session for OAuth refactoring"

Orchestrator Claude:
1. list_active_projects() - Determine which project
2. create_session(project_id, "oauth-refactor", branch, options)
3. track_file_access() - As files are opened
4. analyze_current_activity() - Periodic health checks
5. generate_task_summary() - When human asks for updates
```

## Tool Implementation Notes

### Error Handling
All tools return consistent error structures:
```typescript
interface ToolError {
  error: string;                 // Human-readable error message
  error_code: string;           // Machine-readable error type
  details?: any;                // Additional error context
  suggestions?: string[];       // How to fix the error
}
```

### Response Formatting
All tools return rich data suitable for conversational responses:
- Include related context automatically
- Provide insights and recommendations where relevant
- Format data for easy natural language summarization
- Include metadata for confidence and freshness