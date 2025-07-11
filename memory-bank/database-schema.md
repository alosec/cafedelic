# Database Schema

## Overview

The SQLite database is the intelligence core of cafedelic, storing all project, session, and activity data in a structured format that enables rich querying, analysis, and coordination across multiple Claude Code sessions.

## Core Entities

### Projects Table
```sql
CREATE TABLE projects (
  id TEXT PRIMARY KEY,              -- UUID for project
  name TEXT NOT NULL,               -- Human-friendly name
  repo_path TEXT NOT NULL UNIQUE,  -- Absolute path to git repository
  description TEXT,                 -- Optional project description
  status TEXT NOT NULL DEFAULT 'active', -- 'active', 'archived', 'paused'
  default_branch TEXT DEFAULT 'main',     -- Default branch for new sessions
  created_at DATETIME NOT NULL,    -- Project creation timestamp
  last_activity DATETIME,          -- Last activity across all sessions
  metadata TEXT,                   -- JSON: additional project settings
  
  CONSTRAINT projects_status_check 
    CHECK (status IN ('active', 'archived', 'paused'))
);

-- Indexes for common queries
CREATE INDEX idx_projects_status ON projects(status);
CREATE INDEX idx_projects_last_activity ON projects(last_activity);
CREATE UNIQUE INDEX idx_projects_repo_path ON projects(repo_path);
```

### Sessions Table
```sql
CREATE TABLE sessions (
  id TEXT PRIMARY KEY,              -- UUID for session
  project_id TEXT NOT NULL,        -- Foreign key to projects
  name TEXT NOT NULL,               -- Human-friendly session name
  assistant_type TEXT DEFAULT 'claude-code', -- 'claude-code', 'cline', etc.
  process_id INTEGER,              -- PID of Claude Code process
  working_directory TEXT NOT NULL, -- Absolute path where session runs
  branch TEXT NOT NULL,            -- Git branch for this session
  worktree TEXT,                   -- Optional worktree path
  yolo_mode BOOLEAN DEFAULT FALSE, -- Whether yolo mode is enabled
  status TEXT NOT NULL DEFAULT 'starting', -- Session lifecycle status
  description TEXT,                -- Session purpose/goals
  tags TEXT,                       -- JSON array: categorization tags
  created_at DATETIME NOT NULL,    -- Session creation time
  started_at DATETIME,             -- When Claude Code actually started
  last_activity DATETIME,          -- Last recorded activity
  terminated_at DATETIME,          -- When session ended
  termination_reason TEXT,         -- Why session ended
  
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
  
  CONSTRAINT sessions_status_check 
    CHECK (status IN ('starting', 'active', 'idle', 'error', 'terminated')),
  CONSTRAINT sessions_project_name_unique 
    UNIQUE (project_id, name) -- Session names unique within project
);

-- Indexes for session queries
CREATE INDEX idx_sessions_project_id ON sessions(project_id);
CREATE INDEX idx_sessions_status ON sessions(status);
CREATE INDEX idx_sessions_last_activity ON sessions(last_activity);
CREATE INDEX idx_sessions_assistant_type ON sessions(assistant_type);
```

### Activities Table
```sql
CREATE TABLE activities (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT NOT NULL,        -- Foreign key to sessions
  type TEXT NOT NULL,              -- Type of activity
  content TEXT NOT NULL,           -- Activity description/details
  files_affected TEXT,             -- JSON array of file paths
  timestamp DATETIME NOT NULL,     -- When activity occurred
  duration_ms INTEGER,             -- How long activity took
  context_tags TEXT,              -- JSON array: extracted concepts/tags
  metadata TEXT,                   -- JSON: additional activity data
  
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
  
  CONSTRAINT activities_type_check 
    CHECK (type IN (
      'file_read', 'file_write', 'file_search', 'file_delete',
      'directory_list', 'git_operation', 'test_run', 'build_run',
      'search_code', 'search_web', 'ask_question', 'provide_answer',
      'task_start', 'task_complete', 'error_encountered', 'session_start',
      'session_pause', 'session_resume', 'context_switch', 'analysis',
      'stuck_indicator', 'progress_milestone'
    ))
);

-- Indexes for activity analysis
CREATE INDEX idx_activities_session_id ON activities(session_id);
CREATE INDEX idx_activities_type ON activities(type);
CREATE INDEX idx_activities_timestamp ON activities(timestamp);
CREATE INDEX idx_activities_session_timestamp ON activities(session_id, timestamp);

-- Full-text search on activity content
CREATE VIRTUAL TABLE activities_fts USING fts5(
  content, context_tags, 
  content='activities', content_rowid='id'
);

-- Trigger to keep FTS in sync
CREATE TRIGGER activities_fts_insert AFTER INSERT ON activities
BEGIN
  INSERT INTO activities_fts(rowid, content, context_tags) 
  VALUES (new.id, new.content, new.context_tags);
END;

CREATE TRIGGER activities_fts_update AFTER UPDATE ON activities
BEGIN
  UPDATE activities_fts 
  SET content = new.content, context_tags = new.context_tags
  WHERE rowid = new.id;
END;

CREATE TRIGGER activities_fts_delete AFTER DELETE ON activities
BEGIN
  DELETE FROM activities_fts WHERE rowid = old.id;
END;
```

### Session Context Table
```sql
CREATE TABLE session_context (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT NOT NULL,        -- Foreign key to sessions
  current_task TEXT,               -- High-level current task description
  open_files TEXT,                 -- JSON array of currently open files
  progress_status TEXT,            -- Current progress assessment
  confidence_level REAL,           -- 0-1: how well session is progressing
  apparent_blockers TEXT,          -- JSON array of detected issues
  context_needs TEXT,              -- JSON array of needed context
  intelligence_summary TEXT,       -- claude -p generated summary
  last_analysis DATETIME NOT NULL, -- When context was last updated
  analysis_source TEXT DEFAULT 'automatic', -- 'automatic', 'manual', 'claude-p'
  
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
  
  CONSTRAINT context_confidence_check 
    CHECK (confidence_level >= 0 AND confidence_level <= 1)
);

-- Only one current context per session
CREATE UNIQUE INDEX idx_session_context_current ON session_context(session_id);
CREATE INDEX idx_session_context_last_analysis ON session_context(last_analysis);
```

### Context Snapshots Table
```sql
CREATE TABLE context_snapshots (
  id TEXT PRIMARY KEY,              -- UUID for snapshot
  session_id TEXT NOT NULL,        -- Session this snapshot belongs to
  name TEXT,                       -- Human-friendly snapshot name
  created_at DATETIME NOT NULL,    -- When snapshot was created
  context_data TEXT NOT NULL,      -- JSON: complete context state
  activity_count INTEGER,          -- Number of activities captured
  file_count INTEGER,              -- Number of files tracked
  conversation_length INTEGER,     -- Length of conversation captured
  
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX idx_context_snapshots_session ON context_snapshots(session_id);
CREATE INDEX idx_context_snapshots_created ON context_snapshots(created_at);
```

### Project Insights Table
```sql
CREATE TABLE project_insights (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  project_id TEXT NOT NULL,        -- Project this insight applies to
  insight_type TEXT NOT NULL,      -- Type of insight
  title TEXT NOT NULL,             -- Short insight title
  description TEXT NOT NULL,       -- Detailed insight description
  confidence REAL NOT NULL,        -- 0-1: confidence in insight
  evidence TEXT,                   -- JSON: supporting evidence
  recommendations TEXT,            -- JSON: suggested actions
  relevance_score REAL,           -- 0-1: how important this insight is
  generated_at DATETIME NOT NULL, -- When insight was generated
  expires_at DATETIME,            -- When insight becomes stale
  status TEXT DEFAULT 'active',   -- 'active', 'acted_on', 'dismissed'
  
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
  
  CONSTRAINT insights_type_check 
    CHECK (insight_type IN (
      'coordination_opportunity', 'potential_conflict', 'knowledge_gap',
      'pattern_identified', 'efficiency_suggestion', 'resource_optimization',
      'session_health', 'progress_blocker', 'technical_debt'
    )),
  CONSTRAINT insights_confidence_check 
    CHECK (confidence >= 0 AND confidence <= 1),
  CONSTRAINT insights_relevance_check 
    CHECK (relevance_score >= 0 AND relevance_score <= 1)
);

CREATE INDEX idx_project_insights_project ON project_insights(project_id);
CREATE INDEX idx_project_insights_type ON project_insights(insight_type);
CREATE INDEX idx_project_insights_relevance ON project_insights(relevance_score);
CREATE INDEX idx_project_insights_generated ON project_insights(generated_at);
```

## Derived Views

### Active Sessions View
```sql
CREATE VIEW v_active_sessions AS
SELECT 
  s.id,
  s.name,
  s.project_id,
  p.name as project_name,
  s.status,
  s.branch,
  s.last_activity,
  sc.current_task,
  sc.confidence_level,
  julianday('now') - julianday(s.last_activity) as hours_since_activity
FROM sessions s
JOIN projects p ON s.project_id = p.id
LEFT JOIN session_context sc ON s.id = sc.session_id
WHERE s.status IN ('active', 'idle')
ORDER BY s.last_activity DESC;
```

### Project Activity Summary View
```sql
CREATE VIEW v_project_activity_summary AS
SELECT 
  p.id as project_id,
  p.name as project_name,
  COUNT(DISTINCT s.id) as total_sessions,
  COUNT(DISTINCT CASE WHEN s.status = 'active' THEN s.id END) as active_sessions,
  COUNT(DISTINCT CASE WHEN s.status = 'idle' THEN s.id END) as idle_sessions,
  COUNT(a.id) as total_activities,
  COUNT(CASE WHEN a.timestamp > datetime('now', '-24 hours') THEN a.id END) as activities_today,
  MAX(a.timestamp) as last_activity,
  GROUP_CONCAT(DISTINCT s.name) as session_names
FROM projects p
LEFT JOIN sessions s ON p.id = s.project_id
LEFT JOIN activities a ON s.id = a.session_id
WHERE p.status = 'active'
GROUP BY p.id, p.name;
```

### Session Health View
```sql
CREATE VIEW v_session_health AS
SELECT 
  s.id,
  s.name,
  s.project_id,
  p.name as project_name,
  s.status,
  sc.confidence_level,
  sc.current_task,
  COUNT(a.id) as recent_activities,
  COUNT(CASE WHEN a.type LIKE '%error%' THEN a.id END) as recent_errors,
  MAX(a.timestamp) as last_activity_time,
  julianday('now') - julianday(MAX(a.timestamp)) as hours_idle,
  CASE 
    WHEN sc.confidence_level < 0.3 THEN 'struggling'
    WHEN julianday('now') - julianday(MAX(a.timestamp)) > 0.5 THEN 'idle'
    WHEN COUNT(a.id) = 0 THEN 'inactive'
    ELSE 'healthy'
  END as health_status
FROM sessions s
JOIN projects p ON s.project_id = p.id
LEFT JOIN session_context sc ON s.id = sc.session_id
LEFT JOIN activities a ON s.id = a.session_id AND a.timestamp > datetime('now', '-1 hour')
WHERE s.status IN ('active', 'idle')
GROUP BY s.id, s.name, s.project_id, p.name, s.status, sc.confidence_level, sc.current_task;
```

## Database Triggers

### Auto-Update Activity Timestamps
```sql
-- Update session last_activity when new activity is recorded
CREATE TRIGGER update_session_activity 
AFTER INSERT ON activities
BEGIN
  UPDATE sessions 
  SET last_activity = NEW.timestamp
  WHERE id = NEW.session_id;
  
  -- Also update project last_activity
  UPDATE projects 
  SET last_activity = NEW.timestamp
  WHERE id = (
    SELECT project_id FROM sessions WHERE id = NEW.session_id
  );
END;
```

### Auto-Archive Idle Sessions
```sql
-- Mark sessions as idle after 4 hours of inactivity
CREATE TRIGGER auto_idle_sessions
AFTER UPDATE OF last_activity ON sessions
WHEN NEW.status = 'active' 
  AND julianday('now') - julianday(NEW.last_activity) > 0.167 -- 4 hours
BEGIN
  UPDATE sessions
  SET status = 'idle'
  WHERE id = NEW.id;
END;
```

### Generate Insights on Activity Patterns
```sql
-- Trigger insight generation when interesting patterns emerge
CREATE TRIGGER generate_activity_insights
AFTER INSERT ON activities
WHEN NEW.type IN ('stuck_indicator', 'error_encountered', 'task_complete')
BEGIN
  -- This would call a function to analyze patterns and generate insights
  -- Implementation would depend on the application layer
  INSERT INTO project_insights (
    project_id, insight_type, title, description, confidence, 
    generated_at, relevance_score
  )
  SELECT 
    s.project_id,
    'session_health',
    'Session ' || s.name || ' showing ' || NEW.type,
    'Activity pattern detected: ' || NEW.content,
    0.7,
    datetime('now'),
    0.8
  FROM sessions s
  WHERE s.id = NEW.session_id;
END;
```

## Common Queries

### Find Related Sessions
```sql
-- Sessions working on similar files
SELECT DISTINCT
  s1.name as session1,
  s2.name as session2,
  COUNT(*) as shared_files
FROM activities a1
JOIN activities a2 ON json_extract(a1.files_affected, '$[0]') = json_extract(a2.files_affected, '$[0]')
JOIN sessions s1 ON a1.session_id = s1.id
JOIN sessions s2 ON a2.session_id = s2.id
WHERE s1.id != s2.id
  AND s1.project_id = s2.project_id
  AND a1.timestamp > datetime('now', '-24 hours')
  AND a2.timestamp > datetime('now', '-24 hours')
GROUP BY s1.id, s2.id
HAVING COUNT(*) > 2;
```

### Session Activity Patterns
```sql
-- Analyze activity velocity for a session
SELECT 
  date(timestamp) as day,
  COUNT(*) as activities,
  COUNT(DISTINCT type) as activity_types,
  COUNT(CASE WHEN type LIKE '%error%' THEN 1 END) as errors,
  COUNT(CASE WHEN type = 'file_write' THEN 1 END) as file_writes
FROM activities
WHERE session_id = ?
  AND timestamp > datetime('now', '-7 days')
GROUP BY date(timestamp)
ORDER BY day;
```

### Cross-Project Intelligence
```sql
-- Find concepts mentioned across multiple projects
SELECT 
  json_extract(context_tags, '$[0]') as concept,
  COUNT(DISTINCT s.project_id) as project_count,
  COUNT(*) as mention_count,
  GROUP_CONCAT(DISTINCT p.name) as projects
FROM activities a
JOIN sessions s ON a.session_id = s.id
JOIN projects p ON s.project_id = p.id
WHERE json_valid(a.context_tags)
  AND a.timestamp > datetime('now', '-30 days')
GROUP BY concept
HAVING project_count > 1
ORDER BY mention_count DESC;
```

## Data Migration & Versioning

### Schema Version Table
```sql
CREATE TABLE schema_version (
  version INTEGER PRIMARY KEY,
  applied_at DATETIME NOT NULL,
  description TEXT
);

-- Initial version
INSERT INTO schema_version (version, applied_at, description)
VALUES (1, datetime('now'), 'Initial cafedelic schema');
```

### Migration Framework
```sql
-- Example migration for adding new activity types
-- Version 2: Add new activity types
ALTER TABLE activities ADD COLUMN priority TEXT 
  CHECK (priority IN ('low', 'medium', 'high', 'critical'));

INSERT INTO schema_version (version, applied_at, description)
VALUES (2, datetime('now'), 'Add activity priority levels');
```

This schema provides a robust foundation for the intelligence layer, enabling rich analysis, cross-session coordination, and comprehensive project management through structured data and powerful querying capabilities.