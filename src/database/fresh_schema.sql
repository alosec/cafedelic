-- Cafedelic Fresh Database Schema
-- Focused on project/session tracking with tmux integration

PRAGMA foreign_keys = ON;

-- Projects table - track Claude Code projects
CREATE TABLE projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    short_id TEXT UNIQUE NOT NULL, -- p1, p2, p3 for easy reference
    name TEXT NOT NULL,
    path TEXT NOT NULL UNIQUE,
    description TEXT,
    status TEXT DEFAULT 'active' CHECK (status IN ('active', 'archived', 'paused')),
    has_git BOOLEAN DEFAULT FALSE,
    git_remote_url TEXT,
    discovered_from TEXT DEFAULT 'manual' CHECK (discovered_from IN ('manual', 'claude_sessions', 'filesystem_scan')),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Project scan paths - user-defined directories to scan for projects
CREATE TABLE project_scan_paths (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    path TEXT NOT NULL UNIQUE,
    name TEXT,
    auto_scan BOOLEAN DEFAULT TRUE,
    max_depth INTEGER DEFAULT 2,
    last_scan DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Sessions table - track Claude Code sessions with tmux integration  
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    short_id TEXT UNIQUE NOT NULL, -- s1, s2, s3 for easy reference
    project_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    claude_session_uuid TEXT UNIQUE, -- Real UUID from Claude Code JSONL files
    tmux_session_name TEXT, -- tmux session name for this Claude session
    tmux_window_id TEXT, -- tmux window ID
    tmux_pane_id TEXT, -- tmux pane ID
    status TEXT DEFAULT 'available' CHECK (status IN ('active', 'available', 'detached', 'stuck', 'inactive')),
    task_description TEXT,
    conversation_turns INTEGER DEFAULT 0,
    total_cost_usd REAL DEFAULT 0.0,
    jsonl_file_path TEXT, -- Path to the Claude session JSONL file
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_activity DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
);

-- Activity log for session tracking
CREATE TABLE activities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    activity_type TEXT NOT NULL, -- 'file_read', 'file_edit', 'command', 'chat', 'resume', 'pause'
    description TEXT NOT NULL,
    file_path TEXT, -- optional, for file operations
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES sessions (id) ON DELETE CASCADE
);

-- File context tracking
CREATE TABLE file_context (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    file_path TEXT NOT NULL,
    context_type TEXT DEFAULT 'active' CHECK (context_type IN ('active', 'recent', 'referenced')),
    last_accessed DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES sessions (id) ON DELETE CASCADE,
    UNIQUE(session_id, file_path)
);

-- Indexes for performance
CREATE INDEX idx_projects_short_id ON projects(short_id);
CREATE INDEX idx_projects_path ON projects(path);
CREATE INDEX idx_projects_discovered_from ON projects(discovered_from);
CREATE INDEX idx_project_scan_paths_path ON project_scan_paths(path);
CREATE INDEX idx_sessions_short_id ON sessions(short_id);
CREATE INDEX idx_sessions_claude_uuid ON sessions(claude_session_uuid);
CREATE INDEX idx_sessions_project_id ON sessions(project_id);
CREATE INDEX idx_sessions_status ON sessions(status);
CREATE INDEX idx_sessions_last_activity ON sessions(last_activity);
CREATE INDEX idx_activities_session_id ON activities(session_id);
CREATE INDEX idx_activities_created_at ON activities(created_at);
CREATE INDEX idx_file_context_session_id ON file_context(session_id);

-- Update triggers
CREATE TRIGGER update_projects_timestamp 
    AFTER UPDATE ON projects
    BEGIN
        UPDATE projects SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
    END;

CREATE TRIGGER update_sessions_timestamp 
    AFTER UPDATE ON sessions
    BEGIN
        UPDATE sessions SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
    END;

-- Views for common queries
CREATE VIEW active_sessions AS
SELECT 
    s.short_id,
    s.name,
    p.name as project_name,
    p.short_id as project_short_id,
    s.status,
    s.task_description,
    s.tmux_session_name,
    s.last_activity,
    s.claude_session_uuid,
    (SELECT COUNT(*) FROM activities a WHERE a.session_id = s.id AND a.created_at > datetime('now', '-1 hour')) as recent_activity_count
FROM sessions s
JOIN projects p ON s.project_id = p.id
WHERE s.status != 'archived'
ORDER BY s.last_activity DESC;

CREATE VIEW project_summary AS
SELECT 
    p.short_id,
    p.name,
    p.path,
    p.status,
    COUNT(s.id) as session_count,
    COUNT(CASE WHEN s.status = 'active' THEN 1 END) as active_sessions,
    MAX(s.last_activity) as last_session_activity
FROM projects p
LEFT JOIN sessions s ON p.id = s.project_id
GROUP BY p.id
ORDER BY last_session_activity DESC;