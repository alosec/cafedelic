-- Cafedelic Database Schema - Simple extensions for Claude Code integration
-- Read-only access to Claude Code's ~/.claude/__store.db
-- Minimal Cafedelic-specific data in our own database

PRAGMA foreign_keys = ON;

-- Projects discovered from Claude Code sessions
CREATE TABLE projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    short_id TEXT UNIQUE NOT NULL, -- p1, p2, p3
    name TEXT NOT NULL,
    path TEXT NOT NULL UNIQUE,
    normalized_path TEXT NOT NULL, -- Claude Code's -home-alex-code-cafedelic format
    status TEXT DEFAULT 'active' CHECK (status IN ('active', 'archived')),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Cafedelic session extensions (links to Claude Code session UUIDs)
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    short_id TEXT UNIQUE NOT NULL, -- s1, s2, s3
    project_id INTEGER NOT NULL,
    claude_session_uuid TEXT UNIQUE NOT NULL, -- Links to Claude Code's session_id
    name TEXT NOT NULL,
    tmux_session_name TEXT,
    status TEXT DEFAULT 'available' CHECK (status IN ('active', 'available', 'detached', 'stuck')),
    task_description TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
);

-- Simple activity log for Cafedelic-specific events
CREATE TABLE activities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER,
    activity_type TEXT NOT NULL, -- 'scan', 'load', 'open', 'delegate'
    description TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES sessions (id) ON DELETE SET NULL
);

-- Indexes for performance
CREATE INDEX idx_projects_path ON projects(path);
CREATE INDEX idx_projects_normalized_path ON projects(normalized_path);
CREATE INDEX idx_sessions_claude_uuid ON sessions(claude_session_uuid);
CREATE INDEX idx_sessions_project_id ON sessions(project_id);
CREATE INDEX idx_activities_session_id ON activities(session_id);

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

-- View for active sessions with project info
CREATE VIEW active_sessions AS
SELECT 
    s.short_id,
    s.name,
    s.claude_session_uuid,
    s.status,
    s.task_description,
    s.tmux_session_name,
    p.name as project_name,
    p.path as project_path,
    p.short_id as project_short_id
FROM sessions s
JOIN projects p ON s.project_id = p.id
WHERE s.status != 'archived'
ORDER BY s.updated_at DESC;