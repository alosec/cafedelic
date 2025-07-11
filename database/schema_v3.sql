-- Cafedelic Intelligence Platform - Database Schema V3
-- Enhanced schema for comprehensive AI session intelligence and project management
-- Migrates from V2 while preserving existing data (2778 activities, 30 sessions)

-- Schema versioning and migration tracking
CREATE TABLE IF NOT EXISTS schema_migrations (
    id INTEGER PRIMARY KEY,
    version TEXT NOT NULL UNIQUE,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    description TEXT
);

-- Core project management
CREATE TABLE projects (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    path TEXT NOT NULL UNIQUE,
    status TEXT DEFAULT 'active' CHECK (status IN ('active', 'archived', 'paused')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_activity TIMESTAMP,
    config_hash TEXT,
    metadata JSON DEFAULT '{}',
    
    -- Intelligence tracking
    total_sessions INTEGER DEFAULT 0,
    total_activities INTEGER DEFAULT 0,
    health_score REAL DEFAULT 0.0,
    
    -- Git integration
    git_repo_url TEXT,
    default_branch TEXT DEFAULT 'main',
    
    INDEX idx_projects_status (status),
    INDEX idx_projects_last_activity (last_activity),
    INDEX idx_projects_path (path)
);

-- Enhanced session tracking with comprehensive metadata
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY,
    project_id INTEGER REFERENCES projects(id) ON DELETE CASCADE,
    
    -- Session identification
    name TEXT,
    process_id INTEGER,
    session_uuid TEXT UNIQUE, -- For external tracking
    
    -- Status and lifecycle
    status TEXT DEFAULT 'starting' CHECK (status IN ('starting', 'active', 'idle', 'paused', 'stuck', 'terminated', 'failed')),
    assistant_type TEXT DEFAULT 'claude-code',
    
    -- Context information
    working_directory TEXT,
    git_branch TEXT,
    git_commit_hash TEXT,
    
    -- UI state persistence
    tab_order INTEGER DEFAULT 0,
    ui_state JSON DEFAULT '{}',
    
    -- Timestamps
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    started_at TIMESTAMP,
    last_activity TIMESTAMP,
    ended_at TIMESTAMP,
    
    -- Intelligence metrics
    activity_count INTEGER DEFAULT 0,
    file_access_count INTEGER DEFAULT 0,
    health_score REAL DEFAULT 0.0,
    productivity_score REAL DEFAULT 0.0,
    
    INDEX idx_sessions_project (project_id),
    INDEX idx_sessions_status (status),
    INDEX idx_sessions_last_activity (last_activity),
    INDEX idx_sessions_process_id (process_id)
);

-- Session context and AI-generated intelligence
CREATE TABLE session_context (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id) ON DELETE CASCADE,
    
    -- Context categorization
    context_type TEXT NOT NULL CHECK (context_type IN ('task_summary', 'progress_update', 'claude_p_analysis', 'error_analysis', 'coordination_context')),
    
    -- AI-generated content
    current_task TEXT,
    task_description TEXT,
    progress_summary TEXT,
    files_in_context JSON DEFAULT '[]',
    key_insights JSON DEFAULT '[]',
    
    -- Quality metrics
    confidence_score REAL DEFAULT 0.0,
    relevance_score REAL DEFAULT 0.0,
    
    -- Metadata
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    generated_by TEXT DEFAULT 'system',
    metadata JSON DEFAULT '{}',
    
    INDEX idx_session_context_session (session_id),
    INDEX idx_session_context_type (context_type),
    INDEX idx_session_context_generated (generated_at)
);

-- Enhanced file access tracking with rich metadata
CREATE TABLE file_access (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id) ON DELETE CASCADE,
    
    -- File information
    file_path TEXT NOT NULL,
    file_name TEXT GENERATED ALWAYS AS (
        CASE 
            WHEN file_path LIKE '%/%' THEN substr(file_path, length(file_path) - instr(reverse(file_path), '/') + 2)
            ELSE file_path 
        END
    ) STORED,
    file_extension TEXT GENERATED ALWAYS AS (
        CASE 
            WHEN file_path LIKE '%.%' THEN substr(file_path, length(file_path) - instr(reverse(file_path), '.') + 2)
            ELSE ''
        END
    ) STORED,
    
    -- Access details
    access_type TEXT NOT NULL CHECK (access_type IN ('read', 'write', 'create', 'delete', 'execute', 'reference')),
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Content context
    content_preview TEXT,
    line_numbers TEXT, -- JSON array of accessed line ranges
    modification_type TEXT, -- 'addition', 'deletion', 'modification', 'refactor'
    
    -- Size and change metrics
    file_size INTEGER,
    lines_added INTEGER DEFAULT 0,
    lines_deleted INTEGER DEFAULT 0,
    lines_modified INTEGER DEFAULT 0,
    
    -- Intelligence metadata
    importance_score REAL DEFAULT 0.0,
    complexity_score REAL DEFAULT 0.0,
    metadata JSON DEFAULT '{}',
    
    INDEX idx_file_access_session (session_id),
    INDEX idx_file_access_path (file_path),
    INDEX idx_file_access_type (access_type),
    INDEX idx_file_access_timestamp (timestamp),
    INDEX idx_file_access_name (file_name)
);

-- Enhanced activity stream with intelligent categorization
CREATE TABLE activities (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id) ON DELETE CASCADE,
    
    -- Activity classification
    activity_type TEXT NOT NULL CHECK (activity_type IN (
        'file_operation', 'code_analysis', 'execution', 'git_operation', 
        'search', 'navigation', 'edit', 'test', 'debug', 'research',
        'coordination', 'planning', 'review', 'refactor', 'documentation'
    )),
    
    -- Content and processing
    raw_content TEXT,
    processed_content TEXT,
    summary TEXT,
    
    -- Context and relationships
    related_files JSON DEFAULT '[]',
    related_sessions JSON DEFAULT '[]',
    parent_activity_id INTEGER REFERENCES activities(id),
    
    -- Timing and duration
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    duration_ms INTEGER DEFAULT 0,
    
    -- Intelligence metrics
    importance_score REAL DEFAULT 0.0,
    productivity_score REAL DEFAULT 0.0,
    complexity_score REAL DEFAULT 0.0,
    
    -- Metadata and context
    metadata JSON DEFAULT '{}',
    tags JSON DEFAULT '[]',
    
    INDEX idx_activities_session (session_id),
    INDEX idx_activities_type (activity_type),
    INDEX idx_activities_timestamp (timestamp),
    INDEX idx_activities_importance (importance_score),
    INDEX idx_activities_parent (parent_activity_id)
);

-- Cross-session intelligence and coordination opportunities
CREATE TABLE coordination_opportunities (
    id INTEGER PRIMARY KEY,
    
    -- Sessions involved
    session_1_id INTEGER REFERENCES sessions(id) ON DELETE CASCADE,
    session_2_id INTEGER REFERENCES sessions(id) ON DELETE CASCADE,
    project_id INTEGER REFERENCES projects(id) ON DELETE CASCADE,
    
    -- Opportunity classification
    opportunity_type TEXT NOT NULL CHECK (opportunity_type IN (
        'file_conflict', 'shared_context', 'knowledge_transfer', 
        'merge_opportunity', 'coordination_needed', 'resource_sharing',
        'duplicate_work', 'dependency_resolution'
    )),
    
    -- Description and analysis
    title TEXT NOT NULL,
    description TEXT,
    recommendation TEXT,
    impact_assessment TEXT,
    
    -- Affected resources
    shared_files JSON DEFAULT '[]',
    conflicting_changes JSON DEFAULT '[]',
    
    -- Scoring and priority
    confidence_score REAL DEFAULT 0.0,
    priority_score REAL DEFAULT 0.0,
    potential_impact TEXT CHECK (potential_impact IN ('low', 'medium', 'high', 'critical')),
    
    -- Status tracking
    status TEXT DEFAULT 'identified' CHECK (status IN ('identified', 'acknowledged', 'in_progress', 'resolved', 'dismissed')),
    resolution_notes TEXT,
    
    -- Timestamps
    identified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    acknowledged_at TIMESTAMP,
    resolved_at TIMESTAMP,
    
    -- Metadata
    metadata JSON DEFAULT '{}',
    
    INDEX idx_coordination_sessions (session_1_id, session_2_id),
    INDEX idx_coordination_project (project_id),
    INDEX idx_coordination_type (opportunity_type),
    INDEX idx_coordination_status (status),
    INDEX idx_coordination_priority (priority_score)
);

-- Comprehensive health monitoring and metrics
CREATE TABLE session_health (
    id INTEGER PRIMARY KEY,
    session_id INTEGER REFERENCES sessions(id) ON DELETE CASCADE,
    
    -- Health metric categories
    health_metric TEXT NOT NULL CHECK (health_metric IN (
        'responsiveness', 'activity_rate', 'error_frequency', 'progress_velocity',
        'memory_usage', 'cpu_usage', 'file_churn', 'context_stability',
        'coordination_health', 'productivity_trend'
    )),
    
    -- Metric values and context
    value REAL NOT NULL,
    unit TEXT, -- 'ms', 'percentage', 'count', 'rate', etc.
    threshold_min REAL,
    threshold_max REAL,
    
    -- Status assessment
    threshold_status TEXT CHECK (threshold_status IN ('healthy', 'warning', 'critical')),
    status_reason TEXT,
    
    -- Temporal data
    measured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    measurement_window TEXT, -- '5min', '1hour', '24hour', etc.
    
    -- Trend analysis
    trend_direction TEXT CHECK (trend_direction IN ('improving', 'stable', 'declining')),
    trend_confidence REAL DEFAULT 0.0,
    
    -- Metadata
    metadata JSON DEFAULT '{}',
    
    INDEX idx_session_health_session (session_id),
    INDEX idx_session_health_metric (health_metric),
    INDEX idx_session_health_measured (measured_at),
    INDEX idx_session_health_status (threshold_status)
);

-- Project-level intelligence and insights aggregation
CREATE TABLE project_insights (
    id INTEGER PRIMARY KEY,
    project_id INTEGER REFERENCES projects(id) ON DELETE CASCADE,
    
    -- Insight classification
    insight_type TEXT NOT NULL CHECK (insight_type IN (
        'productivity_analysis', 'pattern_recognition', 'optimization_opportunity',
        'risk_assessment', 'trend_analysis', 'coordination_summary',
        'health_overview', 'performance_benchmark'
    )),
    
    -- Insight content
    title TEXT NOT NULL,
    description TEXT,
    key_findings JSON DEFAULT '[]',
    recommendations JSON DEFAULT '[]',
    
    -- Supporting data
    data_sources JSON DEFAULT '[]', -- References to sessions, activities, etc.
    metrics JSON DEFAULT '{}',
    confidence_level REAL DEFAULT 0.0,
    
    -- Temporal relevance
    insight_period_start TIMESTAMP,
    insight_period_end TIMESTAMP,
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    
    -- Action tracking
    action_required BOOLEAN DEFAULT FALSE,
    action_taken TEXT,
    action_result TEXT,
    
    -- Metadata
    generated_by TEXT DEFAULT 'system',
    metadata JSON DEFAULT '{}',
    
    INDEX idx_project_insights_project (project_id),
    INDEX idx_project_insights_type (insight_type),
    INDEX idx_project_insights_generated (generated_at),
    INDEX idx_project_insights_action (action_required)
);

-- File relationships and dependency tracking
CREATE TABLE file_relationships (
    id INTEGER PRIMARY KEY,
    
    -- File relationship
    source_file TEXT NOT NULL,
    target_file TEXT NOT NULL,
    relationship_type TEXT NOT NULL CHECK (relationship_type IN (
        'import', 'include', 'reference', 'dependency', 'extends', 
        'implements', 'calls', 'modifies', 'tests', 'documents'
    )),
    
    -- Discovery context
    discovered_in_session INTEGER REFERENCES sessions(id),
    discovery_method TEXT, -- 'static_analysis', 'runtime_tracking', 'git_analysis'
    
    -- Relationship strength and metadata
    strength REAL DEFAULT 1.0, -- How strong/important this relationship is
    frequency INTEGER DEFAULT 1, -- How often this relationship is observed
    last_observed TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Contextual information
    line_number INTEGER,
    code_context TEXT,
    metadata JSON DEFAULT '{}',
    
    UNIQUE(source_file, target_file, relationship_type),
    INDEX idx_file_relationships_source (source_file),
    INDEX idx_file_relationships_target (target_file),
    INDEX idx_file_relationships_type (relationship_type),
    INDEX idx_file_relationships_session (discovered_in_session)
);

-- Configuration and user preferences
CREATE TABLE user_preferences (
    id INTEGER PRIMARY KEY,
    
    -- Preference categorization
    category TEXT NOT NULL, -- 'ui', 'notifications', 'health_monitoring', 'intelligence'
    key TEXT NOT NULL,
    value TEXT NOT NULL,
    value_type TEXT DEFAULT 'string' CHECK (value_type IN ('string', 'number', 'boolean', 'json')),
    
    -- Metadata
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE(category, key),
    INDEX idx_user_preferences_category (category)
);

-- Event log for system events and debugging
CREATE TABLE system_events (
    id INTEGER PRIMARY KEY,
    
    -- Event classification
    event_type TEXT NOT NULL, -- 'system_startup', 'session_created', 'error', 'warning', 'info'
    component TEXT NOT NULL, -- 'mcp_server', 'tui', 'database', 'intelligence_engine'
    
    -- Event content
    message TEXT NOT NULL,
    details JSON DEFAULT '{}',
    
    -- Error context (if applicable)
    error_code TEXT,
    stack_trace TEXT,
    
    -- Timing and correlation
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    correlation_id TEXT, -- For tracing related events
    
    -- Metadata
    metadata JSON DEFAULT '{}',
    
    INDEX idx_system_events_type (event_type),
    INDEX idx_system_events_component (component),
    INDEX idx_system_events_timestamp (timestamp),
    INDEX idx_system_events_correlation (correlation_id)
);

-- Views for common queries and intelligence aggregation

-- Active sessions with health status
CREATE VIEW active_sessions_with_health AS
SELECT 
    s.*,
    p.name as project_name,
    p.path as project_path,
    AVG(CASE WHEN sh.health_metric = 'responsiveness' THEN sh.value END) as avg_responsiveness,
    AVG(CASE WHEN sh.health_metric = 'activity_rate' THEN sh.value END) as avg_activity_rate,
    COUNT(DISTINCT sh.health_metric) as health_metrics_count,
    MAX(sh.measured_at) as last_health_check
FROM sessions s
JOIN projects p ON s.project_id = p.id
LEFT JOIN session_health sh ON s.id = sh.session_id 
    AND sh.measured_at > datetime('now', '-1 hour')
WHERE s.status IN ('active', 'idle')
GROUP BY s.id;

-- Project activity summary
CREATE VIEW project_activity_summary AS
SELECT 
    p.*,
    COUNT(DISTINCT s.id) as total_sessions,
    COUNT(DISTINCT CASE WHEN s.status IN ('active', 'idle') THEN s.id END) as active_sessions,
    COUNT(DISTINCT a.id) as total_activities,
    COUNT(DISTINCT fa.file_path) as files_touched,
    MAX(s.last_activity) as last_session_activity,
    AVG(s.health_score) as avg_session_health
FROM projects p
LEFT JOIN sessions s ON p.id = s.project_id
LEFT JOIN activities a ON s.id = a.session_id
LEFT JOIN file_access fa ON s.id = fa.session_id
WHERE p.status = 'active'
GROUP BY p.id;

-- Recent coordination opportunities
CREATE VIEW recent_coordination_opportunities AS
SELECT 
    co.*,
    s1.name as session_1_name,
    s2.name as session_2_name,
    p.name as project_name
FROM coordination_opportunities co
JOIN sessions s1 ON co.session_1_id = s1.id
JOIN sessions s2 ON co.session_2_id = s2.id
JOIN projects p ON co.project_id = p.id
WHERE co.status IN ('identified', 'acknowledged')
  AND co.identified_at > datetime('now', '-24 hours')
ORDER BY co.priority_score DESC, co.identified_at DESC;

-- Triggers for maintaining derived data and analytics

-- Update project activity when sessions change
CREATE TRIGGER update_project_last_activity
AFTER UPDATE OF last_activity ON sessions
BEGIN
    UPDATE projects 
    SET last_activity = NEW.last_activity
    WHERE id = NEW.project_id 
      AND (last_activity IS NULL OR NEW.last_activity > last_activity);
END;

-- Update session activity count when activities are added
CREATE TRIGGER update_session_activity_count
AFTER INSERT ON activities
BEGIN
    UPDATE sessions 
    SET activity_count = activity_count + 1,
        last_activity = NEW.timestamp
    WHERE id = NEW.session_id;
END;

-- Update session file access count when file access is recorded
CREATE TRIGGER update_session_file_count
AFTER INSERT ON file_access
BEGIN
    UPDATE sessions 
    SET file_access_count = file_access_count + 1,
        last_activity = NEW.timestamp
    WHERE id = NEW.session_id;
END;

-- Auto-update coordination opportunity status based on session status
CREATE TRIGGER auto_resolve_coordination_on_session_end
AFTER UPDATE OF status ON sessions
WHEN NEW.status = 'terminated'
BEGIN
    UPDATE coordination_opportunities 
    SET status = 'resolved',
        resolution_notes = 'Auto-resolved: session terminated',
        resolved_at = CURRENT_TIMESTAMP
    WHERE (session_1_id = NEW.id OR session_2_id = NEW.id)
      AND status IN ('identified', 'acknowledged');
END;

-- Initial data and default preferences
INSERT OR IGNORE INTO user_preferences (category, key, value, description) VALUES
('ui', 'theme', 'dark', 'Default UI theme'),
('ui', 'tab_position', 'top', 'Tab bar position'),
('ui', 'auto_refresh_interval', '5000', 'Auto refresh interval in milliseconds'),
('notifications', 'coordination_alerts', 'true', 'Show coordination opportunity alerts'),
('notifications', 'health_warnings', 'true', 'Show session health warnings'),
('health_monitoring', 'check_interval', '300', 'Health check interval in seconds'),
('health_monitoring', 'stuck_threshold', '600', 'Stuck session threshold in seconds'),
('intelligence', 'auto_analysis', 'true', 'Enable automatic session analysis'),
('intelligence', 'coordination_detection', 'true', 'Enable coordination opportunity detection');

-- Record initial schema migration
INSERT OR IGNORE INTO schema_migrations (version, description) VALUES
('3.0.0', 'Initial V3 schema with comprehensive intelligence features'),
('3.0.1', 'Added file relationships and system events tables'),
('3.0.2', 'Enhanced views and triggers for real-time analytics');

-- Performance optimization indices
CREATE INDEX IF NOT EXISTS idx_activities_session_timestamp ON activities(session_id, timestamp);
CREATE INDEX IF NOT EXISTS idx_file_access_session_timestamp ON file_access(session_id, timestamp);
CREATE INDEX IF NOT EXISTS idx_session_health_session_metric ON session_health(session_id, health_metric);
CREATE INDEX IF NOT EXISTS idx_projects_status_activity ON projects(status, last_activity);

-- Full-text search support for intelligence queries
CREATE VIRTUAL TABLE IF NOT EXISTS activities_fts USING fts5(
    summary, processed_content, metadata,
    content='activities'
);

CREATE VIRTUAL TABLE IF NOT EXISTS session_context_fts USING fts5(
    current_task, task_description, progress_summary,
    content='session_context'
);

-- Create triggers to maintain FTS indices
CREATE TRIGGER activities_fts_insert AFTER INSERT ON activities BEGIN
    INSERT INTO activities_fts(rowid, summary, processed_content, metadata)
    VALUES (NEW.id, NEW.summary, NEW.processed_content, NEW.metadata);
END;

CREATE TRIGGER activities_fts_delete AFTER DELETE ON activities BEGIN
    DELETE FROM activities_fts WHERE rowid = OLD.id;
END;

CREATE TRIGGER activities_fts_update AFTER UPDATE ON activities BEGIN
    DELETE FROM activities_fts WHERE rowid = OLD.id;
    INSERT INTO activities_fts(rowid, summary, processed_content, metadata)
    VALUES (NEW.id, NEW.summary, NEW.processed_content, NEW.metadata);
END;

CREATE TRIGGER session_context_fts_insert AFTER INSERT ON session_context BEGIN
    INSERT INTO session_context_fts(rowid, current_task, task_description, progress_summary)
    VALUES (NEW.id, NEW.current_task, NEW.task_description, NEW.progress_summary);
END;

CREATE TRIGGER session_context_fts_delete AFTER DELETE ON session_context BEGIN
    DELETE FROM session_context_fts WHERE rowid = OLD.id;
END;

CREATE TRIGGER session_context_fts_update AFTER UPDATE ON session_context BEGIN
    DELETE FROM session_context_fts WHERE rowid = OLD.id;
    INSERT INTO session_context_fts(rowid, current_task, task_description, progress_summary)
    VALUES (NEW.id, NEW.current_task, NEW.task_description, NEW.progress_summary);
END;