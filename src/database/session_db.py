#!/usr/bin/env python3
"""
SQLite utilities for Cafedelic session and project management
Provides database operations for the TUI interface.
"""

import sqlite3
import os
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass


@dataclass
class Project:
    """Project data model"""
    short_id: str
    name: str
    path: str
    description: str
    status: str
    session_count: int = 0
    active_sessions: int = 0
    last_activity: Optional[str] = None


@dataclass
class Session:
    """Session data model"""
    short_id: str
    name: str
    project_name: str
    project_short_id: str
    status: str
    task_description: str
    tmux_session_name: Optional[str]
    last_activity: str
    recent_activity_count: int = 0


class SessionDatabase:
    """SQLite database manager for projects and sessions"""
    
    def __init__(self, db_path: str = None):
        if db_path is None:
            # Default to ~/.cafedelic/intelligence.db
            home = Path.home()
            cafedelic_dir = home / '.cafedelic'
            cafedelic_dir.mkdir(exist_ok=True)
            db_path = str(cafedelic_dir / 'intelligence.db')
        
        self.db_path = db_path
        self.connection = None
        
    def connect(self) -> sqlite3.Connection:
        """Get or create database connection"""
        if self.connection is None:
            self.connection = sqlite3.connect(self.db_path)
            self.connection.row_factory = sqlite3.Row  # Enable dict-like access
            self.connection.execute("PRAGMA foreign_keys = ON")
        return self.connection
    
    def close(self):
        """Close database connection"""
        if self.connection:
            self.connection.close()
            self.connection = None
    
    def initialize(self):
        """Initialize database with schema and seed data"""
        conn = self.connect()
        
        # Read and execute schema
        schema_path = Path(__file__).parent / 'fresh_schema.sql'
        with open(schema_path, 'r') as f:
            schema_sql = f.read()
        
        # Execute schema as a single script (SQLite executescript handles complex SQL)
        try:
            conn.executescript(schema_sql)
        except Exception as e:
            print(f"Error executing schema: {e}")
            # Try executing in smaller chunks for debugging
            statements = [stmt.strip() for stmt in schema_sql.split(';') if stmt.strip()]
            for i, statement in enumerate(statements):
                try:
                    if statement and not statement.startswith('--'):
                        conn.execute(statement)
                except Exception as stmt_error:
                    print(f"Error in statement {i}: {stmt_error}")
                    print(f"Statement: {statement[:100]}...")
                    raise
        
        # No seed data - using real discovery instead
        
        conn.commit()
        return True
    
    def is_initialized(self) -> bool:
        """Check if database is initialized"""
        try:
            conn = self.connect()
            cursor = conn.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='projects'")
            return cursor.fetchone() is not None
        except:
            return False
    
    def get_projects(self) -> List[Project]:
        """Get all projects with summary information"""
        conn = self.connect()
        
        # Use the project_summary view if available, fallback to direct query
        try:
            cursor = conn.execute("SELECT * FROM project_summary ORDER BY last_session_activity DESC")
        except sqlite3.OperationalError:
            # Fallback: direct query if view doesn't exist
            cursor = conn.execute("""
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
                ORDER BY last_session_activity DESC
            """)
        
        projects = []
        for row in cursor.fetchall():
            projects.append(Project(
                short_id=row['short_id'],
                name=row['name'],
                path=row['path'],
                description='',  # Not in view
                status=row['status'],
                session_count=row['session_count'] or 0,
                active_sessions=row['active_sessions'] or 0,
                last_activity=row['last_session_activity']
            ))
        
        return projects
    
    def get_sessions(self, project_id: str = None) -> List[Session]:
        """Get sessions, optionally filtered by project"""
        conn = self.connect()
        
        # Use the active_sessions view if available, fallback to direct query
        try:
            if project_id:
                cursor = conn.execute(
                    "SELECT * FROM active_sessions WHERE project_short_id = ? ORDER BY last_activity DESC",
                    (project_id,)
                )
            else:
                cursor = conn.execute("SELECT * FROM active_sessions ORDER BY last_activity DESC")
        except sqlite3.OperationalError:
            # Fallback: direct query if view doesn't exist
            base_query = """
                SELECT 
                    s.short_id,
                    s.name,
                    p.name as project_name,
                    p.short_id as project_short_id,
                    s.status,
                    s.task_description,
                    s.tmux_session_name,
                    s.last_activity,
                    0 as recent_activity_count
                FROM sessions s
                JOIN projects p ON s.project_id = p.id
                WHERE s.status != 'archived'
                {}
                ORDER BY s.last_activity DESC
            """
            
            if project_id:
                cursor = conn.execute(base_query.format("AND p.short_id = ?"), (project_id,))
            else:
                cursor = conn.execute(base_query.format(""))
        
        sessions = []
        for row in cursor.fetchall():
            sessions.append(Session(
                short_id=row['short_id'],
                name=row['name'],
                project_name=row['project_name'],
                project_short_id=row['project_short_id'],
                status=row['status'],
                task_description=row['task_description'] or 'No task assigned',
                tmux_session_name=row['tmux_session_name'],
                last_activity=row['last_activity'],
                recent_activity_count=row['recent_activity_count'] if 'recent_activity_count' in row.keys() else 0
            ))
        
        return sessions
    
    def get_session_by_id(self, short_id: str) -> Optional[Session]:
        """Get specific session by short ID"""
        sessions = self.get_sessions()
        return next((s for s in sessions if s.short_id == short_id), None)
    
    def get_project_by_id(self, short_id: str) -> Optional[Project]:
        """Get specific project by short ID"""
        projects = self.get_projects()
        return next((p for p in projects if p.short_id == short_id), None)
    
    def update_session_tmux_info(self, session_id: str, tmux_session_name: str, 
                                tmux_window_id: str = None, tmux_pane_id: str = None):
        """Update tmux information for a session"""
        conn = self.connect()
        conn.execute("""
            UPDATE sessions 
            SET tmux_session_name = ?, tmux_window_id = ?, tmux_pane_id = ?, 
                updated_at = CURRENT_TIMESTAMP, last_activity = CURRENT_TIMESTAMP
            WHERE short_id = ?
        """, (tmux_session_name, tmux_window_id, tmux_pane_id, session_id))
        conn.commit()
    
    def update_session_status(self, session_id: str, status: str):
        """Update session status"""
        conn = self.connect()
        conn.execute("""
            UPDATE sessions 
            SET status = ?, updated_at = CURRENT_TIMESTAMP, last_activity = CURRENT_TIMESTAMP
            WHERE short_id = ?
        """, (status, session_id))
        conn.commit()
    
    def add_activity(self, session_id: str, activity_type: str, description: str, file_path: str = None):
        """Add activity record for a session"""
        conn = self.connect()
        conn.execute("""
            INSERT INTO activities (session_id, activity_type, description, file_path)
            SELECT id, ?, ?, ? FROM sessions WHERE short_id = ?
        """, (activity_type, description, file_path, session_id))
        conn.commit()
    
    def get_recent_activities(self, session_id: str = None, limit: int = 10) -> List[Dict]:
        """Get recent activities, optionally for specific session"""
        conn = self.connect()
        
        if session_id:
            cursor = conn.execute("""
                SELECT a.*, s.short_id as session_short_id, s.name as session_name
                FROM activities a
                JOIN sessions s ON a.session_id = s.id
                WHERE s.short_id = ?
                ORDER BY a.created_at DESC
                LIMIT ?
            """, (session_id, limit))
        else:
            cursor = conn.execute("""
                SELECT a.*, s.short_id as session_short_id, s.name as session_name
                FROM activities a
                JOIN sessions s ON a.session_id = s.id
                ORDER BY a.created_at DESC
                LIMIT ?
            """, (limit,))
        
        return [dict(row) for row in cursor.fetchall()]
    
    def create_project(self, name: str, path: str, description: str = '', 
                      has_git: bool = False, git_remote_url: str = '', 
                      discovered_from: str = 'manual') -> str:
        """Create new project and return short_id"""
        conn = self.connect()
        
        # Find next available short_id
        cursor = conn.execute("SELECT short_id FROM projects WHERE short_id LIKE 'p%' ORDER BY short_id DESC LIMIT 1")
        last_id = cursor.fetchone()
        if last_id:
            next_num = int(last_id['short_id'][1:]) + 1
        else:
            next_num = 1
        
        short_id = f"p{next_num}"
        
        conn.execute("""
            INSERT INTO projects (short_id, name, path, description, has_git, git_remote_url, discovered_from)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (short_id, name, path, description, has_git, git_remote_url, discovered_from))
        conn.commit()
        
        return short_id
    
    def create_session(self, project_id: str, name: str, task_description: str = '') -> str:
        """Create new session and return short_id"""
        conn = self.connect()
        
        # Find next available short_id
        cursor = conn.execute("SELECT short_id FROM sessions WHERE short_id LIKE 's%' ORDER BY short_id DESC LIMIT 1")
        last_id = cursor.fetchone()
        if last_id:
            next_num = int(last_id['short_id'][1:]) + 1
        else:
            next_num = 1
        
        short_id = f"s{next_num}"
        
        # Get project's actual ID
        cursor = conn.execute("SELECT id FROM projects WHERE short_id = ?", (project_id,))
        project_row = cursor.fetchone()
        if not project_row:
            raise ValueError(f"Project {project_id} not found")
        
        conn.execute("""
            INSERT INTO sessions (short_id, project_id, name, task_description)
            VALUES (?, ?, ?, ?)
        """, (short_id, project_row['id'], name, task_description))
        conn.commit()
        
        return short_id


# Global database instance
_db_instance = None

def get_database() -> SessionDatabase:
    """Get global database instance"""
    global _db_instance
    if _db_instance is None:
        _db_instance = SessionDatabase()
        if not _db_instance.is_initialized():
            _db_instance.initialize()
    return _db_instance