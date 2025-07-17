#!/usr/bin/env python3
"""
Real data functions for Cafedelic simple chat interface
Provides real session and project data from database.
"""

from typing import List, Dict, Any
from src.database.session_db import get_database

def get_sessions() -> List[Dict[str, Any]]:
    """Get real sessions from database"""
    db = get_database()
    sessions = db.get_sessions()
    
    # Convert to format expected by TUI
    result = []
    for session in sessions:
        result.append({
            'id': session.short_id,
            'name': session.name,
            'project': session.project_name,
            'status': session.status,
            'task': session.task_description,
            'progress': 0.5,  # Default progress
            'duration': '0m',  # Calculate from timestamps if needed
            'last_activity': session.last_activity,
            'files_context': []  # Could be populated from file_context table
        })
    
    return result

def get_projects() -> List[Dict[str, Any]]:
    """Get real projects from database"""
    db = get_database()
    projects = db.get_projects()
    
    # Convert to format expected by TUI
    result = []
    for project in projects:
        result.append({
            'name': project.name,
            'path': project.path,
            'status': project.status,
            'sessions': [s.short_id for s in db.get_sessions() if s.project_name == project.name],
            'activity_level': min(project.session_count, 3)  # 0-3 for emoji display
        })
    
    return result

def get_session_by_id(session_id: str) -> Dict[str, Any]:
    """Get specific session by ID"""
    sessions = get_sessions()
    return next((s for s in sessions if s['id'] == session_id), {})

def get_activity_emoji(activity_level: int) -> str:
    """Convert activity level (0-3) to emoji indicators"""
    return ["○○○", "●○○", "●●○", "●●●"][min(activity_level, 3)]

def get_files_by_project(project_name: str) -> List[Dict]:
    """Get files associated with a project (placeholder for now)"""
    return []  # Could query file_context table when implemented

def get_context_files_by_session(session_id: str) -> List[Dict]:
    """Get context files for a session (placeholder for now)"""
    return []  # Could query file_context table when implemented