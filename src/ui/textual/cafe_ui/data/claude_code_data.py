#!/usr/bin/env python3
"""
Claude Code data functions for Cafedelic simple chat interface
Provides real session and project data from Claude Code filesystem via HTTP API.
"""

from typing import List, Dict, Any
from ..adapter import mcp_adapter

def get_sessions() -> List[Dict[str, Any]]:
    """Get real sessions from Claude Code filesystem via HTTP API"""
    try:
        response = mcp_adapter.get_sessions()
        if 'error' in response:
            print(f"Error getting sessions: {response['error']}")
            return []
        return response.get('sessions', [])
    except Exception as e:
        print(f"Failed to get sessions: {e}")
        return []

def get_projects() -> List[Dict[str, Any]]:
    """Get real projects from Claude Code filesystem via HTTP API"""
    try:
        response = mcp_adapter.get_projects()
        if 'error' in response:
            print(f"Error getting projects: {response['error']}")
            return []
        return response.get('projects', [])
    except Exception as e:
        print(f"Failed to get projects: {e}")
        return []

def get_session_by_id(session_id: str) -> Dict[str, Any]:
    """Get specific session by ID"""
    sessions = get_sessions()
    return next((s for s in sessions if s['id'] == session_id), {})

def get_activity_emoji(activity_level: int) -> str:
    """Convert activity level (0-3) to emoji indicators"""
    return ["○○○", "●○○", "●●○", "●●●"][min(activity_level, 3)]

def get_files_by_project(project_name: str) -> List[Dict]:
    """Get files associated with a project from sessions"""
    sessions = get_sessions()
    files = []
    for session in sessions:
        if session.get('project') == project_name:
            files.extend(session.get('files_context', []))
    return files

def get_context_files_by_session(session_id: str) -> List[Dict]:
    """Get context files for a specific session"""
    session = get_session_by_id(session_id)
    return session.get('files_context', [])