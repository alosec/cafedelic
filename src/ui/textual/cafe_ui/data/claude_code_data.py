#!/usr/bin/env python3
"""
Claude Code data functions for Cafedelic navigation interface
Provides real session and project data from Claude Code filesystem via HTTP API.
Enhanced with project/session grouping and navigation support.
"""

from typing import List, Dict, Any, Optional
from collections import defaultdict
from ..adapter import mcp_adapter

def get_sessions() -> List[Dict[str, Any]]:
    """Get real sessions from Claude Code filesystem via HTTP API"""
    try:
        response = mcp_adapter.get_sessions()
        if 'error' in response:
            print(f"Error getting sessions: {response['error']}")
            return []
        sessions = response.get('sessions', [])
        
        # Enhance sessions with derived data for navigation
        for session in sessions:
            session = _enhance_session_data(session)
        
        return sessions
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
        projects = response.get('projects', [])
        
        # Enhance projects with session data
        sessions = get_sessions()
        for project in projects:
            project = _enhance_project_data(project, sessions)
        
        return projects
    except Exception as e:
        print(f"Failed to get projects: {e}")
        return []

def get_sessions_by_project(project_name: str) -> List[Dict[str, Any]]:
    """Get all sessions for a specific project"""
    sessions = get_sessions()
    return [s for s in sessions if s.get('project') == project_name]

def get_projects_with_session_counts() -> List[Dict[str, Any]]:
    """Get projects with session count information"""
    projects = get_projects()
    sessions = get_sessions()
    
    # Group sessions by project
    project_sessions = defaultdict(list)
    for session in sessions:
        project_name = session.get('project', 'Unknown')
        project_sessions[project_name].append(session)
    
    # Add session counts to projects
    for project in projects:
        project_name = project.get('name', '')
        project_session_list = project_sessions.get(project_name, [])
        
        project['session_counts'] = {
            'total': len(project_session_list),
            'active': len([s for s in project_session_list if s.get('status') not in ['idle', 'archived']]),
            'idle': len([s for s in project_session_list if s.get('status') == 'idle']),
            'stuck': len([s for s in project_session_list if s.get('status') == 'stuck'])
        }
    
    return projects

def get_session_by_id(session_id: str) -> Dict[str, Any]:
    """Get specific session by ID"""
    sessions = get_sessions()
    session = next((s for s in sessions if s['id'] == session_id), {})
    return _enhance_session_data(session) if session else {}

def get_project_by_name(project_name: str) -> Dict[str, Any]:
    """Get specific project by name"""
    projects = get_projects()
    return next((p for p in projects if p.get('name') == project_name), {})

def get_activity_emoji(activity_level: int) -> str:
    """Convert activity level (0-3) to emoji indicators"""
    return ["○○○", "●○○", "●●○", "●●●"][min(activity_level, 3)]

def get_files_by_project(project_name: str) -> List[Dict]:
    """Get files associated with a project from sessions"""
    sessions = get_sessions_by_project(project_name)
    files = []
    seen_paths = set()
    
    for session in sessions:
        for file in session.get('files_context', []):
            file_path = file.get('path', '')
            if file_path and file_path not in seen_paths:
                files.append(file)
                seen_paths.add(file_path)
    
    return files

def get_context_files_by_session(session_id: str) -> List[Dict]:
    """Get context files for a specific session"""
    session = get_session_by_id(session_id)
    return session.get('files_context', [])

def get_recent_sessions(hours: int = 24) -> List[Dict[str, Any]]:
    """Get sessions with recent activity"""
    # TODO: Implement proper time filtering when timestamps are available
    sessions = get_sessions()
    # For now, return sessions that are not idle
    return [s for s in sessions if s.get('status') != 'idle']

def get_stuck_sessions() -> List[Dict[str, Any]]:
    """Get sessions that appear to be stuck"""
    sessions = get_sessions()
    return [s for s in sessions if s.get('status') == 'stuck']

def get_sessions_by_status(status: str) -> List[Dict[str, Any]]:
    """Get sessions filtered by status"""
    sessions = get_sessions()
    return [s for s in sessions if s.get('status') == status]

def group_sessions_by_project() -> Dict[str, List[Dict[str, Any]]]:
    """Group all sessions by their project"""
    sessions = get_sessions()
    grouped = defaultdict(list)
    
    for session in sessions:
        project_name = session.get('project', 'Unknown')
        grouped[project_name].append(session)
    
    return dict(grouped)

def get_project_activity_summary(project_name: str) -> Dict[str, Any]:
    """Get activity summary for a project"""
    sessions = get_sessions_by_project(project_name)
    files = get_files_by_project(project_name)
    
    # Calculate activity levels
    total_activity = sum(s.get('activity_level', 0) for s in sessions)
    avg_activity = total_activity / len(sessions) if sessions else 0
    
    # Session status counts
    status_counts = defaultdict(int)
    for session in sessions:
        status_counts[session.get('status', 'unknown')] += 1
    
    return {
        'project_name': project_name,
        'total_sessions': len(sessions),
        'total_files': len(files),
        'average_activity': avg_activity,
        'status_distribution': dict(status_counts),
        'recent_activity': len([s for s in sessions if s.get('status') not in ['idle', 'archived']])
    }

def search_sessions(query: str) -> List[Dict[str, Any]]:
    """Search sessions by name, project, or content"""
    sessions = get_sessions()
    query_lower = query.lower()
    
    matching_sessions = []
    for session in sessions:
        # Search in session name
        if query_lower in session.get('name', '').lower():
            matching_sessions.append(session)
            continue
        
        # Search in project name
        if query_lower in session.get('project', '').lower():
            matching_sessions.append(session)
            continue
        
        # Search in session task/description
        if query_lower in session.get('task', '').lower():
            matching_sessions.append(session)
            continue
    
    return matching_sessions

def search_projects(query: str) -> List[Dict[str, Any]]:
    """Search projects by name or description"""
    projects = get_projects()
    query_lower = query.lower()
    
    matching_projects = []
    for project in projects:
        # Search in project name
        if query_lower in project.get('name', '').lower():
            matching_projects.append(project)
            continue
        
        # Search in project description
        if query_lower in project.get('description', '').lower():
            matching_projects.append(project)
            continue
    
    return matching_projects

def _enhance_session_data(session: Dict[str, Any]) -> Dict[str, Any]:
    """Enhance session data with computed fields for navigation"""
    if not session:
        return session
    
    # Ensure required fields exist with defaults
    session.setdefault('activity_level', 0)
    session.setdefault('progress', 0.0)
    session.setdefault('duration', '0m')
    session.setdefault('status', 'unknown')
    session.setdefault('files_context', [])
    
    # Compute activity level from file operations if not set
    if session['activity_level'] == 0 and session['files_context']:
        # Simple heuristic: more files in context = higher activity
        file_count = len(session['files_context'])
        if file_count >= 10:
            session['activity_level'] = 3
        elif file_count >= 5:
            session['activity_level'] = 2
        elif file_count >= 1:
            session['activity_level'] = 1
    
    return session

def _enhance_project_data(project: Dict[str, Any], sessions: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Enhance project data with session information"""
    if not project:
        return project
    
    project_name = project.get('name', '')
    project_sessions = [s for s in sessions if s.get('project') == project_name]
    
    # Add session counts
    project['session_count'] = len(project_sessions)
    project['active_session_count'] = len([s for s in project_sessions if s.get('status') not in ['idle', 'archived']])
    
    # Calculate overall activity level
    if project_sessions:
        total_activity = sum(s.get('activity_level', 0) for s in project_sessions)
        avg_activity = total_activity / len(project_sessions)
        project['activity_level'] = min(3, int(avg_activity))
    else:
        project['activity_level'] = 0
    
    # Set project status based on session activity
    if project['active_session_count'] > 0:
        project['status'] = 'active'
    elif project['session_count'] > 0:
        project['status'] = 'idle'
    else:
        project['status'] = 'empty'
    
    return project