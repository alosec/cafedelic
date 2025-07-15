#!/usr/bin/env python3
"""
Session Loader Module
Orchestrates loading of discovered Claude Code sessions into the database.
"""

from typing import List, Dict, Optional, Tuple
from pathlib import Path

from .claude_discovery import ClaudeDiscovery, ClaudeSession, get_claude_discovery
from .session_db import SessionDatabase, get_database


class SessionLoader:
    """Loads discovered Claude Code sessions into the database"""
    
    def __init__(self):
        self.claude_discovery = get_claude_discovery()
        self.db = get_database()
    
    def load_discovered_sessions(self) -> Dict[str, int]:
        """
        Load all discovered Claude Code sessions into the database
        Returns summary of what was loaded
        """
        # Discover all Claude sessions
        claude_sessions = self.claude_discovery.find_all_sessions()
        
        # Get existing projects from database
        db_projects = self.db.get_projects()
        
        # Track loading results
        results = {
            'discovered': len(claude_sessions),
            'loaded': 0,
            'skipped': 0,
            'errors': 0,
            'new_projects': 0
        }
        
        # Group sessions by project path for easier matching
        sessions_by_project = {}
        for session in claude_sessions:
            project_path = session.project_path
            if project_path not in sessions_by_project:
                sessions_by_project[project_path] = []
            sessions_by_project[project_path].append(session)
        
        # Process each project's sessions
        for project_path, sessions in sessions_by_project.items():
            # Find matching database project
            project_id = self._find_or_create_project(project_path, db_projects)
            
            if project_id:
                # Load sessions for this project
                for session in sessions:
                    try:
                        session_id = self.db.create_session_from_claude_data(session, project_id)
                        if session_id:
                            results['loaded'] += 1
                        else:
                            results['skipped'] += 1
                    except Exception as e:
                        print(f"Error loading session {session.session_uuid} for project {project_path}: {e}")
                        results['errors'] += 1
            else:
                print(f"Could not find or create project for path: {project_path} ({len(sessions)} sessions skipped)")
                results['errors'] += len(sessions)
        
        return results
    
    def _find_or_create_project(self, project_path: str, db_projects: List) -> Optional[str]:
        """
        Find existing project in database or create new one if needed
        Returns project short_id or None if error
        """
        # Try to find existing project by path
        for project in db_projects:
            if project.path == project_path:
                return project.short_id
        
        # Project doesn't exist, create it if the path exists on filesystem
        if Path(project_path).exists():
            try:
                project_name = Path(project_path).name
                short_id = self.db.create_project(
                    name=project_name,
                    path=project_path,
                    description=f"Auto-discovered from Claude Code sessions",
                    discovered_from='claude_sessions'
                )
                return short_id
            except Exception as e:
                print(f"Error creating project for {project_path}: {e}")
                return None
        else:
            print(f"Project path {project_path} no longer exists, skipping sessions")
            return None
    
    def get_session_loading_summary(self) -> Dict[str, any]:
        """Get summary of discovered sessions vs database sessions"""
        claude_sessions = self.claude_discovery.find_all_sessions()
        db_sessions = self.db.get_sessions()
        
        # Count sessions by status
        claude_by_uuid = {s.session_uuid: s for s in claude_sessions}
        db_with_claude_uuid = [s for s in db_sessions if hasattr(s, 'claude_session_uuid') and s.claude_session_uuid]
        
        return {
            'claude_sessions_discovered': len(claude_sessions),
            'database_sessions_total': len(db_sessions),
            'database_sessions_from_claude': len(db_with_claude_uuid),
            'claude_sessions_not_in_db': len(claude_sessions) - len(db_with_claude_uuid),
            'active_claude_sessions': len([s for s in claude_sessions if s.is_active])
        }


def get_session_loader() -> SessionLoader:
    """Get session loader instance"""
    return SessionLoader()