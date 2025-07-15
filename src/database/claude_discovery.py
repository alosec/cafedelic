#!/usr/bin/env python3
"""
Claude Code Discovery Module
Scans ~/.claude/projects/ and parses JSONL session files to extract real session data.
"""

import os
import json
import uuid
from pathlib import Path
from datetime import datetime, timezone
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass


@dataclass
class ClaudeSession:
    """Real Claude Code session data extracted from JSONL files"""
    session_uuid: str
    project_path: str
    project_name: str
    created_at: datetime
    last_activity: datetime
    conversation_turns: int
    total_cost_usd: float
    file_operations: List[str]
    is_active: bool
    jsonl_file_path: str


@dataclass
class ClaudeProject:
    """Claude Code project discovered from session analysis"""
    path: str
    name: str
    session_count: int
    last_activity: datetime
    sessions: List[str]  # UUIDs


class ClaudeDiscovery:
    """Discovers and parses Claude Code sessions from filesystem"""
    
    def __init__(self):
        self.claude_dir = Path.home() / '.claude' / 'projects'
        self.path_mappings = self._load_path_mappings()
    
    def _load_path_mappings(self) -> Dict[str, str]:
        """Load path mappings from JSON file"""
        try:
            mappings_file = Path(__file__).parent / 'path_mappings.json'
            if mappings_file.exists():
                with open(mappings_file, 'r') as f:
                    data = json.loads(f.read())
                    return data.get('mappings', {})
            return {}
        except Exception as e:
            print(f"Warning: Could not load path mappings: {e}")
            return {}
        
    def decode_project_path(self, encoded_name: str) -> str:
        """Decode Claude's directory encoding back to filesystem path"""
        # First check if we have a manual mapping for this encoded name
        if encoded_name in self.path_mappings:
            return self.path_mappings[encoded_name]
        
        # Fallback to algorithmic decoding
        # Claude encodes paths like: /home/alex/code/cafedelic -> -home-alex-code-cafedelic
        if encoded_name.startswith('-'):
            # Remove leading dash and replace remaining dashes with slashes
            decoded = encoded_name[1:].replace('-', '/')
            return f"/{decoded}"
        return encoded_name
    
    def find_all_projects(self) -> List[ClaudeProject]:
        """Scan ~/.claude/projects/ and discover all projects"""
        if not self.claude_dir.exists():
            return []
        
        projects = []
        
        for project_dir in self.claude_dir.iterdir():
            if not project_dir.is_dir():
                continue
                
            # Decode project path from directory name
            project_path = self.decode_project_path(project_dir.name)
            project_name = Path(project_path).name if project_path != project_dir.name else project_dir.name
            
            # Find all session files in this project
            session_files = list(project_dir.glob('*.jsonl'))
            session_uuids = []
            last_activity = None
            
            for session_file in session_files:
                # Extract UUID from filename
                session_uuid = session_file.stem
                
                # Validate it's a proper UUID
                try:
                    uuid.UUID(session_uuid)
                    session_uuids.append(session_uuid)
                    
                    # Get file modification time for last activity
                    file_mtime = datetime.fromtimestamp(session_file.stat().st_mtime, tz=timezone.utc)
                    if last_activity is None or file_mtime > last_activity:
                        last_activity = file_mtime
                        
                except ValueError:
                    # Not a valid UUID, skip this file
                    continue
            
            if session_uuids:  # Only add projects that have valid sessions
                projects.append(ClaudeProject(
                    path=project_path,
                    name=project_name,
                    session_count=len(session_uuids),
                    last_activity=last_activity or datetime.now(),
                    sessions=session_uuids
                ))
        
        return sorted(projects, key=lambda p: p.last_activity, reverse=True)
    
    def parse_session_file(self, session_file_path: Path) -> Optional[ClaudeSession]:
        """Parse a JSONL session file to extract metadata"""
        if not session_file_path.exists():
            return None
        
        try:
            session_uuid = session_file_path.stem
            
            # Validate UUID
            uuid.UUID(session_uuid)
            
            # Decode project path from parent directory
            project_path = self.decode_project_path(session_file_path.parent.name)
            project_name = Path(project_path).name if project_path != session_file_path.parent.name else session_file_path.parent.name
            
            # Parse JSONL file for metadata
            conversation_turns = 0
            total_cost = 0.0
            file_operations = []
            created_at = None
            last_activity = None
            
            with open(session_file_path, 'r', encoding='utf-8') as f:
                for line_num, line in enumerate(f):
                    line = line.strip()
                    if not line:
                        continue
                    
                    try:
                        entry = json.loads(line)
                        conversation_turns += 1
                        
                        # Extract timestamp
                        if 'timestamp' in entry:
                            timestamp = datetime.fromisoformat(entry['timestamp'].replace('Z', '+00:00'))
                            if created_at is None:
                                created_at = timestamp
                            last_activity = timestamp
                        
                        # Extract cost
                        if 'total_cost_usd' in entry:
                            total_cost += float(entry.get('total_cost_usd', 0))
                        
                        # Extract file operations from tool calls
                        if 'tool_calls' in entry:
                            for tool_call in entry['tool_calls']:
                                if isinstance(tool_call, dict) and 'tool_input' in tool_call:
                                    tool_input = tool_call['tool_input']
                                    if 'file_path' in tool_input:
                                        file_path = tool_input['file_path']
                                        if file_path not in file_operations:
                                            file_operations.append(file_path)
                    
                    except json.JSONDecodeError:
                        # Skip malformed lines
                        continue
                    except Exception as e:
                        # Skip problematic entries but continue parsing
                        print(f"Warning: Error parsing line {line_num} in {session_file_path}: {e}")
                        continue
            
            # Use file modification time if no timestamps in content
            if last_activity is None:
                last_activity = datetime.fromtimestamp(session_file_path.stat().st_mtime, tz=timezone.utc)
            if created_at is None:
                created_at = datetime.fromtimestamp(session_file_path.stat().st_ctime, tz=timezone.utc)
            
            # Check if session is currently active (basic process check)
            is_active = self._is_session_active(session_uuid)
            
            return ClaudeSession(
                session_uuid=session_uuid,
                project_path=project_path,
                project_name=project_name,
                created_at=created_at,
                last_activity=last_activity,
                conversation_turns=conversation_turns,
                total_cost_usd=total_cost,
                file_operations=file_operations,
                is_active=is_active,
                jsonl_file_path=str(session_file_path)
            )
            
        except Exception as e:
            print(f"Error parsing session file {session_file_path}: {e}")
            return None
    
    def find_all_sessions(self) -> List[ClaudeSession]:
        """Find and parse all Claude Code sessions"""
        sessions = []
        
        if not self.claude_dir.exists():
            return sessions
        
        # Find all JSONL files in all project directories
        for session_file in self.claude_dir.glob('*/*.jsonl'):
            session_data = self.parse_session_file(session_file)
            if session_data:
                sessions.append(session_data)
        
        return sorted(sessions, key=lambda s: s.last_activity, reverse=True)
    
    def find_sessions_for_project(self, project_path: str) -> List[ClaudeSession]:
        """Find all sessions for a specific project path"""
        all_sessions = self.find_all_sessions()
        return [s for s in all_sessions if s.project_path == project_path]
    
    def _is_session_active(self, session_uuid: str) -> bool:
        """Check if a session is currently active (basic process detection)"""
        try:
            import subprocess
            # Look for claude processes that might be running this session
            result = subprocess.run(['ps', 'aux'], capture_output=True, text=True)
            
            # Look for claude processes
            claude_processes = [line for line in result.stdout.split('\n') if 'claude' in line and 'grep' not in line]
            
            # This is a basic check - more sophisticated process detection could be added
            return len(claude_processes) > 0
            
        except Exception:
            return False
    
    def get_session_summary(self) -> Dict[str, int]:
        """Get summary statistics about Claude Code sessions"""
        sessions = self.find_all_sessions()
        projects = self.find_all_projects()
        
        active_sessions = sum(1 for s in sessions if s.is_active)
        total_turns = sum(s.conversation_turns for s in sessions)
        total_cost = sum(s.total_cost_usd for s in sessions)
        
        return {
            'total_projects': len(projects),
            'total_sessions': len(sessions),
            'active_sessions': active_sessions,
            'total_conversation_turns': total_turns,
            'total_cost_usd': round(total_cost, 4)
        }


# Global discovery instance
_discovery_instance = None

def get_claude_discovery() -> ClaudeDiscovery:
    """Get global Claude discovery instance"""
    global _discovery_instance
    if _discovery_instance is None:
        _discovery_instance = ClaudeDiscovery()
    return _discovery_instance