#!/usr/bin/env python3
"""
Project Discovery Module
Manages user-defined project paths and integrates with Claude Code discovery.
"""

import os
import subprocess
from pathlib import Path
from typing import List, Dict, Optional, Set
from dataclasses import dataclass

from .claude_discovery import get_claude_discovery, ClaudeProject


@dataclass
class DiscoveredProject:
    """Project discovered through filesystem scanning"""
    path: str
    name: str
    has_git: bool
    has_claude_sessions: bool
    claude_project: Optional[ClaudeProject] = None


class ProjectDiscovery:
    """Manages project discovery and path management"""
    
    def __init__(self):
        self.claude_discovery = get_claude_discovery()
        
    def scan_directory_for_projects(self, base_path: str, max_depth: int = 2) -> List[DiscoveredProject]:
        """Scan a directory for potential projects (Git repos, etc.)"""
        projects = []
        base_path = Path(base_path).expanduser().resolve()
        
        if not base_path.exists() or not base_path.is_dir():
            return projects
        
        # Get Claude projects for cross-reference
        claude_projects = {p.path: p for p in self.claude_discovery.find_all_projects()}
        
        def scan_path(current_path: Path, depth: int = 0):
            if depth > max_depth:
                return
                
            try:
                # Check if this directory is a project
                is_project = False
                has_git = False
                
                # Check for Git repository
                if (current_path / '.git').exists():
                    is_project = True
                    has_git = True
                
                # Check for common project indicators
                project_indicators = [
                    'package.json',      # Node.js
                    'Cargo.toml',        # Rust
                    'pyproject.toml',    # Python
                    'requirements.txt',  # Python
                    'pom.xml',          # Java Maven
                    'build.gradle',     # Java Gradle
                    'Gemfile',          # Ruby
                    'composer.json',    # PHP
                    'go.mod',           # Go
                    'CMakeLists.txt',   # C/C++ CMake
                    'Makefile',         # C/C++ Make
                    '.project',         # Eclipse
                    'tsconfig.json',    # TypeScript
                ]
                
                for indicator in project_indicators:
                    if (current_path / indicator).exists():
                        is_project = True
                        break
                
                if is_project:
                    project_path = str(current_path)
                    project_name = current_path.name
                    
                    # Check if this project has Claude sessions
                    claude_project = claude_projects.get(project_path)
                    has_claude_sessions = claude_project is not None
                    
                    projects.append(DiscoveredProject(
                        path=project_path,
                        name=project_name,
                        has_git=has_git,
                        has_claude_sessions=has_claude_sessions,
                        claude_project=claude_project
                    ))
                
                # Continue scanning subdirectories if not too deep
                if depth < max_depth:
                    for subdir in current_path.iterdir():
                        if subdir.is_dir() and not subdir.name.startswith('.'):
                            # Skip common non-project directories
                            skip_dirs = {
                                'node_modules', '__pycache__', '.git', 'venv', 'env',
                                'build', 'dist', 'target', '.idea', '.vscode',
                                'coverage', '.pytest_cache', '.mypy_cache'
                            }
                            if subdir.name not in skip_dirs:
                                scan_path(subdir, depth + 1)
                                
            except (PermissionError, OSError):
                # Skip directories we can't access
                pass
        
        scan_path(base_path)
        return sorted(projects, key=lambda p: p.name.lower())
    
    def find_common_project_directories(self) -> List[str]:
        """Find common directories where projects might be located"""
        home = Path.home()
        common_dirs = []
        
        # Common project directories
        candidates = [
            home / 'code',
            home / 'projects',
            home / 'dev',
            home / 'work',
            home / 'src',
            home / 'repos',
            home / 'Development',
            home / 'Documents' / 'code',
            home / 'Documents' / 'projects',
        ]
        
        for candidate in candidates:
            if candidate.exists() and candidate.is_dir():
                try:
                    # Check if it contains subdirectories (potential projects)
                    subdirs = [d for d in candidate.iterdir() if d.is_dir()]
                    if subdirs:
                        common_dirs.append(str(candidate))
                except (PermissionError, OSError):
                    pass
        
        return common_dirs
    
    def get_claude_only_projects(self) -> List[ClaudeProject]:
        """Get projects that exist in Claude but might not be on filesystem"""
        claude_projects = self.claude_discovery.find_all_projects()
        filesystem_projects = []
        
        for project in claude_projects:
            if not Path(project.path).exists():
                # Project path doesn't exist on filesystem anymore
                continue
            filesystem_projects.append(project)
        
        return filesystem_projects
    
    def validate_project_path(self, path: str) -> Dict[str, any]:
        """Validate a project path and return information about it"""
        path_obj = Path(path).expanduser().resolve()
        
        result = {
            'path': str(path_obj),
            'exists': path_obj.exists(),
            'is_directory': path_obj.is_dir() if path_obj.exists() else False,
            'is_readable': os.access(path_obj, os.R_OK) if path_obj.exists() else False,
            'has_git': False,
            'project_indicators': [],
            'has_claude_sessions': False,
            'claude_session_count': 0
        }
        
        if result['exists'] and result['is_directory'] and result['is_readable']:
            # Check for Git
            if (path_obj / '.git').exists():
                result['has_git'] = True
            
            # Check for project indicators
            project_indicators = [
                'package.json', 'Cargo.toml', 'pyproject.toml', 'requirements.txt',
                'pom.xml', 'build.gradle', 'Gemfile', 'composer.json', 'go.mod',
                'CMakeLists.txt', 'Makefile', '.project', 'tsconfig.json'
            ]
            
            for indicator in project_indicators:
                if (path_obj / indicator).exists():
                    result['project_indicators'].append(indicator)
            
            # Check for Claude sessions
            claude_projects = {p.path: p for p in self.claude_discovery.find_all_projects()}
            if str(path_obj) in claude_projects:
                result['has_claude_sessions'] = True
                result['claude_session_count'] = claude_projects[str(path_obj)].session_count
        
        return result
    
    def get_git_remote_info(self, project_path: str) -> Dict[str, str]:
        """Get Git remote information for a project"""
        try:
            path_obj = Path(project_path)
            if not (path_obj / '.git').exists():
                return {}
            
            result = subprocess.run(
                ['git', 'remote', '-v'],
                cwd=project_path,
                capture_output=True,
                text=True,
                timeout=5
            )
            
            if result.returncode == 0:
                remotes = {}
                for line in result.stdout.strip().split('\n'):
                    if line:
                        parts = line.split()
                        if len(parts) >= 2:
                            remote_name = parts[0]
                            remote_url = parts[1]
                            if remote_name not in remotes:
                                remotes[remote_name] = remote_url
                return remotes
        except Exception:
            pass
        
        return {}
    
    def suggest_project_paths(self) -> List[Dict[str, any]]:
        """Suggest project paths based on Claude sessions and common directories"""
        suggestions = []
        
        # Add Claude-discovered projects
        claude_projects = self.claude_discovery.find_all_projects()
        for project in claude_projects:
            validation = self.validate_project_path(project.path)
            suggestions.append({
                'path': project.path,
                'name': project.name,
                'source': 'claude_sessions',
                'session_count': project.session_count,
                'last_activity': project.last_activity.strftime('%Y-%m-%d %H:%M'),
                'validation': validation
            })
        
        # Add parent directories of Claude projects as scan targets
        parent_dirs = set()
        for project in claude_projects:
            parent = str(Path(project.path).parent)
            if parent not in parent_dirs:
                parent_dirs.add(parent)
                
                validation = self.validate_project_path(parent)
                if validation['exists'] and validation['is_directory']:
                    suggestions.append({
                        'path': parent,
                        'name': f"Parent of {Path(project.path).name}",
                        'source': 'claude_parent',
                        'session_count': 0,
                        'last_activity': 'Scan target',
                        'validation': validation
                    })
        
        # Add common project directories
        for common_dir in self.find_common_project_directories():
            if common_dir not in [s['path'] for s in suggestions]:
                validation = self.validate_project_path(common_dir)
                suggestions.append({
                    'path': common_dir,
                    'name': Path(common_dir).name,
                    'source': 'common_directory',
                    'session_count': 0,
                    'last_activity': 'Scan target',
                    'validation': validation
                })
        
        return suggestions


# Global discovery instance
_project_discovery_instance = None

def get_project_discovery() -> ProjectDiscovery:
    """Get global project discovery instance"""
    global _project_discovery_instance
    if _project_discovery_instance is None:
        _project_discovery_instance = ProjectDiscovery()
    return _project_discovery_instance