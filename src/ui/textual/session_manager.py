#!/usr/bin/env python3
"""
Tmux session manager with libtmux integration
Handles Claude Code session lifecycle and switching.
"""

import libtmux
import subprocess
import os
import signal
from pathlib import Path
from typing import Optional, Dict, List
from dataclasses import dataclass

from src.database.session_db import get_database, Session


@dataclass
class TmuxSessionInfo:
    """Information about a tmux session"""
    session_name: str
    window_id: Optional[str] = None
    pane_id: Optional[str] = None
    is_attached: bool = False
    is_running: bool = False


class TmuxSessionManager:
    """Manages Claude Code sessions within tmux"""
    
    def __init__(self):
        self.server = libtmux.Server()
        self.db = get_database()
        self.current_session_id = None
        
    def get_tmux_session_name(self, session_id: str) -> str:
        """Generate tmux session name for a Claude Code session"""
        return f"cafedelic-{session_id}"
    
    def is_session_running(self, session_id: str) -> bool:
        """Check if tmux session is running for this Claude session"""
        tmux_name = self.get_tmux_session_name(session_id)
        try:
            session = self.server.get_session(tmux_name)
            return session is not None
        except:
            return False
    
    def get_session_info(self, session_id: str) -> Optional[TmuxSessionInfo]:
        """Get tmux session information"""
        tmux_name = self.get_tmux_session_name(session_id)
        try:
            session = self.server.get_session(tmux_name)
            if session:
                # Check if session is attached
                attached = session.attached_session() is not None
                
                # Get first window and pane info
                windows = session.list_windows()
                window_id = windows[0].get('window_id') if windows else None
                
                panes = session.list_panes()
                pane_id = panes[0].get('pane_id') if panes else None
                
                return TmuxSessionInfo(
                    session_name=tmux_name,
                    window_id=window_id,
                    pane_id=pane_id,
                    is_attached=attached,
                    is_running=True
                )
        except:
            pass
        
        return None
    
    def create_claude_session(self, session_id: str, project_path: str = None) -> bool:
        """Create a new tmux session for Claude Code"""
        tmux_name = self.get_tmux_session_name(session_id)
        
        # Get session info from database
        session = self.db.get_session_by_id(session_id)
        if not session:
            return False
        
        try:
            # Create new tmux session
            tmux_session = self.server.new_session(
                session_name=tmux_name,
                start_directory=project_path or str(Path.home()),
                attach=False  # Don't attach immediately
            )
            
            # Get window and pane info
            window = tmux_session.attached_window()
            pane = window.attached_pane() if window else None
            
            # Update database with tmux info
            self.db.update_session_tmux_info(
                session_id=session_id,
                tmux_session_name=tmux_name,
                tmux_window_id=window.get('window_id') if window else None,
                tmux_pane_id=pane.get('pane_id') if pane else None
            )
            
            # Update session status
            self.db.update_session_status(session_id, 'detached')
            
            return True
            
        except Exception as e:
            print(f"Error creating tmux session: {e}")
            return False
    
    def open_claude_session(self, session_id: str, resume: bool = True) -> bool:
        """Open/resume a Claude Code session"""
        session = self.db.get_session_by_id(session_id)
        if not session:
            return False
        
        # Get project path
        project = self.db.get_project_by_id(session.project_short_id)
        project_path = project.path if project else None
        
        # Check if tmux session already exists
        if not self.is_session_running(session_id):
            # Create new tmux session
            if not self.create_claude_session(session_id, project_path):
                return False
        
        tmux_name = self.get_tmux_session_name(session_id)
        
        try:
            # Get the tmux session
            tmux_session = self.server.get_session(tmux_name)
            window = tmux_session.attached_window()
            pane = window.attached_pane() if window else None
            
            if resume and session.claude_session_id:
                # Launch Claude Code with --resume
                claude_cmd = f"claude --resume {session.claude_session_id}"
            else:
                # Launch new Claude Code session
                claude_cmd = f"cd {project_path} && claude"
            
            # Send command to pane
            if pane:
                pane.send_keys(claude_cmd)
            
            # Update session status
            self.db.update_session_status(session_id, 'active')
            self.current_session_id = session_id
            
            # Add activity log
            self.db.add_activity(
                session_id=session_id,
                activity_type='resume',
                description=f"Opened Claude Code session {'with resume' if resume else 'new'}"
            )
            
            return True
            
        except Exception as e:
            print(f"Error opening Claude session: {e}")
            return False
    
    def attach_session(self, session_id: str) -> bool:
        """Attach to tmux session (switch to it)"""
        if not self.is_session_running(session_id):
            return False
        
        tmux_name = self.get_tmux_session_name(session_id)
        
        try:
            # Use tmux client to attach
            result = subprocess.run(['tmux', 'attach-session', '-t', tmux_name], 
                                  capture_output=False, check=True)
            
            # Update session status when we return
            self.db.update_session_status(session_id, 'detached')
            self.current_session_id = None
            
            return True
            
        except subprocess.CalledProcessError:
            return False
    
    def detach_session(self, session_id: str) -> bool:
        """Detach from tmux session (but keep it running)"""
        if not self.is_session_running(session_id):
            return False
        
        tmux_name = self.get_tmux_session_name(session_id)
        
        try:
            # Send detach command
            subprocess.run(['tmux', 'detach-session', '-t', tmux_name], 
                          capture_output=True, check=True)
            
            # Update session status
            self.db.update_session_status(session_id, 'detached')
            
            return True
            
        except subprocess.CalledProcessError:
            return False
    
    def kill_session(self, session_id: str) -> bool:
        """Kill tmux session completely"""
        tmux_name = self.get_tmux_session_name(session_id)
        
        try:
            # Kill tmux session
            subprocess.run(['tmux', 'kill-session', '-t', tmux_name], 
                          capture_output=True, check=True)
            
            # Update database
            self.db.update_session_status(session_id, 'available')
            self.db.update_session_tmux_info(session_id, None, None, None)
            
            # Add activity log
            self.db.add_activity(
                session_id=session_id,
                activity_type='command',
                description="Session terminated"
            )
            
            return True
            
        except subprocess.CalledProcessError:
            return False
    
    def send_exit_command(self, session_id: str) -> bool:
        """Send graceful exit command to Claude Code session"""
        if not self.is_session_running(session_id):
            return False
        
        tmux_name = self.get_tmux_session_name(session_id)
        
        try:
            # Get session and send Ctrl+C to stop Claude Code
            tmux_session = self.server.get_session(tmux_name)
            window = tmux_session.attached_window()
            pane = window.attached_pane() if window else None
            
            if pane:
                # Send Ctrl+C to interrupt Claude
                pane.send_keys('C-c', enter=False)
                
                # Wait a moment then send exit
                import time
                time.sleep(0.5)
                pane.send_keys('exit')
            
            return True
            
        except Exception as e:
            print(f"Error sending exit command: {e}")
            return False
    
    def list_running_sessions(self) -> List[Dict]:
        """List all running tmux sessions for Cafedelic"""
        running_sessions = []
        
        # Get all sessions from database
        sessions = self.db.get_sessions()
        
        for session in sessions:
            info = self.get_session_info(session.short_id)
            if info and info.is_running:
                running_sessions.append({
                    'session_id': session.short_id,
                    'name': session.name,
                    'project': session.project_name,
                    'status': session.status,
                    'tmux_info': info
                })
        
        return running_sessions
    
    def setup_signal_handlers(self):
        """Setup signal handlers for graceful session management"""
        def handle_sigterm(signum, frame):
            # Detach all sessions on exit
            for session in self.list_running_sessions():
                self.detach_session(session['session_id'])
        
        signal.signal(signal.SIGTERM, handle_sigterm)
        signal.signal(signal.SIGINT, handle_sigterm)


# Global session manager instance
_session_manager = None

def get_session_manager() -> TmuxSessionManager:
    """Get global session manager instance"""
    global _session_manager
    if _session_manager is None:
        _session_manager = TmuxSessionManager()
        _session_manager.setup_signal_handlers()
    return _session_manager