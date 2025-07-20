#!/usr/bin/env python3
"""
Cafedelic MCP Server Adapter
Skeleton interface for connecting Textual UI to cafedelic's MCP intelligence server.
"""

import requests
from typing import Dict, Any, Optional


class CafedelicMCPAdapter:
    """
    Skeleton adapter for communicating with cafedelic's MCP server.
    This will eventually provide real-time intelligence data to the Textual UI.
    """
    
    def __init__(self, mcp_host: str = "localhost", mcp_port: int = 3001):
        self.base_url = f"http://{mcp_host}:{mcp_port}"
        self.session = requests.Session()
    
    def get_health(self) -> Dict[str, Any]:
        """Check if the MCP server is running."""
        try:
            response = self.session.get(f"{self.base_url}/health", timeout=5)
            return response.json()
        except Exception as e:
            return {"status": "error", "error": str(e)}
    
    def get_projects(self) -> Dict[str, Any]:
        """Get list of active projects from Claude Code filesystem."""
        try:
            response = self.session.get(f"{self.base_url}/api/projects", timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            return {"projects": [], "error": str(e)}
    
    def get_sessions(self) -> Dict[str, Any]:
        """Get current sessions from Claude Code filesystem."""
        try:
            response = self.session.get(f"{self.base_url}/api/sessions", timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            return {"sessions": [], "error": str(e)}
    
    def get_session_summary(self) -> Dict[str, Any]:
        """Get session summary statistics."""
        try:
            response = self.session.get(f"{self.base_url}/api/sessions/summary", timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            return {"totalSessions": 0, "activeSessions": 0, "totalProjects": 0, "totalCost": 0, "error": str(e)}


# Singleton instance for the UI to use
mcp_adapter = CafedelicMCPAdapter()