#!/usr/bin/env python3
"""
Mock data for Cafedelic delegation platform demo
Provides sample sessions, tasks, and delegation states for UI development.
"""

from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Dict, Any

@dataclass
class MockSession:
    """Mock Claude Code session data"""
    id: str
    name: str
    project: str
    status: str  # 'planning', 'analyzing', 'implementing', 'reviewing', 'stuck', 'available'
    task: str
    progress: float  # 0.0 to 1.0
    duration: str
    last_activity: datetime
    files_context: List[str]
    
@dataclass 
class MockTask:
    """Mock delegation task data"""
    id: str
    description: str
    session_id: str
    status: str  # 'pending', 'delegated', 'in_progress', 'completed', 'failed'
    priority: str  # 'high', 'medium', 'low'
    created_at: datetime
    
@dataclass
class MockCommand:
    """Mock project command data"""
    name: str
    description: str
    category: str  # 'workflow', 'test', 'deploy', 'analyze'

@dataclass
class MockProject:
    """Mock project data"""
    name: str
    path: str
    status: str  # 'active', 'idle', 'issues'
    sessions: List[str]  # session IDs
    activity_level: int  # 0-3 (●●●)
    
@dataclass
class MockFile:
    """Mock file data"""
    name: str
    path: str
    status: str  # 'modified', 'created', 'read', 'referenced'
    in_context: bool
    activity_level: int  # 0-3 (●●●)

# Sample session data
MOCK_SESSIONS = [
    MockSession(
        id="abc123",
        name="auth-refactor",
        project="webapp",
        status="planning",
        task="Implement OAuth2 authentication flows",
        progress=0.8,
        duration="3h 42m",
        last_activity=datetime.now() - timedelta(minutes=5),
        files_context=["src/auth/oauth.js", "src/middleware/auth.js", "tests/auth.test.js"]
    ),
    MockSession(
        id="def456", 
        name="ui-components",
        project="webapp",
        status="analyzing",
        task="Add dark theme toggle and accessibility",
        progress=0.3,
        duration="1h 15m",
        last_activity=datetime.now() - timedelta(minutes=12),
        files_context=["src/components/ThemeToggle.jsx", "src/styles/themes.css"]
    ),
    MockSession(
        id="ghi789",
        name="database-opt",
        project="webapp", 
        status="stuck",
        task="Optimize user query performance",
        progress=0.6,
        duration="2h 30m",
        last_activity=datetime.now() - timedelta(minutes=25),
        files_context=["src/db/queries.sql", "src/models/User.js"]
    ),
    MockSession(
        id="jkl012",
        name="mobile-app",
        project="mobile",
        status="available",
        task="No current task",
        progress=0.0,
        duration="0m",
        last_activity=datetime.now() - timedelta(hours=2),
        files_context=[]
    )
]

# Sample task data
MOCK_TASKS = [
    MockTask(
        id="task_001",
        description="Run comprehensive authentication tests",
        session_id="abc123",
        status="completed",
        priority="high",
        created_at=datetime.now() - timedelta(minutes=30)
    ),
    MockTask(
        id="task_002", 
        description="Implement dark theme toggle UI component",
        session_id="def456",
        status="in_progress",
        priority="medium",
        created_at=datetime.now() - timedelta(minutes=15)
    ),
    MockTask(
        id="task_003",
        description="Analyze database query bottlenecks",
        session_id="ghi789", 
        status="failed",
        priority="high",
        created_at=datetime.now() - timedelta(minutes=10)
    ),
    MockTask(
        id="task_004",
        description="Set up mobile app project structure",
        session_id="",
        status="pending",
        priority="low",
        created_at=datetime.now() - timedelta(minutes=5)
    )
]

# Sample project commands
MOCK_COMMANDS = [
    MockCommand("/plan", "Initiate planning phase for feature", "workflow"),
    MockCommand("/analyze", "Begin analysis of requirements/codebase", "workflow"), 
    MockCommand("/act", "Execute planned implementation", "workflow"),
    MockCommand("/auth-tests", "Run comprehensive authentication tests", "test"),
    MockCommand("/deploy-staging", "Deploy current branch to staging", "deploy"),
    MockCommand("/security-audit", "Run security analysis on components", "analyze"),
    MockCommand("/keep-planning", "Continue planning without execution", "workflow"),
    MockCommand("/keep-analyzing", "Extend analysis phase", "workflow"),
    MockCommand("/review", "Review completed work", "workflow"),
    MockCommand("/coordinate", "Identify cross-session dependencies", "workflow")
]

def get_sessions() -> List[MockSession]:
    """Get all mock sessions"""
    return MOCK_SESSIONS

def get_session_by_id(session_id: str) -> MockSession | None:
    """Get session by ID"""
    return next((s for s in MOCK_SESSIONS if s.id == session_id), None)

def get_tasks() -> List[MockTask]:
    """Get all mock tasks"""
    return MOCK_TASKS

def get_commands() -> List[MockCommand]:
    """Get all mock commands"""
    return MOCK_COMMANDS

def get_active_sessions() -> List[MockSession]:
    """Get sessions that are currently active (not available)"""
    return [s for s in MOCK_SESSIONS if s.status != 'available']

def get_session_status_emoji(status: str) -> str:
    """Get emoji for session status"""
    status_emojis = {
        'planning': '🧠',
        'analyzing': '🔍', 
        'implementing': '⚡',
        'reviewing': '👀',
        'stuck': '⚠️',
        'available': '💤'
    }
    return status_emojis.get(status, '❓')

def get_task_status_emoji(status: str) -> str:
    """Get emoji for task status"""
    task_emojis = {
        'pending': '⏳',
        'delegated': '📤',
        'in_progress': '🔄', 
        'completed': '✅',
        'failed': '❌'
    }
    return task_emojis.get(status, '❓')

# Sample project data
MOCK_PROJECTS = [
    MockProject(
        name="webapp",
        path="/home/alex/projects/webapp",
        status="active", 
        sessions=["abc123", "def456", "ghi789"],
        activity_level=3
    ),
    MockProject(
        name="api-server",
        path="/home/alex/projects/api-server",
        status="issues",
        sessions=["xyz999"],
        activity_level=1
    ),
    MockProject(
        name="mobile",
        path="/home/alex/projects/mobile",
        status="idle",
        sessions=["jkl012"],
        activity_level=0
    )
]

# Sample file data
MOCK_FILES = [
    # webapp files
    MockFile("oauth.js", "src/auth/oauth.js", "modified", True, 3),
    MockFile("middleware.js", "src/auth/middleware.js", "read", True, 2),
    MockFile("tokens.js", "src/auth/tokens.js", "created", False, 1),
    MockFile("auth.test.js", "tests/auth.test.js", "created", True, 3),
    MockFile("ThemeToggle.jsx", "src/components/ThemeToggle.jsx", "modified", True, 2),
    MockFile("themes.css", "src/styles/themes.css", "modified", True, 1),
    MockFile("queries.sql", "src/db/queries.sql", "referenced", True, 1),
    MockFile("User.js", "src/models/User.js", "read", True, 1),
    MockFile("package.json", "package.json", "read", False, 0),
    
    # api-server files  
    MockFile("auth.js", "src/routes/auth.js", "modified", True, 1),
    MockFile("middleware.js", "src/middleware/auth.js", "read", False, 0),
    
    # mobile files (minimal)
    MockFile("App.js", "src/App.js", "read", False, 0),
    MockFile("package.json", "package.json", "read", False, 0)
]

def get_projects() -> List[MockProject]:
    """Get all mock projects"""
    return MOCK_PROJECTS

def get_project_by_name(name: str) -> MockProject | None:
    """Get project by name"""
    return next((p for p in MOCK_PROJECTS if p.name == name), None)

def get_files() -> List[MockFile]:
    """Get all mock files"""
    return MOCK_FILES

def get_files_by_project(project_name: str) -> List[MockFile]:
    """Get files for a specific project"""
    # Simple filtering based on common patterns
    if project_name == "webapp":
        return [f for f in MOCK_FILES if not f.path.startswith("src/routes/") and f.name != "App.js"]
    elif project_name == "api-server":
        return [f for f in MOCK_FILES if f.path.startswith("src/routes/") or f.path.startswith("src/middleware/")]
    elif project_name == "mobile":
        return [f for f in MOCK_FILES if f.name == "App.js" or (f.name == "package.json" and project_name == "mobile")]
    return []

def get_context_files_by_session(session_id: str) -> List[MockFile]:
    """Get files currently in context for a session"""
    session = get_session_by_id(session_id)
    if not session:
        return []
    
    context_paths = session.files_context
    return [f for f in MOCK_FILES if any(path in f.path for path in context_paths)]

def get_activity_emoji(level: int) -> str:
    """Get activity level emoji representation"""
    if level == 3:
        return "●●●"
    elif level == 2:
        return "●●○"
    elif level == 1:
        return "●○○"
    else:
        return "○○○"