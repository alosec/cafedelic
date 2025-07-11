# UI Mockups V2: Cafedelic Intelligence Platform

## Overview

This document provides detailed UI mockups for the Cafedelic Intelligence Platform using a configurable panel system. The interface prioritizes intelligence-first architecture with reactive components and task delegation capabilities.

## Design Philosophy

### Intelligence-First Panel System
- **LeftPanel (30%)**: ProjectTree + FileTree + SessionTree with DirectoryTree widget
- **MainViewFrame (70%)**: Central workspace with TabbedContent for multiple view types
- **Reactive Components**: Database-driven UI with automatic updates
- **Task Delegation**: Quick-chat interface for natural language task assignment

### Core Principles
- Start simple: 30% left tree, 70% main content
- Focus on intelligence layer over layout presets
- Prioritize basic foundation before advanced configurations
- Emphasize functional delegation over visual complexity

## Primary Layout: Resizable Two-Panel System

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ ♦ Cafedelic Task Delegation    [Ctrl+T] Task [Ctrl+N] New Session    17:42 │ Header
├─────────────────────────────────────────────────────────────────────────────┤
│ Quick Delegate: [Type task here...] → [Session ▼] [Find Similar] [Send]    │ Quick Delegation
├─────────────────────────┬⋮⋮⋮┬─────────────────────────────────────────────────┤
│ LeftPanel (resizable)   │ │ │ MainViewFrame (resizable)                     │
│                         │ │ │                                               │
│ 📁 MyProject ●●●       │ │ │ ┌─ SessionViewPane: auth-refactor ──────────┐ │
│ ├─ 🎯 SessionTree      │ │ │ │ Status: ● Planning    Duration: 3h 42m     │ │
│ │  ├─ ● auth-refactor  │ │ │ │ Task: Implement OAuth2 authentication      │ │
│ │  │  └─ [Open Chat]   │ │ │ │                                             │ │
│ │  ├─ ○ ui-components  │ │ │ │ [Open Interactive Chat] → TmuxViewPane     │ │
│ │  │  └─ [Open Chat]   │ │ │ │                                             │ │
│ │  └─ + New Session    │ │ │ │ Workflow Commands:                          │ │
│ │                      │ │ │ │ [/plan] [/analyze] [/act] [/coordinate]     │ │
│ └─ 📁 FileTree         │ │ │ │                                             │ │
│    ├─ 📂 In Context    │ │ │ │ Context Files:                              │ │
│    │  ├─ oauth.js ●●● │ │ │ │ 📄 src/auth/oauth.js ●●● [Modified 5m ago] │ │
│    │  └─ auth.test ○● │ │ │ │ 📄 tests/auth.test.js ○●● [Tests running]  │ │
│    └─ 📂 All Files     │ │ │ │                                             │ │
│       ├─ 📁 src/       │ │ │ │ Activity Feed:                              │ │
│       └─ 📁 tests/     │ │ │ │ [17:42] Modified oauth.js (3s ago)         │ │
│                         │ │ │ │ [17:41] Created test suite (2m ago)        │ │
│ 📁 api-server ⚠○○     │ │ │ │                                             │ │
│ ├─ 🎯 SessionTree      │ │ │ │ Health: ● Excellent  Progress: ████████░░  │ │
│ │  └─ ⚠ debug-1       │ │ │ └─────────────────────────────────────────────┘ │
│ │     └─ [Open Chat]   │ │ │                                               │
│ └─ 📁 FileTree         │ │ │ Alternative View: TmuxViewPane                │
│    └─ 📂 All Files     │ │ │ ┌─ claude --resume auth-refactor ───────────┐ │
│                         │ │ │ │ $ claude --resume auth-refactor            │ │
│                         │ │ │ │ ┌─ Claude Code Chat ─────────────────────┐ │ │
│                         │ │ │ │ │ > /plan                                 │ │ │
│                         │ │ │ │ │ I'll help you plan the OAuth2...        │ │ │
│                         │ │ │ │ │ > /act                                  │ │ │
│                         │ │ │ │ │ [Live tmux session interaction]         │ │ │
│                         │ │ │ │ └─────────────────────────────────────────┘ │ │
│                         │ │ │ │ [Managed via libtmux integration]         │ │
│                         │ │ │ └─────────────────────────────────────────────┘ │
├─────────────────────────┴─┴─┴─────────────────────────────────────────────────┤
│ Status: 3 projects │ 4 sessions │ 2 active │ Health: ● Good │ [?] Help     │ Footer
└─────────────────────────────────────────────────────────────────────────────┘
```

**Layout Features:**
- **Resizable Panels**: Drag handle (⋮⋮⋮) allows mouse-draggable width adjustment
- **Default Split**: 30% LeftPanel, 70% MainViewFrame (configurable)
- **Clean Hierarchy**: Sessions and FileTree at same level under each project
- **Direct Chat Access**: [Open Chat] buttons launch TmuxViewPane with `claude --resume`

## LeftPanel: Unified Project Structure

### Hierarchical Tree Layout
```
LeftPanel (configurable width, default 30%)
├─ ProjectTree (DirectoryTree widget)
│  ├─ 📁 MyProject ●●● (click → ProjectViewPane)
│  │  ├─ 🎯 SessionTree (click → SessionViewPane)
│  │  │  ├─ ● auth-refactor (active)
│  │  │  │  └─ [Open Chat] → TmuxViewPane
│  │  │  ├─ ○ ui-components (idle)
│  │  │  │  └─ [Open Chat] → TmuxViewPane
│  │  │  └─ + New Session
│  │  │
│  │  └─ 📁 FileTree
│  │     ├─ 📂 In Context (session-aware files)
│  │     │  ├─ 📄 src/auth/oauth.js ●●●
│  │     │  ├─ 📄 src/auth/middleware.js ●●○
│  │     │  └─ 📄 tests/auth.test.js ○○●
│  │     │
│  │     └─ 📂 All Files (full project structure)
│  │        ├─ 📁 src/
│  │        │  ├─ 📁 auth/
│  │        │  ├─ 📁 components/
│  │        │  └─ 📁 utils/
│  │        ├─ 📁 tests/
│  │        ├─ 📁 docs/
│  │        └─ 📄 package.json
│  │
│  ├─ 📁 api-server ⚠○○ (click → ProjectViewPane)
│  │  ├─ 🎯 SessionTree
│  │  │  ├─ ⚠ debug-session (stuck)
│  │  │  │  └─ [Open Chat] → TmuxViewPane
│  │  │  └─ + New Session
│  │  │
│  │  └─ 📁 FileTree
│  │     ├─ 📂 In Context
│  │     │  └─ 📄 src/routes/auth.js ⚠○○
│  │     └─ 📂 All Files
│  │        ├─ 📁 src/
│  │        └─ 📁 tests/
│  │
│  └─ 📁 mobile ○○○ (click → ProjectViewPane)
│     ├─ 🎯 SessionTree
│     │  └─ + New Session (no active sessions)
│     │
│     └─ 📁 FileTree
│        ├─ 📂 In Context (empty)
│        └─ 📂 All Files
│           ├─ 📁 src/
│           └─ 📁 ios/
│
└─ [Drag handle for resizing] ⋮⋮⋮
```

## MainViewFrame Content Types

### SessionViewPane: Individual Session Management
Opened when clicking on SessionTree or individual sessions.

**Components:**
- Session header with status, duration, keyboard shortcuts
- Workflow command buttons (/plan, /analyze, /act, /review, /coordinate)
- Context files list with activity indicators
- Real-time activity feed with 3-second auto-refresh
- Session health metrics and progress bars
- **[Open Interactive Chat]** button → launches TmuxViewPane

### TmuxViewPane: Direct Claude Chat Interface
Opened via [Open Chat] button from SessionTree, uses libtmux integration.

**Implementation:**
- Executes `claude --resume {session_id}` in tmux pane
- Direct terminal interaction with Claude Code
- Managed via libtmux for programmatic control
- Full-screen chat experience within MainViewFrame

**Future Enhancement: CafeChatPane**
- Custom UI wrapper over Claude chat interface
- Reactive UX by monitoring session logs
- Real-time transformation and database integration
- Enhanced task-based interaction patterns
- Seamless integration with delegation workflows

### ProjectViewPane: Single Project Overview
Opened when clicking on project name (e.g., "MyProject").

**Components:**
- Project metadata (path, git status, last activity)
- Session overview with health indicators
- File activity summary with recent changes
- Project-specific intelligence insights
- Quick actions for project management

### DashboardViewFrame: System Overview
High-level system monitoring and task queue management.

**Components:**
- Active task queue with priorities
- System health metrics (CPU, memory, response times)
- Database performance indicators
- Overall status and alerts

## RightPanel Specifications

*Note: RightPanel implementation is deferred in favor of focusing on core intelligence layer*

Future considerations:
- Intelligence feed overlay
- Coordination alerts
- Session health monitoring
- Activity streams

## BottomPanel with Z-Index Overlays

*Note: BottomPanel complexity is deferred for initial implementation*

Future considerations:
- Terminal output overlay
- Multi-session logs
- Configurable z-index behavior
- Panel visibility toggles

## Quick Delegation Widget

```
Quick Delegate Interface (Header area)
├─ Input Field: [Type task here...]
├─ Session Selector: [Session ▼]
│  ├─ webapp/auth-refactor ● (active)
│  ├─ webapp/ui-components ○ (idle)
│  ├─ api-server/debug-1 ⚠ (stuck)
│  └─ [+ New Session]
├─ Actions: [Find Similar] [Send]
└─ Auto-suggestions based on find_relevant_chats()
```

**Features:**
- Natural language task input
- Smart session targeting with status indicators
- Context-aware suggestions
- Integration with find_relevant_chats() function

## Reactive Data Binding

### Multi-Rate Updates
- Active tasks: 1-second updates for progress
- Session status: 3-second updates for health
- Project stats: 30-second updates for overview
- Database sync: 5-second polling with caching

### Performance Targets
- Database queries: <50ms response time
- Memory usage: <100MB for typical workloads
- UI responsiveness: Real-time updates without blocking

## Keyboard Shortcuts and Focus Management

### Primary Shortcuts
- `Ctrl+T`: Create new task/session
- `Ctrl+N`: New project
- `Ctrl+F`: Find relevant chats
- `Ctrl+W`: Close current session tab
- `Ctrl+Tab`: Switch between session tabs
- `Tab/Shift+Tab`: Navigate between panels

### Focus Flow
1. Quick delegation input (primary focus)
2. LeftPanel tree navigation
3. MainViewFrame session content
4. Footer status actions

## Widget Implementation Guide

### Textual Widget Mapping
- **LeftPanel**: `Vertical` container with `DirectoryTree` (unified project structure)
- **MainViewFrame**: `TabbedContent` with dynamic view types (Session, Project, Tmux, CafeChat)
- **Quick Delegation**: `Input` widget with session dropdown
- **Activity Feed**: `RichLog` with auto-scroll
- **Health Indicators**: Custom widgets with color coding
- **Progress Bars**: `ProgressBar` widget with reactive updates
- **Resizable Panels**: `Horizontal` with drag handle for width adjustment
- **TmuxViewPane**: Terminal widget with libtmux integration

### Component Architecture
```python
class CafedelicApp(App):
    def compose(self) -> ComposeResult:
        yield Header()
        yield QuickDelegationWidget()
        with Horizontal():
            yield LeftPanel(classes="left-panel")  # resizable, default 30%
            yield ResizeDragHandle()  # ⋮⋮⋮ mouse-draggable
            yield MainViewFrame(classes="main-view")  # resizable, default 70%
        yield Footer()

class LeftPanel(Widget):
    """Unified project structure with sessions and files at same level"""
    def compose(self) -> ComposeResult:
        yield ProjectDirectoryTree()  # Custom DirectoryTree with:
                                      # - Project nodes (click → ProjectViewPane)
                                      # - SessionTree nodes (click → SessionViewPane)  
                                      # - FileTree with In Context / All Files
                                      # - [Open Chat] buttons → TmuxViewPane

class MainViewFrame(Widget):
    def compose(self) -> ComposeResult:
        with TabbedContent():
            yield SessionViewPane()     # Session management + [Open Interactive Chat]
            yield TmuxViewPane()        # claude --resume via libtmux
            yield ProjectViewPane()     # Single project overview
            yield DashboardViewFrame()  # System overview
            # Future: yield CafeChatPane()  # Custom chat wrapper

class TmuxViewPane(Widget):
    """Direct Claude Code chat via tmux integration"""
    def __init__(self, session_id: str):
        self.session_id = session_id
        super().__init__()
    
    def compose(self) -> ComposeResult:
        # Execute: claude --resume {session_id}
        # Managed via libtmux for programmatic control
        yield TerminalWidget(command=f"claude --resume {self.session_id}")

class CafeChatPane(Widget):
    """Future: Custom UI wrapper over Claude chat"""
    def compose(self) -> ComposeResult:
        # Monitor session logs, transform, integrate with database
        # Reactive UX with task-based interaction patterns
        yield ChatInterface()
        yield TaskDelegationOverlay()
        yield DatabaseIntegration()
```

### Interaction Flow
1. **Project Selection**: Click project name → opens ProjectViewPane
2. **Session Selection**: Click SessionTree → opens SessionViewPane  
3. **Direct Chat**: Click [Open Chat] → opens TmuxViewPane with `claude --resume`
4. **File Context**: In Context shows session-aware files, All Files shows full structure
5. **Panel Resizing**: Drag handle (⋮⋮⋮) adjusts LeftPanel/MainViewFrame widths

## Implementation Priority

### Phase 1: Foundation (Focus Area)
1. Simple 30/70 split layout
2. Basic ProjectTree with DirectoryTree widget
3. SessionViewPane with TabbedContent
4. Quick delegation input
5. Real-time activity feed

### Phase 2: Intelligence Layer
1. Database-driven reactive components
2. Session health monitoring
3. Task delegation routing
4. find_relevant_chats() integration

### Phase 3: Advanced Features
1. Configurable panel system
2. Layout presets (kept minimal)
3. Advanced coordination features
4. Performance optimization

This simplified approach prioritizes the intelligence layer and core delegation functionality over complex layout configurations, providing a solid foundation for iterative development.