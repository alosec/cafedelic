# UI Mockups: Cafedelic Intelligence Platform

## Overview

This document provides detailed UI mockups and layout specifications for the Cafedelic Intelligence Platform TUI. All mockups use ASCII art to represent the terminal interface and specify exact Textual widget usage.

## Main Dashboard Layout

### Full Application Layout
```
┌─────────────────────────────────────────────────────────────────────────────┐
│ ♦ Cafedelic Task Delegation Platform             [17:42] │ ● 2 active tasks │ Header
├─────────────────────────────────────────────────────────────────────────────┤
│ Quick Delegate: [Type task here...] → [Session ▼] [Find Similar] [Send]    │ Quick Chat
├─────────────────────────────────────────────────────────────────────────────┤
│ [Projects] [auth: Planning ● ] [ui: Analyzing ○ ] [db: Stuck ⚠ ] [+ New]   │ Tab Bar
├──────────────┬──────────────────────────────────────────────────────────────┤
│              │ ┌──────────────────────────────────────────────────────────┐ │
│  Projects    │ │ Session: auth-refactor                    ● Planning Phase │ │
│              │ │ Project: /home/alex/projects/webapp              14:23:15 │ │
│ ┌─ webapp    │ │ Delegated Task: Implement OAuth2 authentication flows    │ │
│              │ │ Status: Planning... security architecture patterns       │ │
│ ├─ api-srv   │ │                                                           │ │
│ ├─ frontend  │ │ Files in Context:                                        │ │
│ └─ mobile    │ │ ├── 📄 src/auth/oauth.js        ●●○ [Modified Today]    │ │
│              │ │ ├── 📄 src/middleware/auth.js   ●○○ [Recently Read]     │ │
│              │ │ ├── 📄 tests/auth.test.js       ○○● [Tests Created]     │ │
│  Sessions    │ │ └── 📄 docs/oauth-spec.md       ○●○ [Referenced]        │ │
│              │ │                                                           │ │
│ ● webapp/1   │ │ Recent Activity:                                         │ │
│ ○ webapp/2   │ │ [17:41] Reading OAuth2 specification documentation       │ │
│ ⚠ api-srv/1  │ │ [17:39] Modified authentication middleware structure     │ │
│              │ │ [17:37] Created comprehensive test suite for OAuth       │ │
│  Health      │ │ [17:35] Analyzed security implications of token storage  │ │
│              │ │                                                           │ │
│ ● 2 Active   │ │ Session Health: ● Excellent    Progress: ████████░░ 80% │ │
│ ○ 1 Idle     │ │ Duration: 3h 42m               Files: 15    Commits: 7  │ │
│ ⚠ 1 Issues   │ └──────────────────────────────────────────────────────────┘ │
├──────────────┴──────────────────────────────────────────────────────────────┤
│ Task Delegation Feed:                                [Filter: All] [Search] │
│ [17:42] 🔄 auth-refactor: Ready for /act command - plan complete          │
│ [17:41] ✓ ui-components: Task completed - authentication UI ready          │
│ [17:40] 📝 database-opt: Analyzing... query optimization strategies        │
│ [17:39] ⚠ database-opt: Stuck - requires coordination with auth session   │
│ [17:38] 🎯 Task suggested: Handoff auth tokens → database-opt session     │
├─────────────────────────────────────────────────────────────────────────────┤
│ Status: 3 projects │ 4 sessions │ 2 active │ Health: ● Good │ [Q]uit [?]Help │ Footer
└─────────────────────────────────────────────────────────────────────────────┘
```

### Widget Mapping
- **Header**: Custom header with title, clock, and session count
- **Quick Chat**: Text input with session selector and find_relevant_chats() integration
- **Tab Bar**: `TabbedContent` with task-state indicators and delegation controls
- **Left Sidebar**: `Vertical` container with multiple sections
- **Main Content**: Tab-specific content area with session details
- **Task Delegation Feed**: `ScrollView` with task status updates and coordination alerts
- **Footer**: Status bar with quick stats and keyboard shortcuts

## Tab Content Layouts

### Task Delegation Session Tab
```
┌──────────────────────────────────────────────────────────────────────────┐
│ Session: auth-refactor [abc123]                      ● Planning Phase   │
│ Delegated Task: Implement OAuth2 authentication flows      Duration: 3h │
├──────────────────────────────────────────────────────────────────────────┤
│ ┌─ Task Status ───────────────────┐ ┌─ Delegation Controls ─────────────────┐ │
│ │ Status: Planning...            │ │ [/plan] [/analyze] [/act]      │ │
│ │ Current: Security patterns     │ │ [Send Message] [Handoff]       │ │
│ │ Progress: ████░░░░░░ 40%        │ │ [Find Similar] [Duplicate]     │ │
│ │ Ready for: /act command        │ │ Priority: High ▲              │ │
│ └────────────────────────────────┘ └────────────────────────────────┘ │
│                                                                              │
│ ┌─ Files in Context ─────────────────────────────────────────────────────┐ │
│ │ 📁 src/auth/                                                           │ │
│ │ ├── 📄 oauth.js              ●●● Modified 5 min ago    [Lines: 45-67] │ │
│ │ ├── 📄 middleware.js         ●●○ Read 15 min ago       [Lines: 12-34] │ │
│ │ └── 📄 tokens.js             ●○○ Created 2h ago        [Lines: 1-89]  │ │
│ │ 📁 tests/                                                              │ │
│ │ ├── 📄 auth.test.js          ○○● Running tests        [All Tests]    │ │
│ │ └── 📄 oauth.test.js         ○●○ Test created         [Lines: 1-156] │ │
│ │ 📁 docs/                                                               │ │
│ │ └── 📄 oauth-spec.md         ○●○ Referenced          [Section: 3.2] │ │
│ └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│ ┌─ Recent Activity (Last 1 hour) ────────────────────────────────────────┐ │
│ │ [17:41] ● Modified src/auth/oauth.js - Added token refresh logic      │ │
│ │ [17:39] ● Created tests/oauth.test.js - Comprehensive test coverage   │ │
│ │ [17:37] ○ Read docs/oauth-spec.md - Section 3.2 (Token Lifecycle)    │ │
│ │ [17:35] ● Modified src/auth/middleware.js - Enhanced error handling   │ │
│ │ [17:33] ● Committed changes - "Implement OAuth token validation"      │ │
│ │ [17:30] ○ Read external docs - OAuth2 RFC 6749 specification          │ │
│ │ [16:58] ● Created src/auth/tokens.js - Token management utilities     │ │
│ └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│ [Delegation: [Send /act] [Handoff Task] [Find Context] [Monitor Progress]] │
└──────────────────────────────────────────────────────────────────────────┘
```

### Project Overview Tab
```
┌──────────────────────────────────────────────────────────────────────────┐
│ Projects Overview                                              4 Projects │
├──────────────────────────────────────────────────────────────────────────┤
│ ┌─ Active Projects ──────────────────────────────────────────────────────┐ │
│ │                                                                        │ │
│ │ ┌─ webapp ─────────────┐ ┌─ api-server ─────────────┐ ┌─ mobile ──────┐ │ │
│ │ │ ● 2 Active Sessions  │ │ ⚠ 1 Stuck Session      │ │ ○ 0 Sessions  │ │ │
│ │ │ 📊 High Activity     │ │ 📊 Low Activity        │ │ 📊 Idle       │ │ │
│ │ │ 🕒 3h 42m today     │ │ 🕒 45m today          │ │ 🕒 0m today   │ │ │
│ │ │ 📁 15 files touched │ │ 📁 3 files touched     │ │ 📁 0 files    │ │ │
│ │ │ ✅ 7 commits        │ │ ⚠ 0 commits           │ │ ○ 0 commits   │ │ │
│ │ │ [Open Sessions]     │ │ [Debug Session]       │ │ [Start Work]  │ │ │
│ │ └─────────────────────┘ └───────────────────────┘ └───────────────┘ │ │
│ │                                                                        │ │
│ │ ┌─ frontend ───────────┐                                              │ │
│ │ │ ○ 1 Idle Session     │                                              │ │
│ │ │ 📊 Moderate Activity │                                              │ │
│ │ │ 🕒 1h 15m today     │                                              │ │
│ │ │ 📁 8 files touched  │                                              │ │
│ │ │ ✅ 2 commits        │                                              │ │
│ │ │ [Resume Session]    │                                              │ │
│ │ └─────────────────────┘                                              │ │
│ └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│ ┌─ Project Health Summary ───────────────────────────────────────────────┐ │
│ │ Overall Health: ● Good (85/100)          Most Active: webapp          │ │
│ │ Active Sessions: 4                       Needs Attention: api-server  │ │
│ │ Total Activity: 247 actions today        Coordination Opportunities: 2│ │
│ │ Files Modified: 26                       Avg Session Duration: 2h 15m │ │
│ └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│ ┌─ Quick Actions ────────────────────────────────────────────────────────┐ │
│ │ [+ New Project] [+ New Session] [📊 Analytics] [⚙️ Settings] [📋 Export]│ │
│ └────────────────────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────────────────┘
```

## Sidebar Components

### Project Browser Sidebar
```
┌─ Projects ─────────┐
│ 📁 webapp      ●●● │ ← Active project with multiple sessions
│ 📁 api-server  ⚠○○ │ ← Project with issues
│ 📁 frontend    ○●○ │ ← Project with idle session
│ 📁 mobile      ○○○ │ ← Inactive project
│                    │
│ [+ New Project]    │
└────────────────────┘

┌─ Sessions ─────────┐
│ webapp/auth-ref ●  │ ← Active session
│ webapp/ui-comp  ○  │ ← Idle session
│ api-srv/debug   ⚠  │ ← Session with issues
│                    │
│ [+ New Session]    │
└────────────────────┘

┌─ Health ───────────┐
│ ● 2 Active         │
│ ○ 1 Idle           │
│ ⚠ 1 Issues         │
│                    │
│ Overall: ● Good    │
└────────────────────┘
```

### File Tree Sidebar (Context-Aware)
```
┌─ Files (auth-refactor context) ─┐
│ 📁 src/                         │
│ ├─ 📁 auth/                ●●●  │ ← High activity folder
│ │  ├─ 📄 oauth.js         ●●●  │ ← Currently modified
│ │  ├─ 📄 middleware.js    ●●○  │ ← Recently read
│ │  └─ 📄 tokens.js        ●○○  │ ← Recently created
│ ├─ 📁 components/         ○●○  │ ← Some activity
│ │  ├─ 📄 AuthForm.js      ○●○  │
│ │  └─ 📄 LoginPage.js     ○○○  │
│ └─ 📁 utils/              ○○○  │
│    └─ 📄 helpers.js       ○○○  │
│ 📁 tests/                 ○○●  │ ← Test activity
│ ├─ 📄 auth.test.js        ○○●  │ ← Tests running
│ └─ 📄 oauth.test.js       ○●○  │
│ 📁 docs/                  ○●○  │
│ └─ 📄 oauth-spec.md       ○●○  │ ← Referenced
└─────────────────────────────────┘

Legend:
● High activity (recent/current)
○ Low/no activity
●●● = Created/Modified/Read recently
```

## Quick-Chat Delegation Widget

### Primary Delegation Interface
```
┌─ Quick Delegate ─────────────────────────────────────────────────────────┐
│ Task: [Implement user authentication with JWT tokens              ]     │
│ Target: [Find Session ▼] [auth-refactor] [ui-components] [+ New]        │
│ Context: [☑] Include current files  [☑] Include git history             │
│ Priority: [● High ○ Medium ○ Low]   Deadline: [Today 6PM]              │
│                                                                          │
│ Similar Conversations Found:                            [Find All Chats] │
│ • auth-refactor [abc123] - "OAuth implementation" (85% match)           │
│ • security-audit [def456] - "JWT token security" (72% match)            │
│ • user-mgmt [ghi789] - "Authentication flow" (68% match)               │
│                                                                          │
│ [Use Existing: abc123] [Create New Session] [Delegate to Queue]         │
└──────────────────────────────────────────────────────────────────────────┘
```

### find_relevant_chats() Function Integration
```typescript
// Core function for context discovery
interface ChatMatch {
  session_id: string;
  title: string;
  similarity_score: number;
  context_summary: string;
  last_activity: Date;
  status: 'active' | 'idle' | 'completed';
}

function find_relevant_chats(query: string): Promise<ChatMatch[]> {
  // 1. Semantic search against session context and task descriptions
  // 2. Analyze task patterns and technical requirements
  // 3. Score sessions based on context similarity and recency
  // 4. Return ranked list of potential session matches
}

// Usage in UI:
// - Auto-complete suggestions as user types task
// - "Find Similar" button for explicit context discovery
// - Background matching for proactive session recommendations
```

### Workflow Command Interface
```
┌─ Task Control Panel ────────────────────────────────────────────────────┐
│ Session: auth-refactor [abc123]                    Status: Planning... │
│ Current Task: Implement OAuth2 authentication flows                    │
│                                                                         │
│ Workflow Commands:                                                      │
│ [/plan] [/keep-planning] [/analyze] [/keep-analyzing] [/act] [/review]  │
│                                                                         │
│ Quick Actions:                                                          │
│ [Send "/act"] [Send "/keep-planning"] [Custom Command: ____________]    │
│                                                                         │
│ Auto-Suggestions:                                                       │
│ • "/act" - Planning phase appears complete, ready for implementation    │
│ • "/coordinate" - Database session needs auth token integration         │
│ • "/handoff" - UI session ready for authentication component work      │
└─────────────────────────────────────────────────────────────────────────┘
```

## Task Delegation Feed Component

### Task Status and Coordination Feed
```
┌─ Intelligence Feed ────────────────────────────────────────────────┐
│ [Filter: All ▼] [Sessions: All ▼] [🔍 Search...]  [⚙️] [📊] [📋] │
├────────────────────────────────────────────────────────────────────┤
│ [17:42] ♦ COORDINATION: auth-refactor + ui-components             │
│         └─ Both sessions modifying shared authentication utilities  │
│                                                    [View] [Merge]  │
│                                                                    │
│ [17:41] ● auth-refactor: Completed OAuth token validation         │
│         └─ File: src/auth/oauth.js (Lines 45-67)                   │
│                                              [View Code] [Context] │
│                                                                    │
│ [17:40] ○ ui-components: Modified authentication components        │
│         └─ File: src/components/AuthForm.js                        │
│                                              [View Diff] [Context] │
│                                                                    │
│ [17:39] ⚠ database-opt: Session appears stuck                     │
│         └─ Query optimization taking longer than expected          │
│                                          [Debug] [Restart] [Kill]  │
│                                                                    │
│ [17:38] ● auth-refactor: Created comprehensive test suite          │
│         └─ File: tests/oauth.test.js (156 lines)                   │
│                                          [Run Tests] [View Tests]  │
│                                                                    │
│ [17:37] 📊 INSIGHT: High productivity session detected             │
│         └─ auth-refactor: 15 meaningful changes in past hour       │
│                                                [Analysis] [Export]  │
│                                                                    │
│ [17:35] ● auth-refactor: Analyzed security implications            │
│         └─ External research: OAuth2 RFC 6749                      │
│                                                [View Docs] [Notes] │
└────────────────────────────────────────────────────────────────────┘
```

## Status and Health Indicators

### Session Status Indicators
```
Status Icons:
● Active    - Session is actively processing
○ Idle      - Session waiting for input
⚠ Issues   - Session has problems/stuck
⏸ Paused   - Session manually paused
⏹ Stopped  - Session terminated
🔄 Starting - Session initializing
```

### Health Visualization
```
┌─ Session Health ─────────────────┐
│ Status: ● Active                 │
│                                  │
│ CPU Usage:    ████░░░░░░ 40%    │
│ Memory:       ██████░░░░ 60%    │
│ Response:     ████████░░ 120ms  │
│ Activity:     ██████████ High   │
│                                  │
│ Health Score: 94/100 ↗          │
│ Trend: Improving                 │
│                                  │
│ Issues: None                     │
│ Alerts: None                     │
└──────────────────────────────────┘
```

## Modal Dialogs and Overlays

### New Project Dialog
```
┌─ Create New Project ─────────────────────────────────────┐
│                                                          │
│ Project Name: [webapp-v2                             ]  │
│                                                          │
│ Project Path: [/home/alex/projects/webapp-v2         ]  │
│                                                    [📁]  │
│                                                          │
│ Template:     [○ Empty  ● Node.js  ○ Python  ○ React]  │
│                                                          │
│ Git Repo:     [☑] Initialize git repository             │
│               [☑] Create .gitignore                     │
│               [☐] Connect to remote                     │
│                                                          │
│ Claude Setup: [☑] Add to ~/.claude/projects             │
│               [☑] Create CLAUDE.md                      │
│               [☐] Start initial session                 │
│                                                          │
│                           [Cancel]  [Create Project]    │
└──────────────────────────────────────────────────────────┘
```

### Session Actions Menu
```
┌─ Session Actions ────────────────┐
│                                  │
│ ▶️ Resume Session                │
│ ⏸️ Pause Session                 │
│ 🔄 Restart Session               │
│ 📋 Duplicate Session             │
│ 📤 Export Context                │
│ ──────────────────               │
│ ⚙️ Session Settings              │
│ 📊 Session Analytics             │
│ 🔍 Debug Session                 │
│ ──────────────────               │
│ ⚠️ Force Stop                    │
│ 🗑️ Delete Session                │
│                                  │
└──────────────────────────────────┘
```

## Settings and Configuration Screens

### Settings Overview
```
┌─ Settings ─────────────────────────────────────────────────────────┐
│                                                                    │
│ ┌─ General ──────────┐ ┌─ Interface ───────┐ ┌─ Intelligence ───┐ │
│ │ ● Auto-save        │ │ Theme: Dark        │ │ ● AI Analysis    │ │
│ │ ● Project sync     │ │ Layout: Standard   │ │ ● Coordination   │ │
│ │ ○ Sound alerts    │ │ Font: Monospace    │ │ ○ Predictions   │ │
│ │ Backup: Daily      │ │ Tabs: Top          │ │ Update: 5min     │ │
│ └────────────────────┘ └───────────────────┘ └──────────────────┘ │
│                                                                    │
│ ┌─ Health Monitoring ─────────────────────────────────────────────┐ │
│ │ Check Interval:     [5 minutes      ▼]                         │ │
│ │ Stuck Threshold:    [10 minutes     ▼]                         │ │
│ │ Idle Timeout:       [30 minutes     ▼]                         │ │
│ │ Health Alerts:      [☑] Desktop  [☑] Sound  [☐] Email         │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                                                                    │
│ ┌─ Keyboard Shortcuts ────────────────────────────────────────────┐ │
│ │ New Tab:           Ctrl+T         [Change]                     │ │
│ │ Close Tab:         Ctrl+W         [Change]                     │ │
│ │ Switch Tab:        Ctrl+Tab       [Change]                     │ │
│ │ Search:            Ctrl+F         [Change]                     │ │
│ │ Settings:          Ctrl+,         [Change]                     │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                                                                    │
│                              [Reset Defaults]  [Apply]  [Cancel]  │
└────────────────────────────────────────────────────────────────────┘
```

## Responsive Design Specifications

### Minimum Terminal Size: 80x24
```
┌───────────────────────────────────────────────────────────────────────────┐ 80 chars
│ Cafedelic │ auth-refactor ● │ ui-comp ○ │ + │          [17:42] │ ● 2 sess │
├───────────┼─────────────────────────────────────────────────────────────────┤
│           │ Session: auth-refactor                              ● Active   │
│ Projects  │ Task: OAuth implementation                         Duration 3h │
│ • webapp  │ Files: src/auth/oauth.js ●●●, middleware.js ●●○               │
│ • api     │ Activity: [17:41] Completed token validation                   │
│ • mobile  │ Health: ● Excellent (94/100)   Progress: ████████░░ 80%      │
│           │ [Actions: Resume│Pause│Restart│Export]                        │
│ Sessions  ├─────────────────────────────────────────────────────────────────┤
│ ● auth    │ Intelligence: [17:42] Coordination with ui-components         │
│ ○ ui-comp │ [17:41] OAuth implementation completed successfully            │
│ ⚠ db-opt  │ [17:40] Modified shared authentication utilities               │
├───────────┴─────────────────────────────────────────────────────────────────┤
│ Status: 3 projects │ 3 sessions │ 2 active │ Health: Good │ Q:Quit ?:Help  │
└───────────────────────────────────────────────────────────────────────────┘
```

### Optimal Terminal Size: 120x40 (Recommended)
*Full layout as shown in main dashboard mockup above*

### Large Terminal Size: 160x50+ 
```
Enhanced layout with:
- Wider file tree with more detail
- Extended activity feed
- Additional sidebar panels
- More detailed health metrics
- Extended action buttons
```

## Textual Widget Implementation Guide

### Main Layout Structure
```python
class CafedelicApp(App):
    def compose(self) -> ComposeResult:
        yield Header()
        with TabbedContent(id="main-tabs"):
            with TabPane("Projects", id="projects-tab"):
                yield ProjectsView()
            with TabPane("Session 1", id="session-1"):
                yield SessionView(session_id="session-1")
        yield Footer()

class SessionView(Widget):
    def compose(self) -> ComposeResult:
        with Horizontal():
            with Vertical(classes="sidebar"):
                yield ProjectBrowser()
                yield SessionList()
                yield HealthPanel()
            with Vertical(classes="main-content"):
                yield SessionDetails()
                yield ActivityFeed()
```

### Specific Widget Usage
- **Header**: `Header()` with custom title and status
- **Tabs**: `TabbedContent` with dynamic tab creation
- **Sidebar**: `Vertical` containers with custom widgets
- **File Tree**: `DirectoryTree` with custom node styling
- **Data Tables**: `DataTable` for project and session listings
- **Activity Feed**: `ScrollView` with `ListView` of activity items
- **Status Indicators**: Custom widgets with color-coded status
- **Forms**: Modal dialogs using `Screen` overlays

This mockup specification provides the foundation for implementing the Cafedelic Intelligence Platform TUI with Textual, ensuring a rich, interactive interface that leverages the framework's capabilities while maintaining excellent usability and visual appeal.