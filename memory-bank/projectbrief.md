# Cafedelic Project Brief

## Project Name
Cafedelic - Intelligence Layer for Claude Code Project Management

## Vision
Provide an asynchronous task delegation platform that transforms Claude Code from isolated conversations into an orchestrated workflow engine. Cafedelic enables rapid task assignment, intelligent session coordination, and automated handoffs between AI instances through natural language interfaces and contextual session discovery.

## Core Problem
AI-assisted development creates new workflow challenges that traditional tools don't address:
1. **Task Assignment Friction**: No quick way to delegate tasks to appropriate Claude Code sessions
2. **Context Discovery Challenge**: Can't find relevant previous conversations for similar tasks
3. **Manual Workflow Management**: Constantly switching between planning, analysis, and implementation modes
4. **Session Coordination Overhead**: No async task delegation or automated handoffs between sessions
5. **Command Workflow Isolation**: Project-specific slash commands in ~/.claude/commands not integrated with delegation

## Solution Architecture
A three-layer intelligence platform:

### 1. **MCP Toolset** (API Layer)
Natural language interface for managing projects and sessions:
- `create_project(name, repo_path, description)` 
- `create_session(project_id, branch, worktree, assistant_type)`
- `read_session_context(session_id, include_summary)`
- `update_project_status(project_id, status)`

### 2. **SQLite Intelligence Database** (Core Layer)  
Comprehensive data model for AI development workflows:
- **Projects**: Git repositories, worktrees, branch management
- **Sessions**: Claude Code instances with metadata and context
- **Activities**: File operations, conversations, task progression
- **Context**: Dynamic summaries via `claude -p` processing

### 3. **Display Adapters** (View Layer)
Flexible frontend integrations:
- Terminal displays (tmux, terminal multiplexers)
- Editor extensions (VS Code, Emacs integration)  
- Web dashboards (future expansion)
- CLI interfaces (`cafe` command suite)

## Success Metrics
- **Claude Code Enhancement**: Seamless integration that adds intelligence without disrupting existing workflows
- **Session Coordination**: Multiple Claude Code instances managed through single orchestrator interface
- **Context Preservation**: AI conversations maintain project context across sessions and time
- **Real-time Intelligence**: `claude -p` summaries provide actionable insights about current AI activities
- **Database-Driven Insights**: Rich querying capabilities for project history and session analytics

## Scope Definition

### Core Focus: Claude Code Intelligence
- **Project Management**: Database-driven project organization with Git integration
- **Session Orchestration**: Claude Code instance lifecycle and coordination
- **Context Intelligence**: Real-time AI activity analysis and summarization
- **MCP API**: Natural language interface for all operations
- **Display Flexibility**: Adapter pattern for multiple frontend options

### Explicit Non-Goals
- **Editor Replacement**: Cafedelic enhances, doesn't replace existing editors
- **Terminal Management**: Users manage their own tmux/terminal setup
- **Code Generation**: AI assistants handle coding, cafedelic handles coordination
- **Multi-Assistant Support**: Phase 1 focuses exclusively on Claude Code

## Technical Foundation
- **Database-First**: SQLite as single source of truth for all intelligence
- **MCP-Native**: All operations exposed through conversational interface
- **Assistant-Agnostic Design**: Architecture supports future expansion beyond Claude Code
- **Plugin Architecture**: Display adapters separate intelligence from presentation
- **Event-Driven**: Proven WTE pipeline for real-time activity monitoring

## Development Philosophy
- **Intelligence over Interface**: Focus on making AI work visible and manageable
- **Enhancement over Replacement**: Integrate with existing tools rather than replacing them
- **Database over Files**: Structured data enables rich analysis and coordination
- **Conversation over Commands**: Natural language MCP interface for all operations
- **Value-First**: Every feature must provide clear value for Claude Code workflows
