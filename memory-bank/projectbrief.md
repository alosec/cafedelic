# Cafedelic Project Brief

## Project Name
Cafedelic - Intelligence Layer for Claude Code Project Management

## Vision
Provide an intelligence platform that enhances Claude Code workflows through comprehensive project and session management. Cafedelic acts as a "glass box mission control" that makes AI development work visible, manageable, and orchestratable across multiple projects and Claude Code instances.

## Core Problem
AI-assisted development creates new challenges that traditional tools don't address:
1. **Session Invisibility**: Multiple Claude Code instances operate as isolated black boxes
2. **Project Context Loss**: AI work happens without project-level coordination
3. **Activity Fragmentation**: No unified view of what AI is doing across sessions
4. **Manual Orchestration**: Constant switching between projects and AI conversations
5. **Intelligence Isolation**: Each Claude session exists independently without shared context

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
