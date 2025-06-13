# Cafedelic Project Brief

## Project Name
Cafedelic - Claude Code Powered Tmux IDE

## Vision
Create a tmux-based AI pair programming IDE that seamlessly integrates Claude Code sessions with developer workflows, providing intuitive CLI tools, intelligent layout management, and reactive status displays for managing multiple AI-assisted development tasks.

## Core Problem
Developers using Claude Code need:
1. **Session Management**: Track multiple Claude Code instances with human names
2. **Integrated Workspace**: File tree, editor, and messages in optimal layout
3. **Simple Commands**: CLI tools that make complex setups one command away
4. **Activity Awareness**: See what Claude is doing across all sessions
5. **Seamless Integration**: Direct tmux integration without external dependencies

## Solution
A tmux-native IDE framework that:
1. **Validates** system readiness with 'cafe init' - ensures server running
2. **Deploys** simple layouts (70% editor, 30% system events) with 'cafe deploy'
3. **Integrates** deeply with tmux using property-based pane management
4. **Provides** direct bash script execution (no middleware overhead)
5. **Expands** to full session management and activity tracking (future phases)

## Success Metrics (Phase 1)
- 'cafe init' validates system in under 1 second with clear error reporting
- 'cafe deploy' creates working 2-pane layout in under 3 seconds
- Emacs integration works seamlessly as Claude Desktop's editor
- System events display provides activity visibility
- Zero configuration required for basic workflow

## Scope
### In Scope
- cafe CLI tool suite with subcommands
- Claude Code session lifecycle management
- SQLite-backed messages database
- Reactive pane displays (not just send-keys)
- Direct script invocation architecture
- Modular layout system with sensible defaults
- Integration with Claude Code SDK
- MCP tools for programmatic access
- Shell script integration patterns

### Out of Scope (Current Phase)
- Multi-agent orchestration beyond Claude Code
- Docker container deployment
- Complex ML-based pattern analysis
- Cross-machine session sync
- GUI interfaces

## Technical Constraints
- Bash-first CLI implementation
- Direct script execution (no middleware)
- SQLite for local persistence
- Tmux as display layer
- Node.js for WTE pipeline
- Shell scripts for core operations

## Timeline
- Phase 1: MCP log watch & translate ✅ (completed)
- Phase 2: Output routing system ✅ (completed)
- Phase 3: Claude Code IDE & cafe CLI (current focus)
- Phase 4: Advanced session intelligence (future)
