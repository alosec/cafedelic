# Cafedelic Project Brief

## Project Name
Cafedelic - AI Development Transparency & Orchestration Framework

## Vision
Create a transparent development environment that makes AI-assisted development visible, analyzable, and orchestratable by watching development activity, providing real-time insights, and enabling flexible output routing to tmux panes.

## Core Problem
Developers working with AI tools like Claude Desktop lack:
1. **Visibility**: What is the AI doing right now?
2. **Integration**: How can AI actions trigger editor responses?
3. **Flexibility**: How to route AI activity to different displays?
4. **Orchestration**: How to manage multiple development contexts?

## Solution
A framework that:
1. **Watches** development activity logs (Desktop Commander, Claude Desktop MCP)
2. **Translates** technical logs into human-readable insights
3. **Routes** output dynamically to user-configured tmux panes
4. **Integrates** with development tools (Emacs, tmux)
5. **Orchestrates** development environments flexibly

## Success Metrics
- Real-time translation of MCP logs into human insights
- Dynamic routing of output to any tmux pane
- Automatic file/directory opening in Emacs
- Zero hard-coded layout assumptions
- Flexible pane assignment via MCP tools

## Scope
### In Scope
- MCP log watching and translation
- Dynamic output routing system
- Emacs integration (file/directory opening)
- TMEX layout management tools
- Pane-specific server management
- Activity context via MCP tools
- Shell script integration patterns

### Out of Scope (Current Phase)
- Multi-agent orchestration (archived for future)
- Docker container deployment
- Database persistence beyond session
- Complex pattern analysis
- Intelligence accumulation

## Technical Constraints
- Express.js server with TypeScript
- Event-driven service architecture
- Shell script wrappers for proven functionality
- Flexible routing without hard-coded assumptions
- Real-time log processing

## Timeline
- Phase 1: MCP log watch & translate ✅ (completed)
- Phase 2: Output routing system ✅ (completed)
- Phase 3: Pane display abstraction (current focus)
- Phase 4: Multi-server platform (future vision)
