# Cafedelic Multi-Server Development Platform

## Vision Statement

This isn't just a testing framework - this IS the fundamental expression of cafedelic's vision: multiple coordinated development environments with AI agents working in parallel, each with its own editor, terminal, and Claude Code instance.

## Core Requirements

- **Base Image**: Debian 12 Bookworm (required)
- **Dynamic Worktrees**: Auto-discover available worktrees, no hardcoding
- **Flexible Layout**: Tmux grid with buffer rows and server-specific panes
- **Shared Database**: All servers operate on same DB file
- **Color Identity**: Red, Blue, Green, Yellow servers

## Architecture Overview

### Tmux Layout Vision
```
┌─────────────────────────────────────────┐
│          Top Buffer Row                 │
├─────────────────────────────────────────┤
│ RED     │ RED      │ RED               │
│ Editor  │ Terminal │ Claude Code       │
├─────────────────────────────────────────┤
│ BLUE    │ BLUE     │ BLUE              │
│ Editor  │ Terminal │ Claude Code       │
├─────────────────────────────────────────┤
│ GREEN   │ GREEN    │ GREEN             │
│ Editor  │ Terminal │ Claude Code       │
├─────────────────────────────────────────┤
│ YELLOW  │ YELLOW   │ YELLOW            │
│ Editor  │ Terminal │ Claude Code       │
├─────────────────────────────────────────┤
│          Bottom Buffer Row              │
└─────────────────────────────────────────┘
```

### Server Configuration
Each server instance:
- Runs from its own git worktree
- Has unique port (3001-3004)
- Knows its assigned tmux panes via environment
- Maintains distinct color identity
- Shares central database

## Implementation Phases

### Phase 1: Docker Foundation (First Step)
Create basic Debian 12 Docker image and verify single server:

```dockerfile
FROM debian:12-bookworm
# Install Node.js, git, tmux dependencies
# Configure for cafedelic server
```

**Deliverables:**
- Dockerfile with Debian 12 base
- docker-compose.yml for single server
- Build script
- Verify output to specific tmux pane

### Phase 2: Dynamic Worktree Discovery
Automated worktree detection and Docker composition:

```bash
# discover-worktrees.sh
git worktree list --porcelain | grep "worktree" | cut -d' ' -f2

# Dynamically generate docker-compose.yml
for worktree in $(discover_worktrees); do
    add_server_to_compose "$worktree" "$color" "$port"
done
```

**Deliverables:**
- Worktree discovery script
- Dynamic docker-compose generator
- Color assignment system
- Port allocation logic

### Phase 3: Tmux Grid Integration
Connect servers to tmux panes:

```bash
# Environment for each server
CAFEDELIC_SERVER_NAME=red
CAFEDELIC_EDITOR_PANE=10:1.0
CAFEDELIC_TERMINAL_PANE=10:1.1  
CAFEDELIC_CLAUDE_PANE=10:1.2
CAFEDELIC_PORT=3001
```

**Deliverables:**
- Tmex-based grid layout creator
- Server-to-pane mapping configuration
- Pane assignment scripts
- Visual feedback system

### Phase 4: Enhanced Agent Communication
Improve Claude Code interaction:

```typescript
// Better agent messaging
async function send_message_to_agent(options: {
  pane: string;
  message: string;
  readFirst: boolean;
  confirmAfter: boolean;
  pressEnter: boolean;
}) {
  // Read current state
  // Send message
  // Confirm delivery
  // Handle response
}
```

## Milestones & Next Steps

### Milestone 1: Single Server Running ✓
- Docker container built with Debian 12
- Server outputs to designated tmux pane
- Basic proof of concept working

### Milestone 2: Four Colors Dancing 🎯
**This is the first truly meaningful milestone!**
- Red, Blue, Green, Yellow servers all running
- Each in their own worktree
- Visible in tmux grid layout
- Color-coded terminal output
- "It's alive!" moment

### Milestone 3: Orchestrated Development
- Editors auto-opening files in sync
- Terminals showing real-time commands
- Claude Code instances ready for interaction
- Shared database updates visible across servers

### Milestone 4: Automated Deployment
**The culmination of the cafedelic vision:**

```typescript
deploy_cafedelic({
  branch: "feature/new-parsing",
  server: "@green",
  panes: {
    terminal: "9:0.1",
    claude: "9:0.2", 
    editor: "9:0.3"
  }
})
```

This function will:
1. Create/checkout worktree for branch
2. Build and launch Docker container
3. Assign to specified panes
4. Configure server identity
5. Start all services
6. Ready for AI-assisted development

## Path to Success

### Next Steps After Phase 1:
1. **Color Assignment System**
   - Map worktree names to colors
   - Visual feedback in terminals
   - Color-coded log output

2. **Multi-Server Launch**
   - Parallel Docker container startup
   - Health check system
   - Graceful failure handling

3. **Pane Coordination**
   - Each server knows its display targets
   - Automatic layout verification
   - Recovery from pane destruction

4. **Inter-Server Communication**
   - Shared event bus
   - Database synchronization
   - Coordinated actions

## Success Criteria

- [ ] Debian 12 Docker image builds successfully
- [ ] Dynamic worktree discovery works
- [ ] Four colored servers run simultaneously
- [ ] Each server has 3 dedicated tmux panes
- [ ] Editors sync with file operations
- [ ] Claude Code instances are interactive
- [ ] deploy_cafedelic() function works
- [ ] Multiple developers can orchestrate AI agents

## The Vision Realized

When complete, cafedelic will enable:
- Parallel experimentation across branches
- AI agents working in concert
- Visual comparison of approaches
- Rapid iteration and testing
- Orchestrated multi-agent development

This is more than infrastructure - it's a new paradigm for AI-assisted development where multiple environments and agents collaborate under human orchestration.
