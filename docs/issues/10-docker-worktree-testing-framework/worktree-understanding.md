# Git Worktrees and Cafedelic Deployment Strategy

## Understanding Git Worktrees

A git worktree is **not** a branch - it's a linked working directory that shares the same repository database. This is a crucial distinction for understanding the cafedelic multi-server architecture.

### Key Concepts

1. **Shared Repository**: All worktrees share the same `.git` repository
2. **Independent Working Trees**: Each worktree has its own:
   - Working directory (uncommitted changes)
   - Index (staging area)
   - Currently checked-out branch/commit
3. **Direct Commits**: When you commit in a worktree, you're committing to whatever branch that worktree has checked out

### Example Workflow

```bash
# Create worktrees all on main branch
git worktree add ../cafedelic-red main
git worktree add ../cafedelic-blue main

# In cafedelic-red directory
cd ../cafedelic-red
echo "red server change" >> file.txt
git add file.txt
git commit -m "Change from red server"
# This commits DIRECTLY to main - no merge needed!

# In cafedelic-blue directory
cd ../cafedelic-blue
git pull  # Gets the change from red server
```

### Why This Is Perfect for Cafedelic

1. **Parallel Development**: Multiple servers can work on the same codebase simultaneously
2. **Branch Flexibility**: Each server can checkout different branches when needed
3. **Shared State**: All servers see the same git history and remote configuration
4. **No Merge Conflicts**: When servers work on the same branch, commits are linear
5. **Live Collaboration**: Changes from one server immediately available to others via pull

## Deployment Evolution Strategy

The cafedelic deployment will gradually evolve from manual processes to fully automated MCP tool-based orchestration:

### Phase 1: Manual Foundation ✅
```bash
# Current state - manual Docker commands
cd docker && docker compose up
```

### Phase 2: Script-Based Discovery (Next)
```bash
# Automated worktree discovery and Docker service generation
./scripts/deploy-worktrees.sh
```

### Phase 3: Tmux Layout Integration
```bash
# Create tmux grid and assign servers to panes
./scripts/create-cafedelic-layout.sh --servers 4
```

### Phase 4: MCP Tool Orchestration (Goal)

The ultimate vision is deployment via MCP tool calls from Claude:

```typescript
// Deploy specific servers with layout control
await deploy_cafedelic({
  branch: "feature/new-parser",
  servers: ["@red", "@green"],
  layout: "horizontal-split",
  panes: {
    red: { terminal: "10:0.0", editor: "10:0.1", claude: "10:0.2" },
    green: { terminal: "10:1.0", editor: "10:1.1", claude: "10:1.2" }
  }
});

// Quick deployment with defaults
await quick_deploy({
  branch: "main",
  serverCount: 4
});

// Grouped pane deployment
await deploy_server_group({
  name: "parser-team",
  servers: ["@red", "@blue"],
  sharedPane: "10:2.0",  // Shared output pane
  branch: "feature/parser-v2"
});
```

### Tool Design Goals

1. **Progressive Complexity**: Simple defaults with advanced options
2. **Layout Templates**: Pre-defined layouts (grid, horizontal, vertical, focus)
3. **Pane Management**: Automatic pane creation and assignment
4. **State Persistence**: Remember deployments across sessions
5. **Health Monitoring**: Built-in health checks and recovery

### Implementation Approach

1. **Start Simple**: Basic worktree detection and Docker composition
2. **Add Intelligence**: Smart color/port assignment based on worktree names
3. **Integrate Tmux**: Programmatic pane creation and assignment
4. **Build Tools**: MCP tools that wrap the underlying scripts
5. **Enhance UX**: Visual feedback, progress indicators, error recovery

## Immediate Next Steps

1. **Create test worktrees** to validate the discovery approach
2. **Build worktree discovery script** that outputs structured data
3. **Design Docker service template** for dynamic generation
4. **Plan tmux integration** approach for Phase 3

The beauty of this approach is that each phase builds on the previous one, gradually automating more of the deployment process while maintaining flexibility for different use cases.