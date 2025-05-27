# Docker Worktree Testing Framework: Parallel Development Infrastructure

## Problem Statement

Development velocity is hampered by:
1. Fear of breaking working configurations while experimenting
2. Serial testing of different approaches
3. Difficult to compare approaches side-by-side
4. No isolated environment for "yolo mode" experimentation

## Proposed Solution

Create a Docker-based testing framework with git worktrees that allows:
- 4+ parallel server configurations running simultaneously
- Isolated experimentation without affecting main branch
- Visual comparison in single tmux window
- Automated testing with Claude Code instances
- Easy promotion of successful experiments to staging/main

## Architecture Overview

```
main branch
├── staging branch (integration testing)
└── worktrees/
    ├── experiment-1/ (plain emacs mode)
    ├── experiment-2/ (daemon with fixes)
    ├── experiment-3/ (alternative parsing)
    └── experiment-4/ (claude code yolo)
    
Each worktree → Docker container → Tmux pane
```

## Implementation Plan

### 1. Docker Configuration

Create `docker/Dockerfile.experiment`:
```dockerfile
FROM node:20-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci
COPY . .
ENV EXPERIMENT_NAME=""
ENV CAFEDELIC_PORT=3000
CMD ["npm", "run", "dev"]
```

### 2. Worktree Setup Script

`scripts/setup-experiments.sh`:
```bash
#!/bin/bash
# Create worktrees for different experiments
git worktree add worktrees/plain-emacs experiment/plain-emacs
git worktree add worktrees/daemon-fixes experiment/daemon-fixes
git worktree add worktrees/new-parser experiment/new-parser
git worktree add worktrees/yolo-mode experiment/yolo-mode

# Create docker-compose.yml
cat > docker-compose.experiments.yml << EOF
version: '3.8'
services:
  plain-emacs:
    build:
      context: ./worktrees/plain-emacs
    environment:
      - EXPERIMENT_NAME=plain-emacs
      - CAFEDELIC_PORT=3001
    ports:
      - "3001:3001"
    volumes:
      - ./logs/plain-emacs:/app/logs
      
  daemon-fixes:
    build:
      context: ./worktrees/daemon-fixes
    environment:
      - EXPERIMENT_NAME=daemon-fixes
      - CAFEDELIC_PORT=3002
    ports:
      - "3002:3002"
    volumes:
      - ./logs/daemon-fixes:/app/logs
EOF
```

### 3. Tmux Visualization Layout

Create viewing layout with `scripts/experiments-view.sh`:
```bash
#!/bin/bash
# Create tmux session with 4 panes showing different experiments
tmux new-session -d -s experiments

# Split into 4 equal panes
tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

# Attach to each container's logs
tmux send-keys -t experiments:0.0 'docker logs -f cafedelic-plain-emacs' C-m
tmux send-keys -t experiments:0.1 'docker logs -f cafedelic-daemon-fixes' C-m
tmux send-keys -t experiments:0.2 'docker logs -f cafedelic-new-parser' C-m
tmux send-keys -t experiments:0.3 'docker logs -f cafedelic-yolo-mode' C-m
```

### 4. Automated Testing Framework

Enable Claude Code to run experiments:
```typescript
// src/tools/run_experiment.ts
export async function runExperiment(config: {
  worktree: string;
  testScript: string;
  duration: number;
}) {
  // Start container
  // Run test script
  // Collect metrics
  // Report results
}
```

### 5. Staging Workflow

Promotion process for successful experiments:
```bash
# Cherry-pick successful changes
cd worktrees/plain-emacs
git log --oneline
# Find successful commits

cd ../..
git checkout staging
git cherry-pick <commit-hash>

# Or merge entire experiment
git merge experiment/plain-emacs
```

## Experiment Ideas

1. **Plain Emacs Mode**: Test direct emacs commands
2. **Daemon Fixes**: Socket management improvements
3. **New Parser**: Alternative log parsing strategies
4. **Yolo Mode**: Claude Code with write permissions

## Benefits

- **Parallel Development**: Test 4+ approaches simultaneously
- **Visual Comparison**: See all experiments in one view
- **Isolation**: Each experiment in its own git worktree
- **Safety**: Main branch remains stable
- **Speed**: Rapid iteration without fear
- **Automation**: Claude Code can run wild in containers

## Success Criteria

- [ ] Docker containers for each experiment
- [ ] Git worktrees properly isolated
- [ ] Tmux layout shows all experiments
- [ ] Easy switching between configurations
- [ ] Metrics collection for comparison
- [ ] Staging branch integration workflow
- [ ] Automated testing capabilities

## Future Enhancements

- Prometheus metrics for performance comparison
- Automated A/B testing framework
- Web dashboard for experiment results
- CI/CD pipeline for successful experiments
- Load testing capabilities
- Multi-agent coordination testing
