# Implementation Roadmap

## Week 1: Foundation
**Goal**: Basic pane naming and text sending

### Day 1-2: Core Scripts
- [ ] Create `~/.cafedelic/` directory structure
- [ ] Implement `assign-name.sh`
- [ ] Implement `find-pane.sh` 
- [ ] Implement `send-to-pane.sh`
- [ ] Test manual script execution

### Day 3-4: MCP Integration
- [ ] Add Express endpoints for pane tools
- [ ] Create TypeScript wrappers for scripts
- [ ] Test via Claude Desktop

### Day 5: Special Keys
- [ ] Implement `send-special-key.sh`
- [ ] Implement `send-ctrl-c.sh` with double-tap
- [ ] Add key mapping logic

## Week 2: Routing System
**Goal**: Dynamic output routing

### Day 1-2: Configuration
- [ ] Design routing config format
- [ ] Implement `set-destination.sh`
- [ ] Implement `get-config.sh`
- [ ] Create config watchers

### Day 3-4: Pipeline Integration
- [ ] Modify executors to read routing
- [ ] Add routing transforms
- [ ] Test with file operations

### Day 5: Polish
- [ ] Error handling
- [ ] Config validation
- [ ] Documentation

## Week 3: Advanced Features
**Goal**: Full interaction capabilities

### Day 1-2: Enhanced Tools
- [ ] Pane roles system
- [ ] Activity summaries
- [ ] Clear/reset operations

### Day 3-4: tmux-mcp Evaluation
- [ ] Test integration possibilities
- [ ] Identify complementary features
- [ ] Document hybrid approach

### Day 5: Testing & Docs
- [ ] End-to-end testing
- [ ] User documentation
- [ ] Example workflows

## Deliverables per Phase

### Phase 1 Deliverables
- Working pane naming system
- Basic text/key sending
- MCP tool integration

### Phase 2 Deliverables  
- Dynamic routing configuration
- Persistent settings
- Pipeline integration

### Phase 3 Deliverables
- Complete interaction toolkit
- tmux-mcp integration plan
- Production-ready system

## Definition of Done
- [ ] All scripts executable and tested
- [ ] MCP tools documented
- [ ] Routing system configurable
- [ ] Zero hard-coded assumptions
- [ ] Works with Claude Desktop & Code
- [ ] Memory bank updated
