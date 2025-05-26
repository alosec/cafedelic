# Feature Request: Intelligent Pane Resizing System

## Summary
Implement an intelligent pane resizing system that automatically adjusts tmux pane sizes based on analysis of current focus and activity, leveraging LLM intelligence for optimal workspace management.

## Vision
Create a smart workspace that adapts to developer workflow by:
- Automatically analyzing which pane/window has current focus or activity
- Using LLM intelligence (BYOK or claude -p in non-interactive mode) to make smart decisions about optimal pane sizes
- Operating both on-demand and in the background
- Dynamically adjusting tmux pane sizes based on context and usage patterns

The system would essentially create a "smart workspace" that expands active panes, shrinks inactive ones, and maintains an optimal layout without manual intervention.

## Technical Context

### Current Architecture Foundation
Cafedelic already has several components that could support this feature:

1. **Event-Driven State Management**
   - `StateManager` service with EventEmitter pattern
   - Tracks file access, activities, and pane states
   - SQLite3 database for persistence

2. **Pane State Tracking**
   - Database schema includes `pane_states` table
   - Tracks pane names, IDs, content types, current files
   - Already integrated with tmux pane naming system

3. **Activity Monitoring**
   - DC log watcher tracks all Claude activities
   - File access patterns stored in database
   - Real-time event emission for state changes

4. **Tmux Integration**
   - MCP tools for tmux operations already registered
   - Pane naming convention established (`@pane_name`)
   - Split pane tools implemented

## Implementation Approach

### Phase 1: Focus Detection
1. **Activity Metrics Collection**
   - Track last activity timestamp per pane
   - Monitor file access frequency per pane
   - Detect command execution patterns
   - Calculate "activity score" for each pane

2. **Focus Heuristics**
   - Most recent file access
   - Command execution frequency
   - Terminal output activity
   - Cursor position changes (if detectable)

### Phase 2: LLM Integration
1. **Decision Engine**
   ```typescript
   interface PaneResizeDecision {
     paneId: string;
     currentSize: { width: number; height: number };
     suggestedSize: { width: number; height: number };
     reasoning: string;
     confidence: number;
   }
   ```

2. **LLM Prompt Context**
   - Current pane layout and sizes
   - Activity metrics for each pane
   - File types and content context
   - Recent user actions
   - Time-based patterns

3. **Integration Options**
   - BYOK (Bring Your Own Key) for any LLM provider
   - Claude CLI in non-interactive mode: `claude -p "analyze pane layout"`
   - Configurable LLM endpoint

### Phase 3: Resize Execution
1. **Smooth Resizing**
   - Gradual size adjustments to avoid jarring changes
   - Minimum/maximum size constraints
   - Preserve aspect ratios where appropriate

2. **User Preferences**
   - Configurable aggressiveness levels
   - Blacklist panes from auto-resize
   - Time-based rules (e.g., no resizing during focused work)

## Database Schema Extensions

```sql
-- Pane activity metrics
CREATE TABLE IF NOT EXISTS pane_activity_metrics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    pane_name TEXT NOT NULL,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    activity_type TEXT CHECK(activity_type IN ('file_access', 'command', 'output', 'focus')),
    activity_score REAL DEFAULT 1.0,
    metadata_json TEXT,
    FOREIGN KEY (pane_name) REFERENCES pane_states(pane_name)
);

-- Resize decisions and history
CREATE TABLE IF NOT EXISTS pane_resize_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    pane_name TEXT NOT NULL,
    old_width INTEGER,
    old_height INTEGER,
    new_width INTEGER,
    new_height INTEGER,
    decision_reasoning TEXT,
    llm_confidence REAL,
    user_override BOOLEAN DEFAULT FALSE
);

-- User preferences
CREATE TABLE IF NOT EXISTS resize_preferences (
    pane_name TEXT PRIMARY KEY,
    min_width INTEGER,
    min_height INTEGER,
    max_width INTEGER,
    max_height INTEGER,
    auto_resize_enabled BOOLEAN DEFAULT TRUE,
    priority_weight REAL DEFAULT 1.0
);
```

## Service Architecture

```typescript
// services/pane-intelligence.service.ts
export class PaneIntelligenceService extends EventEmitter {
  private activityTracker: ActivityTracker;
  private llmDecisionEngine: LLMDecisionEngine;
  private resizeExecutor: ResizeExecutor;
  
  async analyzeAndResize(): Promise<ResizeResult> {
    // 1. Collect current state
    const paneStates = await this.collectPaneStates();
    const activityMetrics = await this.activityTracker.getMetrics();
    
    // 2. Get LLM decision
    const decision = await this.llmDecisionEngine.analyze({
      paneStates,
      activityMetrics,
      userPreferences: await this.getUserPreferences()
    });
    
    // 3. Execute resize
    return await this.resizeExecutor.execute(decision);
  }
}
```

## Configuration

```typescript
interface IntelligentResizeConfig {
  enabled: boolean;
  mode: 'on-demand' | 'background' | 'both';
  backgroundInterval: number; // milliseconds
  llm: {
    provider: 'claude' | 'openai' | 'custom';
    endpoint?: string;
    apiKey?: string;
    model?: string;
  };
  constraints: {
    minPaneWidth: number;
    minPaneHeight: number;
    maxResizePercentage: number;
  };
  triggers: {
    fileAccessThreshold: number;
    inactivityTimeout: number;
    commandExecutionWeight: number;
  };
}
```

## User Interface

### MCP Tools
```typescript
// Get resize suggestions without executing
tool: 'analyze_pane_layout'
// Execute intelligent resize
tool: 'resize_panes_intelligently'
// Configure resize preferences
tool: 'set_pane_resize_preferences'
// View resize history
tool: 'get_resize_history'
```

### Status Integration
Add to existing activity monitor:
- Current focus analysis
- Resize suggestions
- Last resize timestamp
- Activity heatmap

## Benefits

1. **Improved Developer Experience**
   - No manual pane resizing needed
   - Optimal visibility for active work
   - Reduced context switching

2. **Adaptive Workflows**
   - Different layouts for different tasks
   - Time-based preferences
   - Learning from usage patterns

3. **Integration with Cafedelic Vision**
   - Enhances development intelligence
   - Provides deeper workspace analytics
   - Natural extension of activity tracking

## Implementation Phases

### MVP (Phase 1)
- Basic activity tracking per pane
- Simple rule-based resizing
- Manual trigger via MCP tool

### Enhanced (Phase 2)
- LLM integration for decisions
- Background monitoring
- User preference system

### Advanced (Phase 3)
- Predictive resizing
- Multi-monitor support
- Workflow templates

## Open Questions

1. **Performance Impact**: How to minimize CPU usage during background analysis?
2. **User Control**: What level of manual override should be provided?
3. **LLM Costs**: How to optimize prompts to minimize API calls?
4. **Layout Complexity**: How to handle complex nested pane layouts?
5. **Integration Points**: Should this integrate with window managers beyond tmux?

## Success Metrics

- Reduction in manual pane resizing actions
- Increased time spent in "optimal" layouts
- User satisfaction scores
- Performance overhead < 1% CPU
- LLM decision accuracy > 80%

## Related Work

- Tiling window managers (i3, dwm) with dynamic layouts
- IDE adaptive layouts (IntelliJ IDEA's context-aware panels)
- Research on attention-based UI optimization

---

This feature would transform Cafedelic from a passive observer to an active workspace optimizer, creating a truly intelligent development environment that adapts to developer needs in real-time.