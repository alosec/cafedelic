# Intelligence Layer

## Core Concept

The intelligence layer is cafedelic's primary value proposition: transforming Claude Code from isolated conversations into a coordinated, visible, and manageable development ecosystem. This layer sits between raw AI activity and human decision-making, providing context, coordination, and insight.

## The Glass Box Vision

### Making AI Work Visible
Traditional AI assistants operate as black boxes - you see results but not the thinking process. Cafedelic creates a "glass box" where you can observe:

- **What Claude is currently working on** across all sessions
- **How different sessions relate** to each other and to project goals  
- **When Claude needs guidance** or encounters blockers
- **Which files and concepts** are central to current work
- **What patterns emerge** across time and projects

### Real-Time Intelligence
```
Human: "What's happening with my projects?"

Intelligence Layer Response:
"You have 3 active projects:

🔍 auth-refactor (2 sessions active)
   └── Marcel: Deep in OAuth middleware analysis, stuck on token edge cases for 45min
   └── Sandra: Just completed button redesign, now working on form validation

📊 ui-components (1 session)  
   └── Working steadily through component library updates

⏸️ db-migration (paused 3 hours ago)
   └── Migration scripts 80% complete, Jorge left detailed notes

💡 Insight: Marcel's OAuth work might help with the auth components Sandra is building"
```

## Core Intelligence Functions

### 1. Context Awareness
**Problem**: Claude loses context between conversations and sessions
**Solution**: Persistent context preservation and cross-session sharing

```typescript
interface SessionContext {
  current_task: string;           // "Analyzing OAuth token handling"
  open_files: string[];          // Files Claude is actively working with
  recent_activity: Activity[];   // File operations, searches, code changes
  conversation_summary: string;  // High-level summary of recent chat
  blockers: string[];           // Apparent issues or stuck points
  related_sessions: string[];   // Other sessions working on similar code
}
```

### 2. Activity Intelligence
**Problem**: Can't see what Claude is thinking or planning
**Solution**: Real-time analysis of AI activity patterns

```sql
-- Track all AI operations with rich metadata
CREATE TABLE activities (
  id INTEGER PRIMARY KEY,
  session_id TEXT REFERENCES sessions(id),
  type TEXT, -- 'file_read', 'file_write', 'search', 'analysis', 'stuck'
  content TEXT, -- What Claude did/said
  files_affected TEXT, -- JSON array of file paths
  timestamp DATETIME,
  context_tags TEXT -- JSON array: ['oauth', 'authentication', 'middleware']
);

-- Example query: What's Claude doing right now?
SELECT 
  s.name as session,
  a.type,
  a.content,
  a.timestamp
FROM activities a
JOIN sessions s ON a.session_id = s.id
WHERE a.timestamp > datetime('now', '-15 minutes')
ORDER BY a.timestamp DESC;
```

### 3. Cross-Session Coordination
**Problem**: Multiple Claude sessions work in isolation
**Solution**: Intelligence layer coordinates and shares insights

```typescript
// Detect session overlap and potential conflicts
async function analyzeSessionCoordination(project_id: string) {
  const activeSessions = await getActiveSessions(project_id);
  const insights = [];
  
  for (const sessionA of activeSessions) {
    for (const sessionB of activeSessions) {
      if (sessionA.id !== sessionB.id) {
        // Check for file overlap
        const filesA = await getSessionFiles(sessionA.id);
        const filesB = await getSessionFiles(sessionB.id);
        const sharedFiles = findIntersection(filesA, filesB);
        
        if (sharedFiles.length > 0) {
          insights.push({
            type: 'potential_conflict',
            sessions: [sessionA.name, sessionB.name],
            shared_files: sharedFiles,
            recommendation: `${sessionA.name} and ${sessionB.name} are both working on ${sharedFiles.join(', ')} - consider coordinating`
          });
        }
        
        // Check for conceptual overlap using AI analysis
        const conceptualOverlap = await analyzeConceptualSimilarity(
          sessionA.recent_activity,
          sessionB.recent_activity
        );
        
        if (conceptualOverlap.similarity > 0.7) {
          insights.push({
            type: 'related_work',
            sessions: [sessionA.name, sessionB.name],
            similarity: conceptualOverlap.similarity,
            shared_concepts: conceptualOverlap.concepts,
            recommendation: `${sessionA.name} work on ${conceptualOverlap.concepts.join(', ')} could inform ${sessionB.name}`
          });
        }
      }
    }
  }
  
  return insights;
}
```

### 4. Predictive Intelligence
**Problem**: Reactive responses to AI getting stuck or needing guidance
**Solution**: Proactive detection of when intervention would be helpful

```typescript
// Analyze session patterns to predict when guidance is needed
async function analyzeSessionHealth(session_id: string) {
  const recentActivity = await getRecentActivities(session_id, '30 minutes');
  const patterns = analyzeActivityPatterns(recentActivity);
  
  const healthMetrics = {
    progress_velocity: calculateProgressVelocity(recentActivity),
    stuck_indicators: detectStuckPatterns(recentActivity),
    complexity_level: assessComplexityLevel(recentActivity),
    focus_drift: measureFocusDrift(recentActivity),
    help_seeking: detectHelpSeekingBehavior(recentActivity)
  };
  
  // Generate recommendations
  if (healthMetrics.stuck_indicators.score > 0.8) {
    return {
      status: 'needs_attention',
      issue: 'Appears stuck on current problem',
      evidence: healthMetrics.stuck_indicators.patterns,
      recommendation: 'Consider providing additional context or suggesting a different approach'
    };
  }
  
  if (healthMetrics.focus_drift > 0.6) {
    return {
      status: 'unfocused',
      issue: 'Session jumping between multiple unrelated tasks',
      recommendation: 'Help clarify current priority or break work into focused sub-sessions'
    };
  }
  
  return { status: 'healthy', metrics: healthMetrics };
}
```

## Intelligence Processing Pipeline

### 1. Raw Activity Ingestion
```typescript
// Continuous monitoring of Claude Code activity
async function ingestClaudeActivity(session_id: string) {
  const logWatcher = createClaudeCodeLogWatcher(session_id);
  
  for await (const logEntry of logWatcher) {
    // Parse and categorize activity
    const activity = parseActivity(logEntry);
    
    // Store raw activity
    await storeActivity(session_id, activity);
    
    // Trigger intelligence processing
    await processActivityIntelligence(session_id, activity);
  }
}
```

### 2. Context Enhancement via claude -p
```typescript
// Use Claude to analyze its own activity for higher-level insights
async function enhanceActivityContext(session_id: string, activity: Activity) {
  const contextPrompt = `
    Analyze this Claude Code activity in context:
    
    Session: ${session_id}
    Activity: ${JSON.stringify(activity)}
    Recent context: ${await getRecentContext(session_id)}
    
    Provide analysis:
    1. What is the high-level task or goal?
    2. How does this activity advance that goal?
    3. Any apparent blockers or confusion?
    4. What context might be helpful?
    5. Rate confidence level (0-1) in current approach
  `;
  
  const analysis = await callClaudeP(contextPrompt);
  
  // Update session context with enhanced understanding
  await updateSessionContext(session_id, {
    current_task: analysis.task,
    confidence_level: analysis.confidence,
    potential_blockers: analysis.blockers,
    context_needs: analysis.helpful_context,
    last_analysis: new Date().toISOString()
  });
}
```

### 3. Cross-Session Pattern Recognition
```typescript
// Analyze patterns across all sessions for project-level insights
async function analyzeProjectPatterns(project_id: string) {
  const allSessions = await getProjectSessions(project_id);
  const allActivities = await getProjectActivities(project_id, '24 hours');
  
  // Identify recurring patterns
  const patterns = {
    frequent_files: findMostAccessedFiles(allActivities),
    common_concepts: extractCommonConcepts(allActivities), 
    workflow_patterns: identifyWorkflowPatterns(allActivities),
    collaboration_opportunities: findCollaborationOpportunities(allSessions),
    knowledge_gaps: identifyKnowledgeGaps(allActivities)
  };
  
  // Generate project-level insights
  const insights = await generateProjectInsights(patterns);
  
  // Store for future reference and display
  await storeProjectInsights(project_id, insights);
  
  return insights;
}
```

## Intelligence Interface

### Conversational Intelligence API
All intelligence is accessible through natural language MCP tools:

```typescript
// High-level session analysis
interface IntelligenceTools {
  // Current state analysis
  analyze_current_activity(session_id?: string): SessionAnalysis
  summarize_project_status(project_id: string): ProjectSummary
  identify_blockers(session_id?: string): BlockerAnalysis[]
  
  // Predictive insights
  predict_session_needs(session_id: string): PredictedNeeds
  suggest_next_actions(session_id: string): ActionSuggestion[]
  recommend_coordination(project_id: string): CoordinationRecommendation[]
  
  // Cross-session intelligence
  find_related_work(session_id: string): RelatedSession[]
  analyze_project_patterns(project_id: string): ProjectPatterns
  suggest_session_consolidation(project_id: string): ConsolidationSuggestion[]
  
  // Context management
  preserve_session_context(session_id: string): ContextSnapshot
  restore_session_context(session_id: string, snapshot_id: string): boolean
  share_context_between_sessions(from_session: string, to_session: string): boolean
}
```

### Example Intelligence Interactions
```
Human: "Why has Marcel been stuck for 45 minutes?"

Intelligence Analysis:
"Marcel's session shows classic 'deep debugging' patterns:
- 15 sequential file reads in oauth.js and middleware.js
- 3 attempts to run the same test, all failing
- Recent chat messages show confusion about token expiration logic
- No file writes in last 30 minutes (usually indicates stuck state)

Recommendation: The session needs either:
1. Fresh context about how token expiration should work
2. A different debugging approach (maybe logging/console output)
3. Review of how other similar auth systems handle this pattern"