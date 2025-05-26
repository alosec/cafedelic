# Agent Messaging Tool Implementation Plan

## Overview

This plan addresses the formatting loss issues identified in Issue #6 by creating a dedicated agent messaging tool that preserves markdown formatting, code blocks, and special characters when sending messages between AI agents.

## Implementation Architecture

### File Structure
```
└── src/
    ├── tools/
    │   └── send_message_to_agent.ts - MCP tool for sending formatted messages to agents
    ├── services/
    │   └── agent-messenger.service.ts - Core service handling message formatting and delivery
    └── types/
        └── agent-messaging.types.ts - Type definitions for agent messaging system
```

## Component Details

### 1. Agent Messenger Service (`agent-messenger.service.ts`)

Core service providing message formatting and delivery capabilities:

```typescript
interface AgentMessengerService {
  // Send formatted message to agent pane
  sendMessage(agent: string, content: string, mode?: string): Promise<MessagingResult>
  
  // Get agent pane mapping
  getAgentPane(agent: string): Promise<string | null>
  
  // Format message with proper escaping
  formatMessage(content: string, mode?: string): string
  
  // Escape content for tmux while preserving formatting
  escapeForTmux(content: string): string
}
```

**Key Features:**
- Agent-to-pane mapping: 
  - Pierre (9:0.0) - Activity Monitor
  - Sophie (9:0.2) - Emacs Editor
  - Marcel (9:0.3) - Implementation Agent
  - Jacques (9:0.4) - Log Analysis
- Smart escaping that preserves code blocks and markdown
- Mode support for conversation context (plan/act/review)
- Comprehensive error handling

### 2. MCP Tool Interface (`send_message_to_agent.ts`)

Exposes the service as an MCP tool for Claude:

```typescript
interface SendMessageToAgentParams {
  agent: string;      // Agent name: "marcel", "sophie", "pierre", "jacques"
  content: string;    // Message content with full formatting
  mode?: 'plan' | 'act' | 'review';  // Optional conversation mode
}

async function send_message_to_agent(params: SendMessageToAgentParams): Promise<{
  success: boolean;
  message: string;
  destination?: string;
}> {
  // Implementation
}
```

### 3. Type Definitions (`agent-messaging.types.ts`)

```typescript
export type AgentName = 'pierre' | 'sophie' | 'marcel' | 'jacques';
export type ConversationMode = 'plan' | 'act' | 'review';

export interface MessagingResult {
  success: boolean;
  message: string;
  destination?: string;
  agent?: AgentName;
}

export interface AgentConfig {
  name: AgentName;
  paneId: string;
  role: string;
  description: string;
}
```

## Formatting Preservation Strategy

### Current Problem
Using basic `tmux send-keys` or `tmux run-shell` strips formatting:
- Code blocks are removed
- Content after special characters ($, `) is lost
- Line structure is destroyed

### Solution Approach

1. **Code Block Preservation**
   - Parse content to identify code blocks
   - Send opening/closing markers separately
   - Preserve language indicators

2. **Special Character Handling**
   ```typescript
   function escapeForTmux(text: string): string {
     return text
       .replace(/\$/g, '\\$')      // Escape dollar signs
       .replace(/`/g, '\\`')       // Escape backticks
       .replace(/"/g, '\\"')       // Escape quotes
       .replace(/\\/g, '\\\\');    // Escape backslashes
   }
   ```

3. **Line-by-Line Sending**
   - Split content by newlines
   - Send each line with proper Enter key
   - Maintain indentation and structure

4. **Visual Formatting**
   - Add clear message boundaries
   - Include timestamp/sender info
   - Append mode indicators when specified

## Testing Strategy

### Test Cases

1. **Basic Messages**
   - Simple text
   - Multi-line content
   - Unicode characters

2. **Code Blocks**
   ```markdown
   Test sending:
   ```typescript
   const example = "test";
   ```
   ```

3. **Special Characters**
   - Paths: `/home/user/$HOME/project`
   - Commands: `npm install && npm test`
   - Quotes: "test" and 'test'

4. **Complex Documents**
   - Mixed markdown with headers, lists, code
   - Technical documentation
   - Multi-paragraph explanations

5. **Mode Switching**
   - Messages with mode indicators
   - Mode transitions
   - Mode context preservation

### Testing Implementation

Create `test-agent-messaging.sh` script that:
1. Sends test messages to each agent
2. Verifies formatting preservation
3. Tests error cases (unknown agents, invalid content)
4. Validates mode handling

## Integration Points

### With Existing Systems

1. **Output Router Service**
   - Follow similar patterns for pane discovery
   - Use existing logging infrastructure
   - Align with configuration management

2. **Tmux Session Management**
   - Work with existing session/window naming
   - Respect current pane layout (131 transpose)
   - Compatible with cafedelic-session structure

3. **Event System**
   - Emit events for message sending
   - Allow monitoring of agent communication
   - Enable future webhook/notification support

## Implementation Priorities

### Phase 1: Core Functionality
1. Create type definitions
2. Implement agent-messenger service
3. Build basic MCP tool
4. Test with simple messages

### Phase 2: Formatting Support
1. Add code block parsing
2. Implement special character escaping
3. Handle multi-line content
4. Test with complex messages

### Phase 3: Enhanced Features
1. Add mode support
2. Implement message history
3. Add delivery confirmation
4. Create comprehensive tests

## Success Metrics

- [ ] All test cases pass
- [ ] Code blocks preserved completely
- [ ] Special characters handled correctly
- [ ] Multi-line structure maintained
- [ ] Mode indicators work properly
- [ ] Error handling is graceful
- [ ] Integration with existing tools is seamless

## Next Steps

1. Review and approve this implementation plan
2. Create the type definitions first
3. Implement the core service
4. Build the MCP tool interface
5. Create comprehensive tests
6. Document usage examples

This implementation will resolve the formatting issues and enable proper communication between AI agents in the cafedelic system.