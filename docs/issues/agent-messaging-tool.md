# Feature: Agent Messaging Tool with Proper Formatting Support

## Problem Statement

The current `send_to_pane` tool from deli strips important formatting when sending messages to agent panes, making technical communication difficult. Code blocks, special characters, and formatting are lost in transmission.

### Example of Formatting Loss

**Original Message Sent:**
```
## Feedback on Emacs Daemon Manager Implementation

Your plan looks solid! Here are some specific enhancements to consider before implementation:

### 1. Socket Discovery Strategy
```typescript
const SOCKET_SEARCH_PATHS = [
  '~/.emacs.d/server/',
  '/run/user/1000/emacs/',
  '$TMPDIR/emacs${uid}/',
  '/tmp/emacs${uid}/'
];
```

### 2. Minimal Configuration Approach
Start with just:
```elisp
(server-start)
(setq server-name "cafedelic")
```
```

**What Agent Received:**
```
## Feedback on Emacs Daemon Manager Implementation
Your plan looks solid! Here are some specific enhancements to consider before
implementation:
### 1. Socket Discovery Strategy
### 2. Minimal Configuration Approach
Start with just:
Then progressively load cafedelic-editor.el only if needed.
```

### Issues Identified

1. **Code blocks completely stripped** - All ```typescript and ```elisp blocks removed
2. **Content after special chars lost** - Everything after `$` in paths disappeared
3. **Line wrapping issues** - Text reformatted incorrectly
4. **Loss of structure** - Important technical details missing

## Proposed Solution

Create a new MCP tool specifically for agent communication:

```typescript
interface SendMessageToAgentParams {
  agent: string;      // Agent name (e.g., "marcel", "sophie")
  mode: 'plan' | 'act' | 'review';  // Conversation mode
  content: string;    // Message content with full formatting
}

async function send_message_to_agent(params: SendMessageToAgentParams): Promise<void> {
  // Implementation that preserves formatting
}
```

## Design Considerations

### 1. Natural Conversation Format

Agents respond best to natural conversation. The tool should:
- Add appropriate mode indicators (`-- mode:plan --` or `-- mode:act --`)
- Preserve conversational tone
- Maintain markdown formatting
- Support code blocks and technical content

### 2. Formatting Preservation

The implementation must:
- Escape special characters properly
- Preserve code blocks with language indicators
- Maintain indentation and structure
- Handle multi-line content correctly

### 3. Agent Context

Each agent has a specific role and context:
- Marcel: Implementation and coding
- Sophie: Editor and file management
- Pierre: Activity monitoring
- Jacques: Log analysis

The tool should be aware of agent roles for better message framing.

## Implementation Approach

### Option 1: Enhance Existing Tool
- Fix `send_to_pane` formatting issues in deli
- Add proper escaping and preservation logic
- Backward compatible but may affect other uses

### Option 2: New Cafedelic Tool (Recommended)
- Create `send_message_to_agent` in cafedelic
- Purpose-built for agent communication
- Can use tmux directly with proper escaping
- Maintains separation of concerns

### Option 3: Template System
- Create message templates for common scenarios
- Pre-escaped and tested formats
- Less flexible but more reliable

## Recommended Implementation

```typescript
// src/tools/send_message_to_agent.ts
export async function send_message_to_agent(params: {
  agent: string;
  mode: 'plan' | 'act' | 'review';
  content: string;
}): Promise<{ success: boolean; message: string }> {
  const { agent, mode, content } = params;
  
  // Get agent's pane
  const paneId = await getAgentPane(agent);
  if (!paneId) {
    return { success: false, message: `Agent ${agent} not found` };
  }
  
  // Format message with mode
  const formattedMessage = formatAgentMessage(mode, content);
  
  // Send with proper escaping
  await sendToPane(paneId, formattedMessage);
  
  return { success: true, message: `Message sent to ${agent}` };
}

function formatAgentMessage(mode: string, content: string): string {
  // Add mode indicator
  let message = content;
  if (mode) {
    message = `${content}\n\n-- mode:${mode} --`;
  }
  
  return message;
}

function escapeForTmux(text: string): string {
  // Preserve formatting while escaping for tmux
  return text
    .replace(/\$/g, '\\$')  // Escape dollar signs
    .replace(/`/g, '\\`')   // Escape backticks
    .replace(/"/g, '\\"')   // Escape quotes
    .split('\n')
    .map(line => `"${line}"`)
    .join(' Enter ');
}
```

## Success Criteria

- [ ] Code blocks preserved in agent messages
- [ ] Special characters handled correctly
- [ ] Multi-line content maintains structure
- [ ] Mode indicators properly appended
- [ ] Natural conversation flow maintained
- [ ] Works with all named agents

## Testing

Test cases should include:
1. Simple text messages
2. Messages with code blocks in multiple languages
3. Messages with special characters ($, `, ", \)
4. Multi-paragraph technical documentation
5. Mode switching (plan → act → review)

## Priority

**High** - This directly impacts the ability to coordinate between multiple AI agents, which is essential for complex development tasks. Without proper formatting, technical feedback and code examples cannot be communicated effectively.