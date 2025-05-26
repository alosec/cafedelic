# Feature: Pane Display State Management Abstraction

## Problem Statement

Currently, pane content updates are handled through various mechanisms (tmux send-keys, tmux run-shell, direct command execution) without a unified abstraction. This leads to:

1. **Inconsistent Updates**: Different tools update panes differently
2. **State vs View Confusion**: Messages about state appear instead of actual view updates
3. **No Unified API**: Each service implements its own pane update logic
4. **Display Persistence**: No way to ensure pane shows correct content after updates

## Current Behavior

When updating pane content:
- Output routing sends status messages: "Opened file: X" 
- Emacs commands execute but may not update the visible frame
- Terminal panes may show command output instead of desired state
- No guarantee that pane displays intended content

## Proposed Solution

Create a **PaneDisplayManager** service that provides:

```typescript
interface PaneDisplayManager {
  // Upsert display content for a pane
  upsertDisplay(paneId: string, content: DisplayContent): Promise<void>;
  
  // Get current display state
  getDisplayState(paneId: string): Promise<DisplayState>;
  
  // Register display providers
  registerProvider(type: string, provider: DisplayProvider): void;
}

interface DisplayContent {
  type: 'emacs-buffer' | 'terminal-output' | 'log-stream' | 'status-message';
  content: any; // Type-specific content
  priority: number; // For handling conflicts
  persistent: boolean; // Should survive pane switches
}
```

## Use Cases

### Emacs Buffer Updates
```typescript
await paneDisplay.upsertDisplay('sophie', {
  type: 'emacs-buffer',
  content: { 
    file: '/path/to/file.ts',
    line: 42,
    column: 10
  },
  priority: 100,
  persistent: true
});
```

### Status Messages
```typescript
await paneDisplay.upsertDisplay('sophie', {
  type: 'status-message',
  content: 'Opening file...',
  priority: 50,
  persistent: false
});
```

## Implementation Considerations

1. **Display Providers**: Each display type has a provider that knows how to render
2. **Priority System**: Higher priority content takes precedence
3. **Persistence**: Some content should persist, others are transient
4. **Verification**: Ability to verify display state matches expected
5. **Recovery**: Re-render if pane gets corrupted

## Benefits

- Unified API for all pane updates
- Clear separation between state and view
- Ability to verify and recover display state
- Better debugging of display issues
- Foundation for future UI features

## Priority

**Medium** - While not blocking core functionality, this abstraction would significantly improve reliability and debuggability of the UI layer.