# Cafedelic Notification System Plan

## Overview
Add a `notifications` field to all Cafedelic tools to enable cross-communication between Claude Code instances, Claude Desktop, and external systems like Emacs. This creates an organic way to send messages and feedback without interrupting agent flow.

## Core Concept
- Each tool call checks for pending notifications
- Notifications are stored in SQLite for persistence
- Messages can come from other Claude instances, Emacs captures, or system events
- Enables real-time nudges, feedback, and approval requests

## Database Schema

```sql
-- Notifications table
CREATE TABLE notifications (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source TEXT NOT NULL,           -- 'claude_code', 'claude_desktop', 'emacs', 'system'
    target TEXT,                    -- specific instance ID or 'all'
    priority TEXT DEFAULT 'normal', -- 'urgent', 'high', 'normal', 'low'
    message TEXT NOT NULL,
    metadata JSON,                  -- additional context
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    read_at TIMESTAMP,
    acknowledged_at TIMESTAMP,
    expires_at TIMESTAMP,
    status TEXT DEFAULT 'pending'   -- 'pending', 'read', 'acknowledged', 'expired'
);

-- Notification sources registry
CREATE TABLE notification_sources (
    id TEXT PRIMARY KEY,            -- unique identifier for each source
    type TEXT NOT NULL,             -- 'claude_code', 'claude_desktop', 'emacs', etc.
    name TEXT,
    last_seen TIMESTAMP,
    metadata JSON
);

-- Indexes for performance
CREATE INDEX idx_notifications_status ON notifications(status);
CREATE INDEX idx_notifications_target ON notifications(target);
CREATE INDEX idx_notifications_created ON notifications(created_at);
```

## Tool Interface Enhancement

### Modified Tool Response Structure
```typescript
interface CafedelicToolResponse<T> {
    result: T;
    notifications?: Notification[];
}

interface Notification {
    id: number;
    source: string;
    priority: 'urgent' | 'high' | 'normal' | 'low';
    message: string;
    metadata?: any;
    created_at: string;
}
```

### Tool Implementation Pattern
```typescript
// In each tool's execute function
async function executeWithNotifications<T>(
    toolFunction: () => Promise<T>
): Promise<CafedelicToolResponse<T>> {
    // Check for pending notifications
    const notifications = await notificationService.getPending();
    
    // Execute the actual tool
    const result = await toolFunction();
    
    // Mark notifications as read
    await notificationService.markAsRead(notifications.map(n => n.id));
    
    return {
        result,
        notifications: notifications.length > 0 ? notifications : undefined
    };
}
```

## Notification Service

```typescript
class NotificationService {
    async send(notification: {
        source: string;
        target?: string;
        priority?: string;
        message: string;
        metadata?: any;
        expires_at?: Date;
    }): Promise<number>;
    
    async getPending(target?: string): Promise<Notification[]>;
    
    async markAsRead(ids: number[]): Promise<void>;
    
    async acknowledge(id: number): Promise<void>;
    
    async cleanup(): Promise<void>; // Remove expired notifications
}
```

## Emacs Integration

### Capture Template
```elisp
;; Emacs capture for sending notifications to Claude
(defun cafedelic-send-notification (message &optional priority metadata)
  "Send a notification to Claude instances via Cafedelic"
  (let* ((notification (json-encode
                       `((source . "emacs")
                         (priority . ,(or priority "normal"))
                         (message . ,message)
                         (metadata . ,metadata))))
         (command (format "node -e \"
           const db = require('better-sqlite3')('%s');
           const notif = %s;
           db.prepare('INSERT INTO notifications (source, priority, message, metadata) VALUES (?, ?, ?, ?)').run(
             notif.source, notif.priority, notif.message, JSON.stringify(notif.metadata)
           );
         \"" cafedelic-db-path notification)))
    (shell-command command)))

;; Interactive capture
(defun cafedelic-capture-notification ()
  "Interactively capture and send a notification to Claude"
  (interactive)
  (let ((message (read-string "Message for Claude: "))
        (priority (completing-read "Priority: " '("urgent" "high" "normal" "low") nil t "normal")))
    (cafedelic-send-notification message priority)))
```

## Implementation Phases

### Phase 1: Core Infrastructure
1. Create notification tables in existing SQLite database
2. Implement NotificationService class
3. Create base wrapper for tool responses

### Phase 2: Tool Integration
1. Modify each Cafedelic tool to check notifications
2. Update MCP tool definitions to include notification field
3. Test with simple notification flow

### Phase 3: Emacs Integration
1. Create Emacs notification functions
2. Add capture templates and keybindings
3. Test real-time feedback during Claude operations

### Phase 4: Advanced Features
1. Notification filtering and routing
2. Priority-based handling
3. Approval request workflows
4. Notification history and analytics

## Use Case Examples

### 1. Subagent Approval Request
```typescript
// Subagent sends approval request
await notificationService.send({
    source: 'claude_code_instance_2',
    target: 'claude_desktop',
    priority: 'high',
    message: 'Approval needed: About to modify database schema. Proceed?',
    metadata: {
        type: 'approval_request',
        action: 'database_migration',
        details: { /* migration details */ }
    }
});
```

### 2. Real-time Feedback from Emacs
```elisp
;; While Claude is working, user can send feedback
(cafedelic-send-notification 
  "Focus on error handling in the authentication flow"
  "high"
  '((context . "code_review")))
```

### 3. Progress Updates Between Instances
```typescript
// Claude Code notifies Claude Desktop of progress
await notificationService.send({
    source: 'claude_code_instance_1',
    target: 'claude_desktop',
    priority: 'normal',
    message: 'Completed 5/10 test files. All passing so far.',
    metadata: {
        type: 'progress_update',
        completed: 5,
        total: 10
    }
});
```

## Benefits

1. **Non-Intrusive Communication**: Messages delivered during natural tool calls
2. **Persistent Queue**: SQLite ensures messages aren't lost
3. **Flexible Routing**: Target specific instances or broadcast
4. **Real-time Feedback**: Users can guide agents without interruption
5. **Cross-Platform**: Works between different Claude environments

## Next Steps

1. Review and approve this plan
2. Implement Phase 1 infrastructure
3. Create a simple test tool with notifications
4. Build Emacs integration
5. Roll out to all Cafedelic tools