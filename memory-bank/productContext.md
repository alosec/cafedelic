# Product Context

## Why Cafedelic Exists

AI development tools operate as black boxes. When Claude Desktop works on your code, you see results but not the journey. Cafedelic makes the invisible visible while providing control over how and where insights appear.

### Problems We Solve

1. **Lost Context**: What files did Claude read? What patterns did it find?
2. **Opaque Decision-Making**: Why did Claude choose this approach?
3. **Static Display**: Output trapped in Claude's interface
4. **Manual Coordination**: No automatic editor synchronization
5. **Inflexible Layouts**: Hard-coded assumptions about user setup

## The Transparency & Control Gap

Current tools provide either:
- **Too Low Level**: Raw tmux commands, pane coordinates
- **Too High Level**: Final outputs without process visibility
- **Too Rigid**: Fixed layouts and destinations

Cafedelic provides the middle ground - intelligent visibility with flexible control.

## How It Works

### Watch & Translate
Monitor MCP logs and translate to human insights:
```
From: {"method":"tools/call","params":{"name":"read_file","arguments":{"path":"/src/auth.js"}}}
To: "Claude is reading the authentication module"
```

### Route Dynamically
Users configure where insights appear:
```
setEditorDestination("0:0.1")  // Your preferred pane
→ Activity streams to YOUR layout
→ Files open in YOUR editor setup
```

### Integrate Intelligently
- **Emacs Integration**: Files/directories open automatically
- **Pane Servers**: Independent Emacs servers per pane
- **TMEX Layouts**: Create and manage complex layouts
- **Activity Context**: Query current activity via MCP

## User Experience

### For Developers
Real-time visibility with full control:
```
[3:42pm] Claude started exploring authentication → Opens in YOUR editor
[3:43pm] Reading: auth.js, login.js → Visible in YOUR activity pane
[3:44pm] Testing login flow → Output in YOUR terminal
```

### Flexible Configuration
```javascript
// Set your editor pane
setEditorDestination("main:dev.1")

// Query routing state
getRoutingAssignments()

// Adjust layouts on the fly
create_tmex_layout("2:0", "222")
```

## Design Principles

1. **Transparency First**: Make AI activity visible
2. **User Control**: No hard-coded assumptions
3. **Progressive Enhancement**: Start simple, add features
4. **Integration Over Isolation**: Work with existing tools
5. **Shell Scripts as Foundation**: Leverage proven solutions

## Evolution Path

### Today: Single Agent Transparency
- One Claude Desktop, full visibility
- Flexible routing to any pane
- Automatic editor integration

### Tomorrow: Multi-Agent Orchestration  
- Multiple Claude instances
- Coordinated development
- Shared context and state

### Future: Intelligent Platform
- Pattern recognition
- Workflow optimization
- Accumulated intelligence
