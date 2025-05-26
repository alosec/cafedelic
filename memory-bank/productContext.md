# Product Context

## Why Cafedelic Exists

AI development tools operate as black boxes. When Claude Desktop works on your code, you see the results but not the journey. This lack of transparency creates several problems:

1. **Lost Context**: What files did Claude read? What patterns did it find?
2. **Opaque Decision-Making**: Why did Claude choose this approach?
3. **No Learning**: Each session starts fresh without accumulated knowledge
4. **Missed Insights**: Patterns in development activity go unnoticed

## The Intelligence Gap

Current tools provide either:
- **Too Low Level**: Raw tmux commands, pane coordinates, technical details
- **Too High Level**: Final outputs without the process

Cafedelic fills the middle ground - the intelligence layer that understands and explains.

## How It Works

### Watch
Cafedelic monitors development activity through available logs:
- Desktop Commander logs (primary source)
- Future: Other tool logs, file system events

### Translate
Raw logs become human insights:
```
From: {"command": "read_file", "args": {"path": "/src/auth.js"}}
To: "Claude is reading the authentication module"
```

### Analyze
Patterns emerge from translated activities:
- Which files are accessed together?
- What workflows are repeated?
- Where does development focus?

### Accumulate
Intelligence grows over time:
- Project understanding deepens
- Common patterns are recognized
- Context becomes richer

## User Experience

### For Developers
- See what AI tools are doing in real-time
- Understand the development process
- Build on accumulated project knowledge

### For AI Assistants
- Access rich project context
- Understand development history
- Make informed decisions

## Design Principles

1. **Transparency First**: Make the invisible visible
2. **High-Level Intelligence**: Focus on understanding, not control
3. **Progressive Enhancement**: Start simple, add intelligence
4. **Tool Agnostic**: Work with any development tool that produces logs

## What Success Looks Like

A developer opens their terminal and sees:
```
[3:42pm] Claude started exploring the authentication system
[3:42pm] Reading: auth.js, login.js, session.js
[3:43pm] Claude identified the JWT implementation pattern
[3:43pm] Testing the login flow...
[3:44pm] All auth tests passing
```

Not tmux commands. Not JSON logs. Human understanding of AI activity.
