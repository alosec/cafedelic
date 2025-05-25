# Cafedelic

Fractal recursive analysis, summarization, and context management terminal-based IDE built using tmux and emacs.

## Philosophy

Cafedelic follows an austere, minimal approach. We prefer inconvenience from refusing to create complexity over the complexity itself.

## Features

- **launch_ide**: Creates a functional tmux-based IDE layout
- **get_context**: (Stub) Future context management capabilities

## Installation

```bash
npm install
npm run build
```

## Usage

Add to your Claude Desktop configuration:

```json
{
  "cafedelic": {
    "command": "node",
    "args": ["/path/to/cafedelic/dist/index.js"]
  }
}
```

Then use the tools:
- `launch_ide` - Creates IDE layout with named panes
- `get_context` - Retrieves project context (stub)

## Named Panes

The IDE creates four named panes:
- `cafedelic-activity` - Activity monitor
- `cafedelic-editor` - Emacs editor
- `cafedelic-terminal` - Terminal
- `cafedelic-logs` - Context/Logs view

## Development

```bash
npm run dev  # Build and run
```
