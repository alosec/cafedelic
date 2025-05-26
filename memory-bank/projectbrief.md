# Cafedelic Project Brief

## Project Name
Cafedelic - Development Intelligence Framework

## Vision
Create an intelligence layer that makes AI-assisted development transparent and analyzable by watching, translating, and understanding development activity.

## Core Problem
Developers working with AI tools like Claude Desktop lack visibility into what the AI is doing. Log files are opaque, activities are hidden, and there's no accumulated understanding of the development process.

## Solution
An intelligence framework that:
1. Watches development activity logs (starting with Desktop Commander)
2. Translates technical logs into human-readable insights
3. Analyzes patterns and accumulates project knowledge
4. Provides high-level context understanding

## Success Metrics
- Real-time translation of Desktop Commander logs
- Clear visibility into AI development activities
- Accumulated project intelligence over time
- Zero dependency on tmux layout management

## Scope
### In Scope
- DC log watching and translation
- Activity pattern analysis
- Context summarization
- Project intelligence accumulation
- High-level MCP tool interface

### Out of Scope
- Tmux session management
- Layout control
- Low-level pane operations
- Terminal launching
- Direct tmux manipulation

## Technical Constraints
- Express.js server for real-time processing
- File watching without tmux dependencies
- Simple, proven approaches (inspired by teemax success)
- High-level abstractions only

## Timeline
- Phase 1: DC log watch & translate (immediate)
- Phase 2: Pattern analysis & context (next)
- Phase 3: Intelligence accumulation (future)
