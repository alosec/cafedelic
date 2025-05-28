# Cafedelic v2 - Watch-Transform-Execute

A ground-up redesign of Cafedelic using the Watch-Transform-Execute (WTE) pattern.

## Core Pattern

```typescript
interface WTE<T, A> {
  watch: () => AsyncGenerator<T>;
  transform: (event: T) => A | null;
  execute: (action: A) => Promise<void>;
}
```

## Architecture

- **No services** - Just functions implementing WTE
- **No events** - Direct function composition
- **No state** - Stateless transformations
- **Shell scripts first** - Direct execution

## Project Structure

```
├── core/           # WTE interfaces and runner
├── watchers/       # Log and file watchers
├── transforms/     # Event transformations
├── executors/      # Action executors
├── pipelines/      # Complete WTE pipelines
└── scripts/        # Shell scripts (from v1)
```

## Running

```bash
npm run build
npm start
```

## Testing

```bash
npm test
```

## Current Implementation

- ✅ Core WTE pattern
- ✅ MCP log watching
- ✅ File operation detection
- ✅ Emacs integration
- 🚧 Git diff pipeline
- 🚧 Pane configuration

Total lines of code: ~150 (vs ~2000 in v1)