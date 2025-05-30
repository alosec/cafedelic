# Technical Implementation Summary

## Immediate Actions

1. **Create MCP tool endpoints** in cafedelic that mirror deli's functionality
2. **Write simple sh scripts** for pane management operations
3. **Add routing configuration** to the WTE pipelines

## Key Integration Points

### Current Pipeline (index.ts)
```typescript
// Currently runs two fixed pipelines
runPipeline(fileToEmacs)
runPipeline(claudeCodeToEmacsDebounced)
```

### Needed Enhancement
```typescript
// Add routing configuration reading
const routing = await loadRoutingConfig();

// Modify executors to use dynamic destinations
const configuredExecutor = emacsExecutor(routing.getDestination('files'));
```

### Script Examples
```bash
# scripts/set-pane-role.sh
PANE_ID=$1
ROLE=$2
echo "$PANE_ID=$ROLE" >> ~/.cafedelic/pane-roles

# scripts/get-pane-for-role.sh
ROLE=$1
grep "=$ROLE" ~/.cafedelic/pane-roles | cut -d= -f1
```

## File Structure Needed
```
cafedelic/
├── scripts/
│   ├── pane-management/
│   │   ├── assign-name.sh
│   │   ├── read-pane.sh
│   │   └── send-to-pane.sh
│   └── routing/
│       ├── set-destination.sh
│       └── get-config.sh
└── .cafedelic/  (runtime config)
    ├── pane-names
    └── routing-config
```

This gives us a clear path from current state to the configurable system you envision.
