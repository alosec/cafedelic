# Feature: Emacs Daemon Lifecycle Management

## Problem Statement

Currently, Cafedelic relies on an external Emacs daemon being available for the auto-file-opening feature to work. This creates several issues:

1. **Availability**: Users must manually start the Emacs daemon before using Cafedelic
2. **Discovery**: The system fails silently when the daemon isn't running
3. **Configuration**: No guarantee the daemon has the correct configuration loaded
4. **Socket Issues**: emacsclient has trouble finding the socket file in various environments
5. **User Experience**: Manual daemon management adds friction to the workflow

## Current Behavior

When Claude accesses files:
```
EmacsService → emacsclient → ❌ "can't find socket; have you started the server?"
```

This requires users to:
- Know they need an Emacs daemon
- Start it manually with `M-x server-start` or `emacs --daemon`
- Ensure socket permissions are correct
- Restart it if it crashes

## Proposed Solution

Add an **EmacsDaemonManager** service that:

1. **Checks daemon status** on Cafedelic startup
2. **Starts a managed daemon** if none exists
3. **Monitors daemon health** during operation
4. **Restarts on failure** with backoff
5. **Manages socket paths** consistently
6. **Loads Cafedelic configuration** automatically

## Implementation Details

### New Service: EmacsDaemonManager

```typescript
class EmacsDaemonManager extends EventEmitter {
  private daemonProcess?: ChildProcess;
  private socketPath: string;
  private configPath: string;
  
  async initialize(): Promise<void> {
    // Check for existing daemon
    if (await this.checkExistingDaemon()) {
      return this.connectToExisting();
    }
    
    // Start managed daemon
    await this.startManagedDaemon();
  }
  
  async startManagedDaemon(): Promise<void> {
    // Start with Cafedelic config
    this.daemonProcess = spawn('emacs', [
      '--daemon=cafedelic',
      '--load', this.configPath
    ]);
    
    // Monitor health
    this.scheduleHealthChecks();
  }
  
  async ensureRunning(): Promise<void> {
    if (!await this.isHealthy()) {
      await this.restart();
    }
  }
}
```

### Configuration Options

```typescript
interface EmacsDaemonConfig {
  // Daemon management
  manageDaemon: boolean;          // Whether to manage daemon lifecycle
  daemonName: string;             // Socket name (default: 'cafedelic')
  
  // Startup behavior  
  startupTimeout: number;         // Max time to wait for daemon startup
  reuseExisting: boolean;         // Use existing daemon if available
  
  // Health monitoring
  healthCheckInterval: number;    // How often to check daemon health
  maxRestartAttempts: number;     // Restart attempts before giving up
  restartBackoff: number;         // Backoff multiplier for restarts
  
  // Paths
  emacsExecutable: string;        // Path to emacs (default: 'emacs')
  socketDir?: string;             // Override socket directory
  configFile?: string;            // Custom config to load
}
```

### Integration Points

1. **Startup**: EmacsDaemonManager initializes before StateManager
2. **EmacsService**: Uses manager to ensure daemon before operations
3. **Health Monitoring**: Regular checks with automatic recovery
4. **Shutdown**: Graceful daemon shutdown with Cafedelic

### Benefits

- **Zero Configuration**: Works out of the box
- **Reliability**: Automatic recovery from daemon crashes
- **Isolation**: Dedicated daemon for Cafedelic
- **Performance**: Pre-loaded configuration
- **Debugging**: Clear daemon status in `get_emacs_status`

## Alternative Approaches Considered

1. **Direct Emacs Invocation**: Too slow for real-time file opening
2. **Embedded Emacs**: Too complex and platform-specific
3. **Optional Feature**: Poor user experience when not configured

## Implementation Priority

**High** - Core feature (auto-file-opening) depends on this. Without reliable daemon management, the user experience is significantly degraded.

## Testing Considerations

- Daemon startup/shutdown cycles
- Multiple daemon detection
- Socket permission handling
- Cross-platform socket paths
- Recovery from daemon crashes
- Performance impact of health checks

## Success Criteria

- [ ] Cafedelic starts its own Emacs daemon automatically
- [ ] File opening works immediately after Cafedelic starts
- [ ] Daemon failures are detected and recovered
- [ ] Clear status reporting via `get_emacs_status`
- [ ] Graceful handling when Emacs isn't installed
- [ ] No interference with user's existing Emacs daemons