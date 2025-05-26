// Emacs Daemon Management Types

export interface EmacsDaemonStatus {
  isAvailable: boolean;      // Is Emacs installed on system?
  isRunning: boolean;         // Is daemon currently active?
  daemonPid?: number;         // Process ID of daemon
  socketPath?: string;        // Active socket path
  socketName?: string;        // Socket name (e.g., 'cafedelic-1234')
  uptime?: number;            // Seconds since daemon start
  lastHealthCheck?: Date;     // Timestamp of last health check
  restartCount: number;       // Total restart attempts
  lastError?: string;         // Most recent error message
  platform: {
    os: 'linux' | 'darwin' | 'win32';
    socketType: 'unix' | 'pipe';
    searchPaths: string[];
  };
}

export interface EmacsDaemonConfig {
  // Daemon management
  manageDaemon: boolean;          // Whether to manage daemon lifecycle
  daemonName?: string;            // Socket name prefix (default: 'cafedelic')
  uniqueSuffix: boolean;          // Add PID suffix to avoid conflicts
  
  // Startup behavior
  lazyInit: boolean;              // Start daemon only on first use
  startupTimeout: number;         // Max milliseconds to wait for startup
  reuseExisting: boolean;         // Use existing daemon if available
  minimalConfig: boolean;         // Start with minimal configuration
  
  // Health monitoring
  healthCheckInterval: number;    // Milliseconds between health checks
  healthCheckTimeout: number;     // Milliseconds before health check fails
  maxRestartAttempts: number;     // Maximum restart attempts
  restartDelays: number[];        // Exponential backoff delays [1000, 2000, 4000, 8000]
  
  // Paths
  emacsExecutable?: string;       // Path to emacs binary
  socketSearchPaths?: string[];   // Additional paths to search for sockets
  configFile?: string;            // Custom config file to load
  
  // Platform overrides
  platformOverrides?: {
    linux?: Partial<EmacsDaemonConfig>;
    darwin?: Partial<EmacsDaemonConfig>;
    win32?: Partial<EmacsDaemonConfig>;
  };
}

export interface SocketSearchResult {
  found: boolean;
  socketPath?: string;
  socketName?: string;
  method: 'environment' | 'default' | 'scan' | 'process';
  error?: string;
}

export interface DaemonStartResult {
  success: boolean;
  pid?: number;
  socketPath?: string;
  message: string;
  startTime?: number;
}

export interface HealthCheckResult {
  healthy: boolean;
  responsive: boolean;
  version?: string;
  loadTime?: number;
  error?: string;
}

export class EmacsDaemonError extends Error {
  constructor(
    message: string,
    public code: 'NOT_INSTALLED' | 'START_FAILED' | 'SOCKET_ERROR' | 'HEALTH_CHECK_FAILED' | 'MAX_RETRIES',
    public details?: any
  ) {
    super(message);
    this.name = 'EmacsDaemonError';
  }
}