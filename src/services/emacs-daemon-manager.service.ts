// Emacs Daemon Manager - Lifecycle management with lazy initialization

import { EventEmitter } from 'events';
import { spawn, ChildProcess } from 'child_process';
import { exec } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import * as fs from 'fs/promises';
import { 
  EmacsDaemonStatus, 
  EmacsDaemonConfig, 
  DaemonStartResult,
  HealthCheckResult,
  EmacsDaemonError 
} from '../types/emacs-daemon.types.js';
import { discoverEmacsSocket, getSocketPath } from '../utils/socket-discovery.js';
import { logger } from '../utils/logger.js';
import { configManager } from '../config/cafedelic.config.js';

const execAsync = promisify(exec);

export class EmacsDaemonManager extends EventEmitter {
  private config: EmacsDaemonConfig;
  private daemonProcess?: ChildProcess;
  private status: EmacsDaemonStatus;
  private healthCheckTimer?: NodeJS.Timeout;
  private restartAttempt: number = 0;
  private lastHealthCheck?: Date;
  private statusCacheTime: number = 5000; // 5 seconds
  private lastStatusCheck: number = 0;
  private isInitialized: boolean = false;
  private initializationPromise?: Promise<void>;

  constructor() {
    super();
    this.config = this.loadConfig();
    this.status = this.createInitialStatus();
  }

  private loadConfig(): EmacsDaemonConfig {
    const userConfig = configManager.getConfig().emacs?.daemon || {};
    const defaults: EmacsDaemonConfig = {
      manageDaemon: true,
      uniqueSuffix: true,
      lazyInit: true,
      startupTimeout: 30000,
      reuseExisting: true,
      minimalConfig: true,
      healthCheckInterval: 60000,
      healthCheckTimeout: 5000,
      maxRestartAttempts: 5,
      restartDelays: [1000, 2000, 4000, 8000, 16000],
      emacsExecutable: 'emacs'
    };

    // Apply platform overrides
    const platform = process.platform as 'linux' | 'darwin' | 'win32';
    const platformConfig = userConfig.platformOverrides?.[platform] || {};
    
    return { ...defaults, ...userConfig, ...platformConfig };
  }

  private createInitialStatus(): EmacsDaemonStatus {
    const platform = process.platform as 'linux' | 'darwin' | 'win32';
    return {
      isAvailable: false,
      isRunning: false,
      restartCount: 0,
      platform: {
        os: platform,
        socketType: platform === 'win32' ? 'pipe' : 'unix',
        searchPaths: []
      }
    };
  }

  // Lazy initialization - called on first emacs operation
  async ensureInitialized(): Promise<void> {
    if (this.isInitialized) return;
    
    // Prevent multiple simultaneous initializations
    if (this.initializationPromise) {
      return this.initializationPromise;
    }

    this.initializationPromise = this.initialize();
    await this.initializationPromise;
    this.initializationPromise = undefined;
  }

  private async initialize(): Promise<void> {
    logger.info('Initializing Emacs daemon manager', { config: this.config });

    // Check if Emacs is installed
    const isAvailable = await this.checkEmacsAvailable();
    this.status.isAvailable = isAvailable;

    if (!isAvailable) {
      logger.warn('Emacs not found on system, disabling daemon management');
      this.emit('emacs-unavailable');
      this.isInitialized = true;
      return;
    }

    // Check for existing daemon if reuse is enabled
    if (this.config.reuseExisting) {
      const existingSocket = await discoverEmacsSocket(this.config.daemonName);
      if (existingSocket.found) {
        logger.info('Found existing Emacs daemon', existingSocket);
        this.status.isRunning = true;
        this.status.socketPath = existingSocket.socketPath;
        this.status.socketName = existingSocket.socketName;
        this.startHealthMonitoring();
        this.isInitialized = true;
        return;
      }
    }

    // Start managed daemon if configured
    if (this.config.manageDaemon) {
      await this.startDaemon();
    }

    this.isInitialized = true;
  }

  private async checkEmacsAvailable(): Promise<boolean> {
    try {
      const { stdout } = await execAsync(`"${this.config.emacsExecutable}" --version`);
      return stdout.includes('GNU Emacs');
    } catch {
      return false;
    }
  }

  private async startDaemon(): Promise<DaemonStartResult> {
    const socketName = this.generateSocketName();
    const socketPath = getSocketPath(socketName);

    logger.info('Starting Emacs daemon', { socketName, socketPath });

    try {
      // Ensure socket directory exists
      const socketDir = path.dirname(socketPath);
      if (socketDir) {
        await fs.mkdir(socketDir, { recursive: true });
      }

      // Build daemon arguments
      const args = [`--daemon=${socketName}`];
      
      if (this.config.minimalConfig) {
        // Start with minimal config for faster startup
        args.push('--quick', '--no-site-file');
      } else if (this.config.configFile) {
        args.push('--load', this.config.configFile);
      }

      // Start the daemon
      this.daemonProcess = spawn(this.config.emacsExecutable || 'emacs', args, {
        detached: true,
        stdio: 'ignore'
      });

      const startTime = Date.now();

      // Wait for daemon to be ready
      const ready = await this.waitForDaemon(socketName, this.config.startupTimeout);
      
      if (!ready) {
        throw new EmacsDaemonError(
          'Daemon failed to start within timeout',
          'START_FAILED',
          { timeout: this.config.startupTimeout }
        );
      }

      const pid = await this.getDaemonPid(socketName);
      
      this.status.isRunning = true;
      this.status.daemonPid = pid;
      this.status.socketPath = socketPath;
      this.status.socketName = socketName;
      this.status.uptime = 0;

      this.startHealthMonitoring();
      this.emit('daemon-started', { pid, socketName, socketPath });

      logger.info('Emacs daemon started successfully', { 
        pid, 
        socketName, 
        startupTime: Date.now() - startTime 
      });

      return {
        success: true,
        pid,
        socketPath,
        message: 'Daemon started successfully',
        startTime: Date.now() - startTime
      };

    } catch (error) {
      logger.error('Failed to start Emacs daemon', { error, socketName });
      
      if (error instanceof EmacsDaemonError) {
        throw error;
      }
      
      throw new EmacsDaemonError(
        'Failed to start Emacs daemon',
        'START_FAILED',
        { originalError: error }
      );
    }
  }

  private generateSocketName(): string {
    const base = this.config.daemonName || 'cafedelic';
    
    if (this.config.uniqueSuffix) {
      return `${base}-${process.pid}`;
    }
    
    return base;
  }

  private async waitForDaemon(socketName: string, timeout: number): Promise<boolean> {
    const startTime = Date.now();
    const checkInterval = 500;

    while (Date.now() - startTime < timeout) {
      try {
        const { stdout } = await execAsync(
          `emacsclient --socket-name=${socketName} --eval "(emacs-pid)"`,
          { timeout: checkInterval }
        );
        
        if (stdout.trim()) {
          return true;
        }
      } catch {
        // Not ready yet
      }

      await new Promise(resolve => setTimeout(resolve, checkInterval));
    }

    return false;
  }

  private async getDaemonPid(socketName: string): Promise<number | undefined> {
    try {
      const { stdout } = await execAsync(
        `emacsclient --socket-name=${socketName} --eval "(emacs-pid)"`
      );
      const pid = parseInt(stdout.trim());
      return isNaN(pid) ? undefined : pid;
    } catch {
      return undefined;
    }
  }

  private startHealthMonitoring(): void {
    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
    }

    this.healthCheckTimer = setInterval(
      () => this.performHealthCheck(),
      this.config.healthCheckInterval
    );

    // Perform initial health check
    this.performHealthCheck();
  }

  private async performHealthCheck(): Promise<void> {
    const health = await this.checkHealth();
    this.lastHealthCheck = new Date();

    if (!health.healthy) {
      logger.warn('Emacs daemon health check failed', health);
      this.status.lastError = health.error;
      
      if (this.config.manageDaemon && this.restartAttempt < this.config.maxRestartAttempts) {
        await this.restartDaemon();
      }
    } else {
      // Reset restart counter on successful health check
      this.restartAttempt = 0;
      
      // Update uptime if we have a start time
      if (this.status.daemonPid) {
        const uptimeCheck = await this.getUptime();
        if (uptimeCheck !== undefined) {
          this.status.uptime = uptimeCheck;
        }
      }
    }
  }

  private async checkHealth(): Promise<HealthCheckResult> {
    if (!this.status.isRunning || !this.status.socketName) {
      return { healthy: false, responsive: false, error: 'Daemon not running' };
    }

    try {
      const startTime = Date.now();
      const { stdout } = await execAsync(
        `emacsclient --socket-name=${this.status.socketName} --eval "(progn (emacs-version) t)"`,
        { timeout: this.config.healthCheckTimeout }
      );

      const loadTime = Date.now() - startTime;
      
      return {
        healthy: true,
        responsive: true,
        version: stdout.trim(),
        loadTime
      };
    } catch (error) {
      return {
        healthy: false,
        responsive: false,
        error: error instanceof Error ? error.message : 'Health check failed'
      };
    }
  }

  private async getUptime(): Promise<number | undefined> {
    if (!this.status.socketName) return undefined;

    try {
      const { stdout } = await execAsync(
        `emacsclient --socket-name=${this.status.socketName} --eval "(float-time (time-subtract (current-time) before-init-time))"`
      );
      const uptime = parseFloat(stdout.trim());
      return isNaN(uptime) ? undefined : Math.floor(uptime);
    } catch {
      return undefined;
    }
  }

  private async restartDaemon(): Promise<void> {
    const delay = this.config.restartDelays[this.restartAttempt] || 
                  this.config.restartDelays[this.config.restartDelays.length - 1];

    logger.info(`Attempting to restart Emacs daemon`, { 
      attempt: this.restartAttempt + 1,
      delay 
    });

    this.restartAttempt++;
    this.status.restartCount++;

    await new Promise(resolve => setTimeout(resolve, delay));

    try {
      await this.stopDaemon();
      await this.startDaemon();
    } catch (error) {
      logger.error('Failed to restart daemon', { error });
      
      if (this.restartAttempt >= this.config.maxRestartAttempts) {
        this.emit('daemon-failed', {
          error: 'Max restart attempts reached',
          restartCount: this.status.restartCount
        });
      }
    }
  }

  async stopDaemon(): Promise<void> {
    if (!this.status.isRunning || !this.status.socketName) return;

    logger.info('Stopping Emacs daemon', { socketName: this.status.socketName });

    try {
      // Try graceful shutdown first
      await execAsync(
        `emacsclient --socket-name=${this.status.socketName} --eval "(kill-emacs)"`
      );
    } catch {
      // If graceful shutdown fails, try to kill the process
      if (this.daemonProcess) {
        this.daemonProcess.kill('SIGTERM');
      } else if (this.status.daemonPid) {
        try {
          process.kill(this.status.daemonPid, 'SIGTERM');
        } catch {
          // Process might already be dead
        }
      }
    }

    // Clean up state
    this.status.isRunning = false;
    this.status.daemonPid = undefined;
    this.status.socketPath = undefined;
    this.status.uptime = undefined;
    this.daemonProcess = undefined;

    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
      this.healthCheckTimer = undefined;
    }

    this.emit('daemon-stopped');
  }

  // Get current status with caching
  async getStatus(): Promise<EmacsDaemonStatus> {
    const now = Date.now();
    
    // Return cached status if recent
    if (now - this.lastStatusCheck < this.statusCacheTime) {
      return { ...this.status };
    }

    // Update status if initialized
    if (this.isInitialized && this.status.isRunning) {
      const health = await this.checkHealth();
      this.status.isRunning = health.healthy;
      
      if (health.healthy && this.status.socketName) {
        const uptime = await this.getUptime();
        if (uptime !== undefined) {
          this.status.uptime = uptime;
        }
      }
    }

    this.lastStatusCheck = now;
    this.status.lastHealthCheck = this.lastHealthCheck;

    return { ...this.status };
  }

  // Ensure daemon is running (called by EmacsService)
  async ensureRunning(): Promise<void> {
    await this.ensureInitialized();

    if (!this.status.isAvailable) {
      throw new EmacsDaemonError(
        'Emacs is not installed on this system',
        'NOT_INSTALLED'
      );
    }

    if (!this.status.isRunning) {
      if (this.config.manageDaemon) {
        await this.startDaemon();
      } else {
        throw new EmacsDaemonError(
          'Emacs daemon is not running and auto-start is disabled',
          'START_FAILED'
        );
      }
    }

    // Verify daemon is healthy
    const health = await this.checkHealth();
    if (!health.healthy) {
      throw new EmacsDaemonError(
        'Emacs daemon is not responding',
        'HEALTH_CHECK_FAILED',
        health
      );
    }
  }

  // Get socket name for emacsclient commands
  getSocketName(): string | undefined {
    return this.status.socketName;
  }

  // Clean shutdown
  async shutdown(): Promise<void> {
    logger.info('Shutting down Emacs daemon manager');
    
    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
    }

    if (this.config.manageDaemon && this.status.isRunning) {
      await this.stopDaemon();
    }

    this.removeAllListeners();
  }
}

// Export singleton instance
export const emacsDaemonManager = new EmacsDaemonManager();