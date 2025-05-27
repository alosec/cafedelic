// pane-emacs-manager.service.ts - Manage pane-specific emacs servers

import { EventEmitter } from 'events';
import { exec } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import { logger } from '../utils/logger.js';
import { paneServerMapper } from '../utils/pane-server-mapping.js';
import { configManager } from '../config/cafedelic.config.js';

const execAsync = promisify(exec);

export interface PaneServer {
  paneId: string;          // e.g., "9:0.2"
  serverName: string;      // e.g., "pane-9-0-2"
  status: 'starting' | 'ready' | 'error';
  startTime: Date;
  lastCheck?: Date;
  error?: string;
}

export interface PaneServerOptions {
  suffix?: string;         // Optional suffix for server name
  initFile?: string;       // Custom init file
  config?: string;         // Additional config to load
  autoFocus?: boolean;     // Focus pane after operations
}

export class PaneEmacsManager extends EventEmitter {
  private servers: Map<string, PaneServer> = new Map();
  private scriptsPath: string;
  private checkInterval: number = 30000; // 30 seconds
  private checkTimer?: NodeJS.Timeout;
  
  constructor() {
    super();
    
    // Get scripts path from config
    const config = configManager.getConfig();
    const projectRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..', '..');
    this.scriptsPath = path.isAbsolute(config.emacs.scriptsPath) 
      ? config.emacs.scriptsPath 
      : path.join(projectRoot, config.emacs.scriptsPath);
    
    logger.info('PaneEmacsManager initialized', { scriptsPath: this.scriptsPath });
  }

  
  /**
   * Start a pane-specific emacs server
   */
  async startPaneServer(paneId: string, options: PaneServerOptions = {}): Promise<PaneServer> {
    logger.info('Starting pane server', { paneId, options });
    
    // Check if we already have this server
    const existing = this.servers.get(paneId);
    if (existing && existing.status === 'ready') {
      logger.debug('Server already running for pane', { paneId });
      return existing;
    }
    
    // Generate server name
    const serverName = paneServerMapper.generateServerName(paneId, options.suffix);
    
    // Create server entry
    const server: PaneServer = {
      paneId,
      serverName,
      status: 'starting',
      startTime: new Date()
    };
    
    this.servers.set(paneId, server);
    
    try {
      // Execute start script
      const scriptPath = path.join(this.scriptsPath, 'pane-server', 'start-pane-emacs.sh');
      const args = [paneId];
      if (options.suffix) {
        args.push(options.suffix);
      }
      
      const command = `bash "${scriptPath}" ${args.map(a => `"${a}"`).join(' ')}`;
      logger.debug('Executing start command', { command });
      
      // Ensure XDG_RUNTIME_DIR is set
      const env = {
        ...process.env,
        XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR || `/run/user/${process.getuid ? process.getuid() : 1000}`
      };
      
      const { stdout, stderr } = await execAsync(command, {
        timeout: 15000, // 15 second timeout for startup
        env
      });
      
      if (stderr && !stderr.includes('Warning')) {
        logger.warn('Server start had warnings', { paneId, stderr });
      }
      
      // Store mapping
      await paneServerMapper.setServerForPane(paneId, serverName);
      
      // Verify server is running
      const isRunning = await this.verifyServer(serverName);
      
      if (isRunning) {
        server.status = 'ready';
        this.emit('server-started', { paneId, serverName });
        logger.info('Pane server started successfully', { paneId, serverName });
      } else {
        throw new Error('Server failed to start within timeout');
      }
      
    } catch (error) {
      server.status = 'error';
      server.error = error instanceof Error ? error.message : 'Unknown error';
      logger.error('Failed to start pane server', { paneId, error });
      throw error;
    }
    
    return server;
  }

  
  /**
   * Open a file in a pane's emacs server
   */
  async openFileInPane(paneId: string, filePath: string, options: PaneServerOptions = {}): Promise<void> {
    logger.info('Opening file in pane', { paneId, filePath });
    
    // Ensure server is running
    const server = await this.getOrCreateServer(paneId, options);
    
    try {
      const scriptPath = path.join(this.scriptsPath, 'pane-server', 'open-file-in-pane.sh');
      const command = `bash "${scriptPath}" "${filePath}" "${paneId}"`;
      
      // Ensure XDG_RUNTIME_DIR is set
      const env = {
        ...process.env,
        XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR || `/run/user/${process.getuid ? process.getuid() : 1000}`
      };
      
      const { stdout, stderr } = await execAsync(command, {
        timeout: 5000,
        env
      });
      
      if (stderr && !stderr.includes('Files in context')) {
        logger.warn('File open had warnings', { paneId, filePath, stderr });
      }
      
      this.emit('file-opened', { paneId, filePath, serverName: server.serverName });
      logger.info('File opened successfully', { paneId, filePath });
      
    } catch (error) {
      logger.error('Failed to open file in pane', { paneId, filePath, error });
      throw error;
    }
  }
  
  /**
   * Open a directory in a pane's emacs server
   */
  async openDirectoryInPane(paneId: string, dirPath: string, options: PaneServerOptions = {}): Promise<void> {
    logger.info('Opening directory in pane', { paneId, dirPath });
    
    // Ensure server is running
    const server = await this.getOrCreateServer(paneId, options);
    
    try {
      const scriptPath = path.join(this.scriptsPath, 'pane-server', 'open-directory-in-pane.sh');
      const command = `bash "${scriptPath}" "${dirPath}" "${paneId}"`;
      
      const { stdout, stderr } = await execAsync(command, {
        timeout: 5000
      });
      
      if (stderr) {
        logger.warn('Directory open had warnings', { paneId, dirPath, stderr });
      }
      
      this.emit('directory-opened', { paneId, dirPath, serverName: server.serverName });
      logger.info('Directory opened successfully', { paneId, dirPath });
      
    } catch (error) {
      logger.error('Failed to open directory in pane', { paneId, dirPath, error });
      throw error;
    }
  }

  
  /**
   * Get or create server for a pane
   */
  private async getOrCreateServer(paneId: string, options: PaneServerOptions = {}): Promise<PaneServer> {
    // Check if we have a running server
    const existing = this.servers.get(paneId);
    if (existing && existing.status === 'ready') {
      return existing;
    }
    
    // Check if there's a server name stored
    const storedServerName = await paneServerMapper.getServerForPane(paneId);
    if (storedServerName) {
      // Verify it's still running
      const isRunning = await this.verifyServer(storedServerName);
      if (isRunning) {
        const server: PaneServer = {
          paneId,
          serverName: storedServerName,
          status: 'ready',
          startTime: new Date()
        };
        this.servers.set(paneId, server);
        return server;
      }
    }
    
    // Start new server
    return await this.startPaneServer(paneId, options);
  }
  
  /**
   * Verify a server is running
   */
  private async verifyServer(serverName: string): Promise<boolean> {
    try {
      // Ensure XDG_RUNTIME_DIR is set
      const env = {
        ...process.env,
        XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR || `/run/user/${process.getuid ? process.getuid() : 1000}`
      };
      
      const { stdout } = await execAsync(
        `emacsclient --socket-name="${serverName}" --eval "t"`,
        { timeout: 2000, env }
      );
      return stdout.trim() === 't';
    } catch {
      return false;
    }
  }
  
  /**
   * Get status of all servers
   */
  async getServerStatus(): Promise<Map<string, PaneServer>> {
    // Update status for all servers
    for (const [paneId, server] of this.servers) {
      if (server.status === 'ready') {
        const isRunning = await this.verifyServer(server.serverName);
        if (!isRunning) {
          server.status = 'error';
          server.error = 'Server no longer running';
        }
      }
      server.lastCheck = new Date();
    }
    
    return new Map(this.servers);
  }
  
  /**
   * Stop a pane server
   */
  async stopPaneServer(paneId: string): Promise<void> {
    const server = this.servers.get(paneId);
    if (!server) {
      return;
    }
    
    try {
      // Send kill command to server
      await execAsync(
        `emacsclient --socket-name="${server.serverName}" --eval "(kill-emacs)"`,
        { timeout: 2000 }
      );
    } catch {
      // Server might already be dead
    }
    
    // Remove from tracking
    this.servers.delete(paneId);
    await paneServerMapper.removeMapping(paneId);
    
    this.emit('server-stopped', { paneId, serverName: server.serverName });
  }
  
  /**
   * Start health monitoring
   */
  startHealthMonitoring(): void {
    if (this.checkTimer) {
      return;
    }
    
    this.checkTimer = setInterval(async () => {
      await this.getServerStatus();
    }, this.checkInterval);
  }
  
  /**
   * Stop health monitoring
   */
  stopHealthMonitoring(): void {
    if (this.checkTimer) {
      clearInterval(this.checkTimer);
      this.checkTimer = undefined;
    }
  }
  
  /**
   * Shutdown all servers
   */
  async shutdown(): Promise<void> {
    this.stopHealthMonitoring();
    
    // Stop all servers
    const stopPromises = Array.from(this.servers.keys()).map(paneId => 
      this.stopPaneServer(paneId).catch(err => 
        logger.error('Failed to stop server', { paneId, error: err })
      )
    );
    
    await Promise.all(stopPromises);
    this.removeAllListeners();
  }
}

// Export singleton instance
export const paneEmacsManager = new PaneEmacsManager();
