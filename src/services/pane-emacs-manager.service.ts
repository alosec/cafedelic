/**
 * Pane Emacs Manager Service
 * Manage pane-specific emacs servers
 */

import { EventEmitter } from 'events';
import { exec } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import { logger } from '../utils/logger.js';

const execAsync = promisify(exec);

interface ServerInfo {
  paneId: string;
  serverName: string;
  status: 'starting' | 'ready' | 'error';
  startTime: Date;
  error?: string;
}

interface StartOptions {
  suffix?: string;
}

export class PaneEmacsManager extends EventEmitter {
  private servers = new Map<string, ServerInfo>();
  private scriptsPath: string;

  constructor() {
    super();
    // Use project root scripts directory
    const projectRoot = process.cwd();
    this.scriptsPath = path.join(projectRoot, 'scripts');
    logger.info('PaneEmacsManager initialized', { scriptsPath: this.scriptsPath });
  }

  /**
   * Start a pane-specific emacs server
   */
  async startPaneServer(paneId: string, options: StartOptions = {}): Promise<ServerInfo> {
    logger.info('Starting pane server', { paneId, options });
    
    // Check if we already have this server
    const existing = this.servers.get(paneId);
    if (existing && existing.status === 'ready') {
      logger.debug('Server already running for pane', { paneId });
      return existing;
    }

    // Generate server name
    const serverName = this.generateServerName(paneId, options.suffix);
    
    // Create server entry
    const server: ServerInfo = {
      paneId,
      serverName,
      status: 'starting',
      startTime: new Date()
    };
    
    this.servers.set(paneId, server);

    try {
      // Execute start script
      const scriptPath = path.join(this.scriptsPath, 'emacs/pane-server', 'start-pane-emacs.sh');
      const args = [paneId];
      if (options.suffix) {
        args.push(options.suffix);
      }

      const command = `bash "${scriptPath}" ${args.map(a => `"${a}"`).join(' ')}`;
      logger.debug('Executing start command', { command });

      // Ensure XDG_RUNTIME_DIR is set
      const env = {
        ...process.env,
        XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR || `/run/user/${process.getuid?.() || 1000}`
      };

      const { stdout, stderr } = await execAsync(command, {
        timeout: 15000, // 15 second timeout for startup
        env
      });

      if (stderr && !stderr.includes('Warning')) {
        logger.warn('Server start had warnings', { paneId, stderr });
      }

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
   * Get or create server for a pane
   */
  async getOrCreateServer(paneId: string, options: StartOptions = {}): Promise<ServerInfo> {
    // Check if we have a running server
    const existing = this.servers.get(paneId);
    if (existing && existing.status === 'ready') {
      return existing;
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
        XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR || `/run/user/${process.getuid?.() || 1000}`
      };
      
      const { stdout } = await execAsync(`emacsclient --socket-name="${serverName}" --eval "t"`, { 
        timeout: 2000, 
        env 
      });
      return stdout.trim() === 't';
    } catch {
      return false;
    }
  }

  /**
   * Generate server name for a pane
   */
  private generateServerName(paneId: string, suffix?: string): string {
    const base = `pane-${paneId.replace(/[:.]/g, '-')}`;
    return suffix ? `${base}-${suffix}` : base;
  }

  /**
   * Get status of all servers
   */
  async getServerStatus(): Promise<Map<string, ServerInfo>> {
    // Update status for all servers
    for (const [paneId, server] of this.servers) {
      if (server.status === 'ready') {
        const isRunning = await this.verifyServer(server.serverName);
        if (!isRunning) {
          server.status = 'error';
          server.error = 'Server no longer running';
        }
      }
    }
    return new Map(this.servers);
  }

  /**
   * Shutdown all servers
   */
  async shutdown(): Promise<void> {
    // Stop all servers
    const stopPromises = Array.from(this.servers.keys()).map(paneId => 
      this.stopPaneServer(paneId).catch(err => 
        logger.error('Failed to stop server', { paneId, error: err })
      )
    );
    
    await Promise.all(stopPromises);
    this.removeAllListeners();
  }

  /**
   * Stop a pane server
   */
  private async stopPaneServer(paneId: string): Promise<void> {
    const server = this.servers.get(paneId);
    if (!server) {
      return;
    }

    try {
      // Send kill command to server
      await execAsync(`emacsclient --socket-name="${server.serverName}" --eval "(kill-emacs)"`, { 
        timeout: 2000 
      });
    } catch {
      // Server might already be dead
    }

    // Remove from tracking
    this.servers.delete(paneId);
    this.emit('server-stopped', { paneId, serverName: server.serverName });
  }
}

// Export singleton instance
export const paneEmacsManager = new PaneEmacsManager();