// EmacsService - Handles automated emacs file opening
import { EventEmitter } from 'events';
import { exec } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import { configManager } from '../config/cafedelic.config.js';
import { logger } from '../utils/logger.js';
import { outputRouter } from './output-router.service.js';
import { emacsDaemonManager } from './emacs-daemon-manager.service.js';
import { paneEmacsManager } from './pane-emacs-manager.service.js';
import { EmacsDaemonError } from '../types/emacs-daemon.types.js';

const execAsync = promisify(exec);

export interface EmacsOpenResult {
  success: boolean;
  filePath: string;
  message: string;
  bufferCount?: number;
}

export interface EmacsDirectoryResult {
  success: boolean;
  directoryPath: string;
  message: string;
}

export class EmacsService extends EventEmitter {
  private pendingOpens = new Set<string>();
  private openQueue: string[] = [];
  private batchTimer: NodeJS.Timeout | null = null;
  private readonly scriptsPath: string;

  constructor() {
    super();
    const config = configManager.getConfig();
    // Always resolve scripts path relative to the project root
    const projectRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..', '..');
    this.scriptsPath = path.isAbsolute(config.emacs.scriptsPath) 
      ? config.emacs.scriptsPath 
      : path.join(projectRoot, config.emacs.scriptsPath);
    
    logger.info('EmacsService initialized', { 
      scriptsPath: this.scriptsPath,
      configPath: config.emacs.scriptsPath,
      cwd: process.cwd()
    });
  }

  async openFile(filePath: string): Promise<EmacsOpenResult> {
    const config = configManager.getConfig();
    
    // Check if auto-open is enabled
    if (!config.emacs.autoOpen) {
      return {
        success: false,
        filePath,
        message: 'Auto-open is disabled'
      };
    }

    // Check if file type is supported
    if (!configManager.isFileSupported(filePath)) {
      return {
        success: false,
        filePath,
        message: 'File type not supported for auto-open'
      };
    }
    
    // Handle based on mode
    if (config.emacs.mode === 'pane-server') {
      return await this.openFileInPaneServer(filePath);
    }

    // Default daemon mode behavior
    // Ensure daemon is running (lazy initialization)
    try {
      await emacsDaemonManager.ensureRunning();
    } catch (error) {
      if (error instanceof EmacsDaemonError) {
        logger.warn('Emacs daemon not available', { error: error.message });
        return {
          success: false,
          filePath,
          message: `Emacs unavailable: ${error.message}`
        };
      }
      throw error;
    }

    // Avoid duplicate opens
    if (this.pendingOpens.has(filePath)) {
      return {
        success: false,
        filePath,
        message: 'File open already in progress'
      };
    }

    this.pendingOpens.add(filePath);

    try {
      const result = await this.executeFileOpen(filePath);
      this.emit('file-opened', { filePath, success: result.success });
      return result;
    } catch (error) {
      const err = error as Error;
      logger.error('Failed to open file in emacs', { filePath, error: err.message });
      return {
        success: false,
        filePath,
        message: `Error: ${err.message}`
      };
    } finally {
      this.pendingOpens.delete(filePath);
    }
  }

  private async executeFileOpen(filePath: string): Promise<EmacsOpenResult> {
    // Use v2 script that doesn't have hard-coded tmux routing
    const scriptPath = path.join(this.scriptsPath, 'open-claude-file-v2.sh');
    
    // Get socket name from daemon manager (should be available after ensureRunning)
    const socketName = emacsDaemonManager.getSocketName();
    
    // Pass socket info to script via environment
    const env = { ...process.env };
    if (socketName) {
      env.CAFEDELIC_SOCKET_NAME = socketName;
      logger.debug('Using socket for file open', { socketName, filePath });
    } else {
      logger.warn('No socket name available for file open', { filePath });
    }
    
    const command = `bash "${scriptPath}" "${filePath}"`;
    
    try {
      const { stdout, stderr } = await execAsync(command, {
        timeout: configManager.getConfig().emacs.daemonTimeout,
        env
      });
      
      // Parse buffer count from output if available
      const bufferCountMatch = stdout.match(/Total files in context: (\d+)/);
      const bufferCount = bufferCountMatch ? parseInt(bufferCountMatch[1]) : undefined;
      
      logger.info('File opening command executed', {
        filePath,
        bufferCount,
        stdout: stdout.trim(),
        stderr: stderr.trim()
      });
      
      // Route success message to configured pane
      const fileName = path.basename(filePath);
      const message = bufferCount 
        ? `Opened file: ${fileName} (${bufferCount} files in context)`
        : `Opened file: ${fileName}`;
      
      await outputRouter.routeToPane(message, 'editor-output');
      
      return {
        success: true,
        filePath,
        message: 'File opened successfully',
        bufferCount
      };
    } catch (error) {
      const err = error as Error;
      logger.error('File opening command failed', {
        filePath,
        error: err.message,
        command
      });
      
      // Route error message to configured pane
      await outputRouter.routeToPane(
        `Failed to open: ${path.basename(filePath)} - ${err.message}`,
        'editor-output'
      );
      
      throw new Error(`Emacs file open failed: ${err.message}`);
    }
  }

  async openDirectory(directoryPath: string): Promise<EmacsDirectoryResult> {
    const config = configManager.getConfig();
    
    // Check if auto-open directories is enabled
    if (!config.emacs.autoOpenDirectories) {
      return {
        success: false,
        directoryPath,
        message: 'Auto-open directories is disabled'
      };
    }
    
    // Handle based on mode
    if (config.emacs.mode === 'pane-server') {
      return await this.openDirectoryInPaneServer(directoryPath);
    }

    // Default daemon mode behavior
    // Ensure daemon is running (lazy initialization)
    try {
      await emacsDaemonManager.ensureRunning();
    } catch (error) {
      if (error instanceof EmacsDaemonError) {
        logger.warn('Emacs daemon not available', { error: error.message });
        return {
          success: false,
          directoryPath,
          message: `Emacs unavailable: ${error.message}`
        };
      }
      throw error;
    }

    // Avoid duplicate opens
    if (this.pendingOpens.has(directoryPath)) {
      return {
        success: false,
        directoryPath,
        message: 'Directory open already in progress'
      };
    }

    this.pendingOpens.add(directoryPath);

    try {
      const result = await this.executeDirectoryOpen(directoryPath);
      this.emit('directory-opened', { directoryPath, success: result.success });
      return result;
    } catch (error) {
      const err = error as Error;
      logger.error('Failed to open directory in emacs', { directoryPath, error: err.message });
      return {
        success: false,
        directoryPath,
        message: `Error: ${err.message}`
      };
    } finally {
      this.pendingOpens.delete(directoryPath);
    }
  }

  private async executeDirectoryOpen(directoryPath: string): Promise<EmacsDirectoryResult> {
    // Use v2 script that doesn't have hard-coded tmux routing
    const scriptPath = path.join(this.scriptsPath, 'open-dired-v2.sh');
    
    // Get socket name from daemon manager (should be available after ensureRunning)
    const socketName = emacsDaemonManager.getSocketName();
    
    // Pass socket info to script via environment
    const env = { ...process.env };
    if (socketName) {
      env.CAFEDELIC_SOCKET_NAME = socketName;
      logger.debug('Using socket for directory open', { socketName, directoryPath });
    } else {
      logger.warn('No socket name available for directory open', { directoryPath });
    }
    
    const command = `bash "${scriptPath}" "${directoryPath}"`;
    
    try {
      const { stdout, stderr } = await execAsync(command, {
        timeout: configManager.getConfig().emacs.daemonTimeout,
        env
      });
      
      logger.info('Directory opening command executed', {
        directoryPath,
        stdout: stdout.trim(),
        stderr: stderr.trim()
      });
      
      // Route success message to configured pane
      await outputRouter.routeToPane(
        `Opening dired: ${directoryPath}`,
        'editor-output'
      );
      
      return {
        success: true,
        directoryPath,
        message: 'Directory opened successfully in dired'
      };
    } catch (error) {
      const err = error as Error;
      logger.error('Directory opening command failed', {
        directoryPath,
        error: err.message,
        command
      });
      
      // Route error message to configured pane
      await outputRouter.routeToPane(
        `Failed to open directory: ${directoryPath} - ${err.message}`,
        'editor-output'
      );
      
      throw new Error(`Emacs directory open failed: ${err.message}`);
    }
  }

  async checkEmacsHealth(): Promise<{ isRunning: boolean; message: string }> {
    const config = configManager.getConfig();
    
    if (config.emacs.mode === 'pane-server') {
      // Check pane server status
      const servers = await paneEmacsManager.getServerStatus();
      const defaultPane = config.emacs.paneServers?.defaultPane || '9:0.2';
      const server = servers.get(defaultPane);
      
      if (server && server.status === 'ready') {
        return {
          isRunning: true,
          message: `Pane server running: ${server.serverName} in pane ${server.paneId}`
        };
      } else {
        return {
          isRunning: false,
          message: 'No pane server running'
        };
      }
    }
    
    // Default daemon mode
    const status = await emacsDaemonManager.getStatus();
    
    return {
      isRunning: status.isRunning,
      message: status.isRunning 
        ? `Emacs daemon is running (PID: ${status.daemonPid}, Socket: ${status.socketName})`
        : status.lastError || 'Emacs daemon is not running'
    };
  }

  // Pane server methods
  private async openFileInPaneServer(filePath: string): Promise<EmacsOpenResult> {
    const config = configManager.getConfig();
    const paneConfig = config.emacs.paneServers || { defaultPane: '9:0.2', mapping: {}, autoStart: true };
    
    // Determine which pane to use
    const targetPane = paneConfig.defaultPane;
    
    try {
      await paneEmacsManager.openFileInPane(targetPane, filePath);
      
      // Route success message
      const fileName = path.basename(filePath);
      await outputRouter.routeToPane(
        `Opened file: ${fileName} in pane ${targetPane}`,
        'editor-output'
      );
      
      return {
        success: true,
        filePath,
        message: `File opened in pane ${targetPane}`
      };
    } catch (error) {
      const err = error as Error;
      logger.error('Failed to open file in pane server', { filePath, error: err.message });
      
      // Route error message
      await outputRouter.routeToPane(
        `Failed to open: ${path.basename(filePath)} - ${err.message}`,
        'editor-output'
      );
      
      return {
        success: false,
        filePath,
        message: `Error: ${err.message}`
      };
    }
  }
  
  private async openDirectoryInPaneServer(directoryPath: string): Promise<EmacsDirectoryResult> {
    const config = configManager.getConfig();
    const paneConfig = config.emacs.paneServers || { defaultPane: '9:0.2', mapping: {}, autoStart: true };
    
    // Determine which pane to use
    const targetPane = paneConfig.defaultPane;
    
    try {
      await paneEmacsManager.openDirectoryInPane(targetPane, directoryPath);
      
      // Route success message
      await outputRouter.routeToPane(
        `Opened directory: ${directoryPath} in pane ${targetPane}`,
        'editor-output'
      );
      
      return {
        success: true,
        directoryPath,
        message: `Directory opened in pane ${targetPane}`
      };
    } catch (error) {
      const err = error as Error;
      logger.error('Failed to open directory in pane server', { directoryPath, error: err.message });
      
      // Route error message
      await outputRouter.routeToPane(
        `Failed to open directory: ${directoryPath} - ${err.message}`,
        'editor-output'
      );
      
      return {
        success: false,
        directoryPath,
        message: `Error: ${err.message}`
      };
    }
  }

  async batchOpenFiles(filePaths: string[]): Promise<EmacsOpenResult[]> {
    const results: EmacsOpenResult[] = [];
    const config = configManager.getConfig();
    
    // Process files in batches to avoid overwhelming the system
    for (let i = 0; i < filePaths.length; i += config.performance.maxConcurrentOpens) {
      const batch = filePaths.slice(i, i + config.performance.maxConcurrentOpens);
      const batchPromises = batch.map(filePath => this.openFile(filePath));
      const batchResults = await Promise.all(batchPromises);
      results.push(...batchResults);
      
      // Add delay between batches
      if (i + config.performance.maxConcurrentOpens < filePaths.length) {
        await new Promise(resolve => setTimeout(resolve, config.performance.batchDelay));
      }
    }
    
    return results;
  }

  getPendingOpens(): string[] {
    return Array.from(this.pendingOpens);
  }
}

export const emacsService = new EmacsService();
