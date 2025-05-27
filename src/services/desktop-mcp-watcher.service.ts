import { EventEmitter } from 'events';
import * as fs from 'fs';
import { execFile } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import { 
  DesktopMCPLogEntry, 
  DesktopMCPCompatibleEntry,
  DesktopMCPParseResult 
} from '../types/desktop-mcp.types.js';
import { findActiveDesktopMCPLog, discoverDesktopMCPLogs } from '../utils/desktop-mcp-discovery.js';
import { logger } from '../utils/logger.js';

export class DesktopMCPWatcherService extends EventEmitter {
  private lastPositions = new Map<string, number>();
  private watchIntervals = new Map<string, NodeJS.Timeout>();
  private checkInterval = 500; // Check every 500ms
  private discoveryInterval = 30000; // Rediscover logs every 30 seconds
  private discoveryTimer?: NodeJS.Timeout;
  private isRunning = false;
  
  // Emacs integration configuration
  private targetPane = '0:0.0'; // Pane 000 as specified by user
  private scriptsPath = '/home/alex/code/cafedelic/scripts/emacs/pane-server';
  private execFileAsync = promisify(execFile);

  async start() {
    if (this.isRunning) return;
    
    logger.info('Starting Desktop MCP log watcher');
    this.isRunning = true;
    
    // Initial discovery
    await this.discoverAndWatch();
    
    // Periodic rediscovery for new windows/sessions
    this.discoveryTimer = setInterval(() => {
      this.discoverAndWatch();
    }, this.discoveryInterval);
  }

  stop() {
    if (!this.isRunning) return;
    
    logger.info('Stopping Desktop MCP log watcher');
    this.isRunning = false;
    
    // Clear all watch intervals
    for (const [path, interval] of this.watchIntervals) {
      clearInterval(interval);
    }
    this.watchIntervals.clear();
    this.lastPositions.clear();
    
    // Clear discovery timer
    if (this.discoveryTimer) {
      clearInterval(this.discoveryTimer);
      this.discoveryTimer = undefined;
    }
  }

  // Emacs integration methods
  private async triggerEmacsFileOpen(filePath: string): Promise<void> {
    try {
      const scriptPath = path.join(this.scriptsPath, 'open-file-in-pane.sh');
      logger.info('Triggering emacs file open', { file: filePath, pane: this.targetPane });
      
      await this.execFileAsync(scriptPath, [filePath, this.targetPane]);
      logger.info('Successfully opened file in emacs', { file: filePath });
    } catch (error) {
      logger.warn('Failed to open file in emacs', { file: filePath, error: (error as Error).message });
    }
  }

  private async triggerEmacsDirectoryOpen(dirPath: string): Promise<void> {
    try {
      const scriptPath = path.join(this.scriptsPath, 'open-directory-in-pane.sh');
      logger.info('Triggering emacs directory open', { directory: dirPath, pane: this.targetPane });
      
      await this.execFileAsync(scriptPath, [dirPath, this.targetPane]);
      logger.info('Successfully opened directory in emacs dired', { directory: dirPath });
    } catch (error) {
      logger.warn('Failed to open directory in emacs', { directory: dirPath, error: (error as Error).message });
    }
  }

  private shouldTriggerEmacsOpen(command: string, args: any): { shouldOpen: boolean; path?: string; isDirectory?: boolean } {
    // Only trigger for read_file and list_directory operations
    if (command === 'read_file' && args?.path) {
      const filePath = args.path;
      // Skip if it's a log file or temporary file
      if (filePath.includes('/logs/') || filePath.endsWith('.log') || filePath.includes('/tmp/')) {
        return { shouldOpen: false };
      }
      return { shouldOpen: true, path: filePath, isDirectory: false };
    }
    
    if (command === 'list_directory' && args?.path) {
      const dirPath = args.path;
      // Skip common system directories
      if (dirPath.includes('/proc/') || dirPath.includes('/sys/') || dirPath === '/') {
        return { shouldOpen: false };
      }
      return { shouldOpen: true, path: dirPath, isDirectory: true };
    }
    
    return { shouldOpen: false };
  }

  private async discoverAndWatch() {
    try {
      const discovery = await discoverDesktopMCPLogs();
      
      if (!discovery.found) {
        logger.warn('No Desktop MCP logs found');
        return;
      }
      
      logger.info('Discovered Desktop MCP logs', { 
        count: discovery.logFiles.length,
        active: discovery.activeCount 
      });
      
      // Start watching new log files
      for (const logFile of discovery.logFiles) {
        if (!this.watchIntervals.has(logFile.path)) {
          this.startWatchingLog(logFile.path);
        }
      }
      
      // Stop watching stale logs
      const currentPaths = new Set(discovery.logFiles.map(f => f.path));
      for (const [path, interval] of this.watchIntervals) {
        if (!currentPaths.has(path)) {
          logger.info('Stopping watch on stale log', { path });
          clearInterval(interval);
          this.watchIntervals.delete(path);
          this.lastPositions.delete(path);
        }
      }
      
    } catch (error) {
      logger.error('Failed to discover Desktop MCP logs', { error });
    }
  }

  private startWatchingLog(logPath: string) {
    logger.info('Starting watch on Desktop MCP log', { path: logPath });
    
    // Initialize position to end of file
    try {
      const stats = fs.statSync(logPath);
      this.lastPositions.set(logPath, stats.size);
    } catch (error) {
      logger.error('Failed to stat log file', { path: logPath, error });
      return;
    }
    
    // Start watching with polling
    const interval = setInterval(() => {
      this.checkForNewEntries(logPath);
    }, this.checkInterval);
    
    this.watchIntervals.set(logPath, interval);
  }

  private checkForNewEntries(logPath: string) {
    try {
      const stats = fs.statSync(logPath);
      const lastPosition = this.lastPositions.get(logPath) || 0;
      
      if (stats.size > lastPosition) {
        // Read new content
        const stream = fs.createReadStream(logPath, {
          start: lastPosition,
          end: stats.size
        });
        
        let buffer = '';
        
        stream.on('data', (chunk) => {
          buffer += chunk.toString();
        });
        
        stream.on('end', () => {
          this.lastPositions.set(logPath, stats.size);
          this.processBuffer(buffer);
        });
      }
    } catch (error) {
      // File might have been deleted or rotated
      if ((error as any).code === 'ENOENT') {
        logger.info('Log file no longer exists', { path: logPath });
        const interval = this.watchIntervals.get(logPath);
        if (interval) {
          clearInterval(interval);
          this.watchIntervals.delete(logPath);
          this.lastPositions.delete(logPath);
        }
      } else {
        logger.error('Error checking log file', { path: logPath, error });
      }
    }
  }

  private processBuffer(buffer: string) {
    const lines = buffer.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      const parseResult = this.parseLogLine(line);
      
      if (parseResult.success && parseResult.entry) {
        const entry = parseResult.entry;
        
        // Only process tool calls
        if (entry.toolCall) {
          // Convert to compatible format for existing system
          const compatibleEntry: DesktopMCPCompatibleEntry = {
            timestamp: entry.timestamp,
            command: entry.toolCall.name,
            args: entry.toolCall.args
          };
          
          // Emit as DCLogEntry for compatibility
          this.emit('log-entry', compatibleEntry);
          
          // Also emit rich MCP entry for future use
          this.emit('mcp-entry', entry);
          
          // 🎯 NEW: Trigger emacs integration for file operations
          const emacsAction = this.shouldTriggerEmacsOpen(entry.toolCall.name, entry.toolCall.args);
          if (emacsAction.shouldOpen && emacsAction.path) {
            if (emacsAction.isDirectory) {
              // Trigger directory opening in dired (async, don't wait)
              this.triggerEmacsDirectoryOpen(emacsAction.path).catch(err => {
                logger.debug('Emacs directory open failed', { error: err.message });
              });
            } else {
              // Trigger file opening (async, don't wait)
              this.triggerEmacsFileOpen(emacsAction.path).catch(err => {
                logger.debug('Emacs file open failed', { error: err.message });
              });
            }
          }
        }
      }
    }
  }

  private parseLogLine(line: string): DesktopMCPParseResult {
    try {
      // Parse actual Claude Desktop format: "2025-05-27T16:39:53.398Z [desktop-commander] [info] Message from client: {...}"
      const match = line.match(/^(\d{4}-\d{2}-\d{2}T[\d:\.]+Z) \[([^\]]+)\] \[(\w+)\] (.+)$/);
      
      if (!match) {
        return { success: false, error: 'Invalid log format' };
      }
      
      const [, timestamp, serverName, level, message] = match;
      const entry: DesktopMCPLogEntry = {
        timestamp,
        level: level as any,
        message,
        serverName
      };
      
      // Extract tool call information
      entry.toolCall = this.extractToolCall(message);
      
      return { success: true, entry };
      
    } catch (error) {
      return { 
        success: false, 
        error: `Parse error: ${(error as Error).message}` 
      };
    }
  }

  private extractToolCall(message: string): import('../types/desktop-mcp.types.js').DesktopMCPToolCall | undefined {
    // Pattern 1: "Tool execution: tool_name with args: {...}"
    const execMatch = message.match(/^Tool execution: (\w+) with args: (.+)$/);
    if (execMatch) {
      try {
        return {
          name: execMatch[1],
          args: JSON.parse(execMatch[2]),
          status: 'started'
        };
      } catch (error) {
        logger.debug('Failed to parse tool args', { message, error });
      }
    }
    
    // Pattern 2: "Tool completed: tool_name (success|failed)"
    const completedMatch = message.match(/^Tool completed: (\w+) \((success|failed)\)$/);
    if (completedMatch) {
      return {
        name: completedMatch[1],
        args: {},
        status: completedMatch[2] === 'success' ? 'completed' : 'failed'
      };
    }
    
    // Pattern 3: Claude Desktop client request format
    // "Message from client: {"method":"tools/call","params":{"name":"read_file","arguments":{...}}}"
    const clientMatch = message.match(/^Message from client: (.+)$/);
    if (clientMatch) {
      try {
        const requestData = JSON.parse(clientMatch[1]);
        if (requestData.method === 'tools/call' && requestData.params?.name) {
          return {
            name: requestData.params.name,
            args: requestData.params.arguments || {},
            status: 'started'
          };
        }
      } catch (error) {
        logger.debug('Failed to parse client message', { message, error });
      }
    }
    
    // Pattern 4: Legacy format for backward compatibility
    // "Received request: method=tools/call, params={\"name\":\"read_file\",...}"
    const requestMatch = message.match(/^Received request: method=tools\/call, params=(.+)$/);
    if (requestMatch) {
      try {
        const params = JSON.parse(requestMatch[1]);
        if (params.name) {
          return {
            name: params.name,
            args: params.arguments || {},
            status: 'started'
          };
        }
      } catch (error) {
        logger.debug('Failed to parse request params', { message, error });
      }
    }
    
    return undefined;
  }
}
