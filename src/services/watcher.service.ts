import { EventEmitter } from 'events';
import * as fs from 'fs';
import * as path from 'path';
import { logger } from '../utils/logger.js';

export interface DCLogEntry {
  timestamp: string;
  command: string;
  args: any;
}

export class WatcherService extends EventEmitter {
  private lastPosition = 0;
  private logPath: string;
  private watchInterval?: NodeJS.Timeout;

  constructor(logPath?: string) {
    super();
    this.logPath = logPath || path.join(
      process.env.HOME!,
      '.claude-server-commander',
      'claude_tool_call.log'
    );
  }

  async start() {
    logger.info('Starting DC log watcher', { path: this.logPath });
    
    // Check if log file exists
    if (!fs.existsSync(this.logPath)) {
      logger.info('DC log file not found', { path: this.logPath, warning: true });
      return;
    }

    // Initialize position to end of file
    const stats = fs.statSync(this.logPath);
    this.lastPosition = stats.size;
    
    // Start watching with polling (simple approach)
    this.watchInterval = setInterval(() => {
      this.checkForNewEntries();
    }, 500); // Check every 500ms
  }

  stop() {
    if (this.watchInterval) {
      clearInterval(this.watchInterval);
      this.watchInterval = undefined;
    }
  }

  private checkForNewEntries() {
    try {
      const stats = fs.statSync(this.logPath);
      
      if (stats.size > this.lastPosition) {
        // Read new content
        const stream = fs.createReadStream(this.logPath, {
          start: this.lastPosition,
          end: stats.size
        });
        
        let buffer = '';
        
        stream.on('data', (chunk) => {
          buffer += chunk.toString();
        });
        
        stream.on('end', () => {
          this.lastPosition = stats.size;
          this.processBuffer(buffer);
        });
      }
    } catch (error) {
      logger.error('Error checking log file', { error });
    }
  }

  private processBuffer(buffer: string) {
    const lines = buffer.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      try {
        // Parse pipe-delimited format: TIMESTAMP | COMMAND | Arguments: JSON
        const parts = line.split('|').map(p => p.trim());
        if (parts.length >= 3) {
          const timestamp = parts[0];
          const command = parts[1];
          const argsString = parts[2].replace('Arguments: ', '');
          
          const entry: DCLogEntry = {
            timestamp,
            command,
            args: JSON.parse(argsString)
          };
          
          this.emit('log-entry', entry);
        }
      } catch (error) {
        logger.info('Failed to parse log line', { line, error, warning: true });
      }
    }
  }
}
