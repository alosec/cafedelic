import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { DesktopMCPLogFile, DesktopMCPDiscoveryResult } from '../types/desktop-mcp.types.js';
import { logger } from './logger.js';

// Base directory for VS Code logs
const LOG_BASE_DIR = path.join(os.homedir(), '.config', 'Code', 'logs');

// Pattern for MCP server log files
const MCP_LOG_PATTERN = /^mcpServer\.claude-desktop\.null\.(.*)\.log$/;

/**
 * Discover active Desktop MCP log files
 */
export async function discoverDesktopMCPLogs(): Promise<DesktopMCPDiscoveryResult> {
  try {
    // Check if log directory exists
    if (!fs.existsSync(LOG_BASE_DIR)) {
      return {
        found: false,
        logFiles: [],
        activeCount: 0,
        error: 'VS Code log directory not found'
      };
    }

    const logFiles: DesktopMCPLogFile[] = [];
    
    // Read date directories (e.g., 2025-05-26T08_20_05)
    const dateDirs = fs.readdirSync(LOG_BASE_DIR)
      .filter(dir => dir.match(/^\d{4}-\d{2}-\d{2}T/))
      .sort()
      .reverse(); // Most recent first
    
    // Only check recent directories (last 2 days)
    const cutoffTime = Date.now() - (2 * 24 * 60 * 60 * 1000);
    
    for (const dateDir of dateDirs) {
      const dateDirPath = path.join(LOG_BASE_DIR, dateDir);
      const stats = fs.statSync(dateDirPath);
      
      if (stats.mtime.getTime() < cutoffTime) {
        continue; // Skip old directories
      }
      
      // Look for window directories
      const windowDirs = fs.readdirSync(dateDirPath)
        .filter(dir => dir.startsWith('window'));
      
      for (const windowDir of windowDirs) {
        const windowPath = path.join(dateDirPath, windowDir);
        
        // Find MCP server logs
        const files = fs.readdirSync(windowPath);
        
        for (const file of files) {
          const match = file.match(MCP_LOG_PATTERN);
          if (match) {
            const filePath = path.join(windowPath, file);
            const fileStats = fs.statSync(filePath);
            
            logFiles.push({
              path: filePath,
              windowId: windowDir,
              serverName: match[1],
              lastModified: fileStats.mtime,
              size: fileStats.size
            });
          }
        }
      }
    }
    
    // Sort by last modified, most recent first
    logFiles.sort((a, b) => b.lastModified.getTime() - a.lastModified.getTime());
    
    // Count active logs (modified in last 10 minutes)
    const activeTime = Date.now() - (10 * 60 * 1000);
    const activeCount = logFiles.filter(log => log.lastModified.getTime() > activeTime).length;
    
    logger.info('Desktop MCP log discovery complete', {
      found: logFiles.length,
      active: activeCount
    });
    
    return {
      found: logFiles.length > 0,
      logFiles,
      activeCount
    };
    
  } catch (error) {
    logger.error('Failed to discover Desktop MCP logs', { error });
    return {
      found: false,
      logFiles: [],
      activeCount: 0,
      error: (error as Error).message
    };
  }
}

/**
 * Find the most recently active Desktop MCP log file
 */
export async function findActiveDesktopMCPLog(): Promise<DesktopMCPLogFile | null> {
  const discovery = await discoverDesktopMCPLogs();
  
  if (!discovery.found || discovery.logFiles.length === 0) {
    return null;
  }
  
  // Return the most recently modified log that's still active
  const activeTime = Date.now() - (10 * 60 * 1000);
  const activeLog = discovery.logFiles.find(log => log.lastModified.getTime() > activeTime);
  
  return activeLog || discovery.logFiles[0]; // Fallback to most recent
}

/**
 * Check if a log file is still being written to
 */
export function isLogFileActive(logFile: DesktopMCPLogFile, thresholdMinutes: number = 10): boolean {
  const threshold = Date.now() - (thresholdMinutes * 60 * 1000);
  return logFile.lastModified.getTime() > threshold;
}
