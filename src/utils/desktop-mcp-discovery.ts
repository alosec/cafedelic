import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { DesktopMCPLogFile, DesktopMCPDiscoveryResult } from '../types/desktop-mcp.types.js';
import { logger } from './logger.js';
import { configManager } from '../config/cafedelic.config.js';

/**
 * Get the Claude Desktop log directory from config
 */
function getLogBaseDir(): string {
  const config = configManager.getConfig();
  let logDir = config.desktopMCP.logBaseDir;
  
  // Expand tilde if present
  if (logDir.startsWith('~')) {
    logDir = path.join(os.homedir(), logDir.slice(1));
  }
  
  return logDir;
}

// Pattern for MCP server log files in Claude Desktop
const MCP_LOG_PATTERN = /^mcp-server-(.*)\.log$|^mcp\.log$|^mcp\d+\.log$/;

/**
 * Discover active Desktop MCP log files
 */
export async function discoverDesktopMCPLogs(): Promise<DesktopMCPDiscoveryResult> {
  try {
    const LOG_BASE_DIR = getLogBaseDir();
    
    // Check if log directory exists
    if (!fs.existsSync(LOG_BASE_DIR)) {
      return {
        found: false,
        logFiles: [],
        activeCount: 0,
        error: `Claude Desktop log directory not found: ${LOG_BASE_DIR}`
      };
    }

    const logFiles: DesktopMCPLogFile[] = [];
    const config = configManager.getConfig();
    const targetLogs = config.desktopMCP.targetLogs || [];
    
    // Read all files in the Claude logs directory
    const files = fs.readdirSync(LOG_BASE_DIR);
    
    for (const file of files) {
      const match = file.match(MCP_LOG_PATTERN);
      if (match) {
        const filePath = path.join(LOG_BASE_DIR, file);
        
        try {
          const fileStats = fs.statSync(filePath);
          
          // Skip if file is empty or too old
          if (fileStats.size === 0) continue;
          
          const cutoffTime = Date.now() - (config.desktopMCP.maxLogAge * 24 * 60 * 60 * 1000);
          if (fileStats.mtime.getTime() < cutoffTime) continue;
          
          // Determine server name from filename
          let serverName = 'unknown';
          if (file === 'mcp.log') {
            serverName = 'mcp-general';
          } else if (file.match(/^mcp\d+\.log$/)) {
            serverName = 'mcp-rotated';
          } else if (match[1]) {
            serverName = match[1];
          }
          
          // If targetLogs specified, only include those
          if (targetLogs.length > 0 && !targetLogs.includes(file)) {
            continue;
          }
          
          logFiles.push({
            path: filePath,
            windowId: 'claude-desktop', // Claude Desktop doesn't use window concept like VS Code
            serverName,
            lastModified: fileStats.mtime,
            size: fileStats.size
          });
        } catch (statError) {
          logger.warn(`Failed to stat log file ${file}`, { error: statError });
        }
      }
    }
    
    // Sort by last modified, most recent first
    logFiles.sort((a, b) => b.lastModified.getTime() - a.lastModified.getTime());
    
    // Count active logs (modified in last 10 minutes)
    const activeTime = Date.now() - (10 * 60 * 1000);
    const activeCount = logFiles.filter(log => log.lastModified.getTime() > activeTime).length;
    
    logger.info('Desktop MCP log discovery complete', {
      logDirectory: LOG_BASE_DIR,
      found: logFiles.length,
      active: activeCount,
      targetLogs: targetLogs.length > 0 ? targetLogs : 'all'
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
  
  // Priority order: desktop-commander > general mcp > others
  if (activeLog) {
    const desktopCommanderLog = discovery.logFiles.find(log => 
      log.serverName === 'desktop-commander' && log.lastModified.getTime() > activeTime
    );
    if (desktopCommanderLog) return desktopCommanderLog;
  }
  
  return activeLog || discovery.logFiles[0]; // Fallback to most recent
}

/**
 * Check if a log file is still being written to
 */
export function isLogFileActive(logFile: DesktopMCPLogFile, thresholdMinutes: number = 10): boolean {
  const threshold = Date.now() - (thresholdMinutes * 60 * 1000);
  return logFile.lastModified.getTime() > threshold;
}
