// Socket Discovery Utilities - Platform-aware Emacs socket detection

import * as os from 'os';
import * as path from 'path';
import * as fs from 'fs/promises';
import { exec } from 'child_process';
import { promisify } from 'util';
import { SocketSearchResult } from '../types/emacs-daemon.types.js';
import { logger } from './logger.js';

const execAsync = promisify(exec);

export abstract class SocketHandler {
  abstract getDefaultPaths(): string[];
  abstract findSocket(socketName?: string): Promise<SocketSearchResult>;
  abstract createSocketPath(name: string): string;
  abstract checkPermissions(path: string): Promise<boolean>;
  
  protected async fileExists(filePath: string): Promise<boolean> {
    try {
      await fs.access(filePath);
      return true;
    } catch {
      return false;
    }
  }
  
  protected async findInPaths(paths: string[], socketName?: string): Promise<SocketSearchResult> {
    for (const searchPath of paths) {
      try {
        if (!await this.fileExists(searchPath)) continue;
        
        const entries = await fs.readdir(searchPath);
        
        // If socketName specified, look for exact match
        if (socketName) {
          if (entries.includes(socketName)) {
            const fullPath = path.join(searchPath, socketName);
            if (await this.checkPermissions(fullPath)) {
              return {
                found: true,
                socketPath: fullPath,
                socketName,
                method: 'default'
              };
            }
          }
        } else {
          // Find any socket file
          for (const entry of entries) {
            const fullPath = path.join(searchPath, entry);
            const stats = await fs.stat(fullPath);
            if (stats.isSocket()) {
              if (await this.checkPermissions(fullPath)) {
                return {
                  found: true,
                  socketPath: fullPath,
                  socketName: entry,
                  method: 'scan'
                };
              }
            }
          }
        }
      } catch (error) {
        logger.debug('Error scanning socket path', { searchPath, error });
      }
    }
    
    return { found: false, method: 'scan' };
  }
}

export class LinuxSocketHandler extends SocketHandler {
  getDefaultPaths(): string[] {
    const uid = process.getuid!(); // We know this is Linux
    const paths = [];
    
    // XDG_RUNTIME_DIR is preferred on modern Linux
    const xdgRuntime = process.env.XDG_RUNTIME_DIR;
    if (xdgRuntime) {
      paths.push(path.join(xdgRuntime, 'emacs'));
    }
    
    // Traditional paths
    paths.push(
      `/run/user/${uid}/emacs`,
      `/tmp/emacs${uid}`,
      path.join(os.homedir(), '.emacs.d', 'server')
    );
    
    return paths;
  }
  
  async findSocket(socketName?: string): Promise<SocketSearchResult> {
    // Check environment variable first
    const envSocket = process.env.EMACS_SOCKET_NAME;
    if (envSocket) {
      if (await this.fileExists(envSocket) && await this.checkPermissions(envSocket)) {
        return {
          found: true,
          socketPath: envSocket,
          socketName: path.basename(envSocket),
          method: 'environment'
        };
      }
    }
    
    // Search default paths
    return this.findInPaths(this.getDefaultPaths(), socketName);
  }
  
  createSocketPath(name: string): string {
    const xdgRuntime = process.env.XDG_RUNTIME_DIR;
    if (xdgRuntime) {
      return path.join(xdgRuntime, 'emacs', name);
    }
    
    const uid = process.getuid!();
    return path.join(`/tmp/emacs${uid}`, name);
  }
  
  async checkPermissions(socketPath: string): Promise<boolean> {
    try {
      const stats = await fs.stat(socketPath);
      // Check if it's a socket and we have read/write permissions
      return stats.isSocket() && (stats.mode & 0o600) === 0o600;
    } catch {
      return false;
    }
  }
}

export class MacOSSocketHandler extends SocketHandler {
  getDefaultPaths(): string[] {
    const uid = process.getuid!();
    const paths = [
      path.join(os.homedir(), 'Library', 'Emacs'),
      `/tmp/emacs${uid}`,
      path.join(os.homedir(), '.emacs.d', 'server'),
      '/tmp'  // Some Emacs builds use /tmp directly
    ];
    
    // Add any paths from TMPDIR
    const tmpdir = process.env.TMPDIR;
    if (tmpdir) {
      paths.unshift(path.join(tmpdir, 'emacs'));
    }
    
    return paths;
  }
  
  async findSocket(socketName?: string): Promise<SocketSearchResult> {
    // Environment variable check
    const envSocket = process.env.EMACS_SOCKET_NAME;
    if (envSocket) {
      if (await this.fileExists(envSocket) && await this.checkPermissions(envSocket)) {
        return {
          found: true,
          socketPath: envSocket,
          socketName: path.basename(envSocket),
          method: 'environment'
        };
      }
    }
    
    // Search default paths
    const result = await this.findInPaths(this.getDefaultPaths(), socketName);
    
    // If not found, try process scanning as fallback
    if (!result.found) {
      return this.findViaProcess(socketName);
    }
    
    return result;
  }
  
  private async findViaProcess(socketName?: string): Promise<SocketSearchResult> {
    try {
      // Use lsof to find emacs sockets
      const { stdout } = await execAsync('lsof -U -a -c emacs 2>/dev/null | grep LISTEN');
      const lines = stdout.trim().split('\n');
      
      for (const line of lines) {
        const match = line.match(/\s+(\S+)\s*$/);
        if (match && match[1]) {
          const socketPath = match[1];
          if (socketName && !socketPath.includes(socketName)) continue;
          
          if (await this.checkPermissions(socketPath)) {
            return {
              found: true,
              socketPath,
              socketName: path.basename(socketPath),
              method: 'process'
            };
          }
        }
      }
    } catch {
      // lsof might not be available or no sockets found
    }
    
    return { found: false, method: 'process' };
  }
  
  createSocketPath(name: string): string {
    const tmpdir = process.env.TMPDIR || '/tmp';
    return path.join(tmpdir, 'emacs', name);
  }
  
  async checkPermissions(socketPath: string): Promise<boolean> {
    try {
      const stats = await fs.stat(socketPath);
      return stats.isSocket();
    } catch {
      return false;
    }
  }
}

export class WindowsSocketHandler extends SocketHandler {
  getDefaultPaths(): string[] {
    // Windows uses named pipes, not file sockets
    return [];
  }
  
  async findSocket(socketName?: string): Promise<SocketSearchResult> {
    // Check if emacsclient can connect
    try {
      const nameArg = socketName ? `--socket-name=${socketName}` : '';
      const { stdout } = await execAsync(`emacsclient ${nameArg} --eval "(emacs-pid)" 2>nul`);
      const pid = parseInt(stdout.trim());
      
      if (!isNaN(pid)) {
        return {
          found: true,
          socketName: socketName || 'server',
          method: 'default'
        };
      }
    } catch {
      // emacsclient failed
    }
    
    // Try to find via wmic
    try {
      const { stdout } = await execAsync('wmic process where name="emacs.exe" get processid');
      const lines = stdout.trim().split('\n').slice(1); // Skip header
      
      if (lines.length > 0 && lines[0].trim()) {
        return {
          found: true,
          socketName: socketName || 'server',
          method: 'process'
        };
      }
    } catch {
      // wmic might not be available
    }
    
    return { found: false, method: 'scan' };
  }
  
  createSocketPath(name: string): string {
    // Windows uses named pipes like \\.\pipe\emacs-server
    return `\\\\.\\pipe\\${name}`;
  }
  
  async checkPermissions(socketPath: string): Promise<boolean> {
    // On Windows, we check via emacsclient connection
    return true;
  }
}

// Factory function to get platform-specific handler
export function createSocketHandler(): SocketHandler {
  switch (process.platform) {
    case 'linux':
      return new LinuxSocketHandler();
    case 'darwin':
      return new MacOSSocketHandler();
    case 'win32':
      return new WindowsSocketHandler();
    default:
      // Fallback to Linux behavior
      return new LinuxSocketHandler();
  }
}

// High-level discovery function
export async function discoverEmacsSocket(
  socketName?: string,
  additionalPaths?: string[]
): Promise<SocketSearchResult> {
  const handler = createSocketHandler();
  
  // Try standard discovery first
  let result = await handler.findSocket(socketName);
  
  // If not found and additional paths provided, search those
  if (!result.found && additionalPaths && additionalPaths.length > 0) {
    result = await (handler as any).findInPaths(additionalPaths, socketName);
  }
  
  return result;
}

// Get platform-specific socket creation path
export function getSocketPath(socketName: string): string {
  const handler = createSocketHandler();
  return handler.createSocketPath(socketName);
}