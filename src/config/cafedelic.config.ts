// Cafedelic Configuration Management
import { EmacsDaemonConfig } from '../types/emacs-daemon.types.js';

export interface CafedelicConfig {
  emacs: {
    autoOpen: boolean;
    autoOpenDirectories: boolean;
    daemonTimeout: number;
    supportedExtensions: string[];
    scriptsPath: string;
    mode?: 'daemon' | 'pane-server' | 'plain';
    paneServers?: {
      defaultPane: string;
      mapping: Record<string, string>;
      autoStart: boolean;
      initFile?: string;
    };
    daemon?: Partial<EmacsDaemonConfig>;
  };
  logging: {
    level: 'debug' | 'info' | 'warn' | 'error';
    enableFileAccess: boolean;
  };
  performance: {
    batchDelay: number;
    maxConcurrentOpens: number;
  };
  output: {
    defaultPane: string;
    assignments: Record<string, string>;
  };
  desktopMCP: {
    enabled: boolean;
    logBaseDir: string;
    targetLogs?: string[];
    watchInterval: number;
    discoveryInterval: number;
    maxLogAge: number; // days
    fallbackToDC: boolean;
  };
}

export const defaultConfig: CafedelicConfig = {
  emacs: {
    autoOpen: true,
    autoOpenDirectories: true,
    daemonTimeout: 5000,
    supportedExtensions: [
      '.js', '.ts', '.jsx', '.tsx',
      '.py', '.java', '.cpp', '.c', '.h',
      '.md', '.txt', '.json', '.yaml', '.yml',
      '.css', '.scss', '.html', '.xml',
      '.sh', '.bash', '.zsh',
      '.el', '.lisp', '.clj',
      '.rs', '.go', '.rb', '.php'
    ],
    scriptsPath: '/home/alex/code/cafedelic/scripts/emacs',
    mode: 'daemon', // default to daemon mode for backward compatibility
    paneServers: {
      defaultPane: '', // No default - user must configure
      mapping: {
        // Dynamic assignments managed by RoutingManager
      },
      autoStart: true
    },
    daemon: {
      manageDaemon: true,
      lazyInit: true,
      reuseExisting: true,
      minimalConfig: true,
      uniqueSuffix: true,
      healthCheckInterval: 60000,
      maxRestartAttempts: 5
    }
  },
  logging: {
    level: 'info',
    enableFileAccess: true
  },
  performance: {
    batchDelay: 500, // ms delay to batch multiple file operations
    maxConcurrentOpens: 3
  },
  output: {
    defaultPane: '',  // No default - managed by RoutingManager
    assignments: {
      // Dynamic assignments managed by RoutingManager
    }
  },
  desktopMCP: {
    enabled: true,
    logBaseDir: '/home/alex/.config/Claude/logs',
    targetLogs: [
      'mcp-server-desktop-commander.log',  // Primary monitoring target
      'mcp.log'                           // General MCP activity
    ],
    watchInterval: 500,
    discoveryInterval: 30000,
    maxLogAge: 2, // days
    fallbackToDC: true
  }
};

class ConfigManager {
  private config: CafedelicConfig = { ...defaultConfig };

  getConfig(): CafedelicConfig {
    return { ...this.config };
  }

  updateConfig(updates: Partial<CafedelicConfig>): void {
    this.config = this.mergeDeep(this.config, updates);
  }

  toggleAutoOpen(): boolean {
    this.config.emacs.autoOpen = !this.config.emacs.autoOpen;
    return this.config.emacs.autoOpen;
  }

  isAutoOpenEnabled(): boolean {
    return this.config.emacs.autoOpen;
  }

  isFileSupported(filePath: string): boolean {
    const ext = this.getFileExtension(filePath);
    return this.config.emacs.supportedExtensions.includes(ext);
  }

  private getFileExtension(filePath: string): string {
    const match = filePath.match(/\.[^.]*$/);
    return match ? match[0].toLowerCase() : '';
  }

  private mergeDeep(target: any, source: any): any {
    const result = { ...target };
    
    for (const key in source) {
      if (source[key] && typeof source[key] === 'object' && !Array.isArray(source[key])) {
        result[key] = this.mergeDeep(target[key] || {}, source[key]);
      } else {
        result[key] = source[key];
      }
    }
    
    return result;
  }
}

export const configManager = new ConfigManager();
