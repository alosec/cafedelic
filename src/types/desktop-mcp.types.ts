// Desktop MCP (Model Context Protocol) log types for Claude Desktop

export interface DesktopMCPLogEntry {
  timestamp: string;
  level: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';
  message: string;
  toolCall?: DesktopMCPToolCall;
}

export interface DesktopMCPToolCall {
  name: string;
  args: any;
  status: 'started' | 'completed' | 'failed';
  duration?: number; // milliseconds
  error?: string;
}

export interface DesktopMCPLogFile {
  path: string;
  windowId: string;
  serverName: string;
  lastModified: Date;
  size: number;
}

export interface DesktopMCPDiscoveryResult {
  found: boolean;
  logFiles: DesktopMCPLogFile[];
  activeCount: number;
  error?: string;
}

// Compatibility interface to match existing DCLogEntry
export interface DesktopMCPCompatibleEntry {
  timestamp: string;
  command: string;
  args: any;
}

// Log parsing result
export interface DesktopMCPParseResult {
  success: boolean;
  entry?: DesktopMCPLogEntry;
  error?: string;
}
