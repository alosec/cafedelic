/**
 * File Operations Transform
 * Extracts file operations from MCP log entries
 */

import { LogEntry } from '../watchers/types.js';
import { FileAction } from './types.js';

export function extractFileOperation(entry: LogEntry): FileAction | null {
  // Look for MCP tool calls
  if (entry.type === 'request' && entry.content?.method === 'tools/call') {
    const { name, arguments: args } = entry.content.params || {};
    
    // Check for read_file tool
    if (name === 'read_file' && args?.path) {
      console.log(`[DESKTOP → EMACS] Opening file: ${args.path}`);
      return {
        type: 'read',
        path: args.path,
        timestamp: entry.timestamp
      };
    }
    
    // Check for list_directory tool
    if (name === 'list_directory' && args?.path) {
      console.log(`[DESKTOP → EMACS] Opening directory: ${args.path}`);
      return {
        type: 'list',
        path: args.path,
        timestamp: entry.timestamp
      };
    }
    
    // Check for write_file/edit_block tools
    if (name === 'write_file' && args?.path) {
      console.log(`[DESKTOP → EMACS] Opening written file: ${args.path}`);
      return {
        type: 'write',
        path: args.path,
        timestamp: entry.timestamp
      };
    }
    
    if (name === 'edit_block' && args?.file_path) {
      console.log(`[DESKTOP → EMACS] Opening edited file: ${args.file_path}`);
      return {
        type: 'write',
        path: args.file_path,
        timestamp: entry.timestamp
      };
    }
  }
  
  return null;
}