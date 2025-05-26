import * as path from 'path';
import { DCLogEntry } from './watcher.service.js';

interface CommandTemplate {
  (args: any): string;
}

export class TranslatorService {
  private templates: Record<string, CommandTemplate> = {
    'read_file': (args) => 
      `Claude is reading ${this.getFileName(args.path)}`,
    
    'write_file': (args) => 
      `Claude is ${args.mode === 'append' ? 'appending to' : 'updating'} ${this.getFileName(args.path)}`,
    
    'edit_block': (args) => 
      `Claude is editing ${this.getFileName(args.file_path)}`,
    
    'search_code': (args) => 
      `Claude searched for "${args.pattern}" in ${this.getPath(args.path)}`,
    
    'search_files': (args) =>
      `Claude is looking for files matching "${args.pattern}" in ${this.getPath(args.path)}`,
    
    'execute_command': (args) => 
      `Claude executed: ${args.command}`,
    
    'list_directory': (args) => 
      `Claude is exploring ${this.getPath(args.path)}`,
    
    'create_directory': (args) => 
      `Claude created directory ${this.getPath(args.path)}`,
    
    'get_file_info': (args) =>
      `Claude is checking info for ${this.getFileName(args.path)}`,
    
    'move_file': (args) =>
      `Claude moved ${this.getFileName(args.source)} to ${this.getFileName(args.destination)}`
  };

  translate(entry: DCLogEntry): string {
    const time = this.formatTime(entry.timestamp);
    const template = this.templates[entry.command];
    
    if (!template) {
      // Fallback for unknown commands
      return `[${time}] Claude used ${entry.command}`;
    }

    try {
      const message = template(entry.args || {});
      return `[${time}] ${message}`;
    } catch (error) {
      // Handle template errors gracefully
      return `[${time}] Claude used ${entry.command}`;
    }
  }

  private formatTime(timestamp: string): string {
    const date = new Date(timestamp);
    const hours = date.getHours();
    const minutes = date.getMinutes().toString().padStart(2, '0');
    const ampm = hours >= 12 ? 'pm' : 'am';
    const displayHours = hours % 12 || 12;
    
    return `${displayHours}:${minutes}${ampm}`;
  }

  private getFileName(filePath: string): string {
    return path.basename(filePath) || filePath;
  }

  private getPath(dirPath: string): string {
    // Shorten home directory
    if (dirPath.startsWith(process.env.HOME!)) {
      return dirPath.replace(process.env.HOME!, '~');
    }
    return dirPath;
  }
}
