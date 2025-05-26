import { promises as fs } from 'fs';
import path from 'path';

// Simple logger that writes to files
class Logger {
  private logDir: string;
  
  constructor() {
    this.logDir = path.join(process.cwd(), 'logs');
  }
  
  private async ensureLogDir() {
    try {
      await fs.mkdir(this.logDir, { recursive: true });
    } catch (error) {
      // Directory might already exist
    }
  }
  
  private formatMessage(level: string, message: string, data?: any): string {
    const timestamp = new Date().toISOString();
    let logLine = `${timestamp} [${level}] ${message}`;
    
    if (data) {
      logLine += ` ${JSON.stringify(data)}`;
    }
    
    return logLine;
  }
  
  async log(level: string, message: string, data?: any) {
    await this.ensureLogDir();
    
    const date = new Date().toISOString().split('T')[0];
    const logFile = path.join(this.logDir, `cafedelic-${date}.log`);
    const logLine = this.formatMessage(level, message, data) + '\n';
    
    try {
      await fs.appendFile(logFile, logLine);
    } catch (error) {
      // Fallback to stderr
      process.stderr.write(logLine);
    }
  }
  
  async info(message: string, data?: any) {
    await this.log('INFO', message, data);
  }
  
  async error(message: string, data?: any) {
    await this.log('ERROR', message, data);
  }
  
  async debug(message: string, data?: any) {
    await this.log('DEBUG', message, data);
  }
  
  async warn(message: string, data?: any) {
    await this.log('WARN', message, data);
  }
}

export const logger = new Logger();
