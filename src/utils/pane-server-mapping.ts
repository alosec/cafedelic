// pane-server-mapping.ts - Track mappings between tmux panes and emacs servers

import * as fs from 'fs/promises';
import * as path from 'path';
import { logger } from './logger.js';

export interface PaneServerMapping {
  paneId: string;      // e.g., "9:0.2"
  serverName: string;  // e.g., "pane-9-0-2"
  timestamp: number;   // Unix timestamp
}

export class PaneServerMapper {
  private stateDir: string;
  private mappingFile: string;
  
  constructor() {
    this.stateDir = path.join(process.env.HOME || '', '.cafedelic', 'pane-servers');
    this.mappingFile = path.join(this.stateDir, 'mappings.txt');
  }
  
  /**
   * Get the server name for a given pane
   */
  async getServerForPane(paneId: string): Promise<string | null> {
    try {
      // Check state file first
      const stateFile = path.join(this.stateDir, `pane-${paneId}.server`);
      try {
        const serverName = (await fs.readFile(stateFile, 'utf-8')).trim();
        if (serverName) {
          logger.debug('Found server from state file', { paneId, serverName });
          return serverName;
        }
      } catch (e) {
        // State file doesn't exist, check mappings
      }
      
      // Check mappings file
      const mappings = await this.loadMappings();
      const mapping = mappings.find(m => m.paneId === paneId);
      
      if (mapping) {
        logger.debug('Found server from mappings', { paneId, serverName: mapping.serverName });
        return mapping.serverName;
      }
      
      return null;
    } catch (error) {
      logger.error('Failed to get server for pane', { paneId, error });
      return null;
    }
  }
  
  /**
   * Store a pane-server mapping
   */
  async setServerForPane(paneId: string, serverName: string): Promise<void> {
    try {
      // Ensure state directory exists
      await fs.mkdir(this.stateDir, { recursive: true });
      
      // Write state file
      const stateFile = path.join(this.stateDir, `pane-${paneId}.server`);
      await fs.writeFile(stateFile, serverName);
      
      // Update mappings file
      const mappings = await this.loadMappings();
      const existingIndex = mappings.findIndex(m => m.paneId === paneId);
      
      const newMapping: PaneServerMapping = {
        paneId,
        serverName,
        timestamp: Date.now()
      };
      
      if (existingIndex >= 0) {
        mappings[existingIndex] = newMapping;
      } else {
        mappings.push(newMapping);
      }
      
      await this.saveMappings(mappings);
      logger.info('Stored pane-server mapping', { paneId, serverName });
      
    } catch (error) {
      logger.error('Failed to store pane-server mapping', { paneId, serverName, error });
      throw error;
    }
  }
  
  /**
   * Remove a pane-server mapping
   */
  async removeMapping(paneId: string): Promise<void> {
    try {
      // Remove state file
      const stateFile = path.join(this.stateDir, `pane-${paneId}.server`);
      try {
        await fs.unlink(stateFile);
      } catch (e) {
        // File might not exist
      }
      
      // Remove from mappings
      const mappings = await this.loadMappings();
      const filtered = mappings.filter(m => m.paneId !== paneId);
      
      if (filtered.length < mappings.length) {
        await this.saveMappings(filtered);
        logger.info('Removed pane-server mapping', { paneId });
      }
      
    } catch (error) {
      logger.error('Failed to remove pane-server mapping', { paneId, error });
    }
  }
  
  /**
   * Get all current mappings
   */
  async getAllMappings(): Promise<PaneServerMapping[]> {
    return await this.loadMappings();
  }
  
  /**
   * Generate server name for a pane
   */
  generateServerName(paneId: string, suffix?: string): string {
    // Convert "9:0.2" to "pane-9-0-2"
    const base = `pane-${paneId.replace(/[:\.]/g, '-')}`;
    return suffix ? `${base}-${suffix}` : base;
  }
  
  private async loadMappings(): Promise<PaneServerMapping[]> {
    try {
      const content = await fs.readFile(this.mappingFile, 'utf-8');
      const lines = content.trim().split('\n').filter(line => line);
      
      return lines.map(line => {
        const [paneId, serverName, timestamp] = line.split(':');
        return {
          paneId,
          serverName,
          timestamp: parseInt(timestamp) || Date.now()
        };
      });
    } catch (error) {
      // File doesn't exist yet
      return [];
    }
  }
  
  private async saveMappings(mappings: PaneServerMapping[]): Promise<void> {
    const lines = mappings.map(m => 
      `${m.paneId}:${m.serverName}:${m.timestamp}`
    );
    
    await fs.writeFile(this.mappingFile, lines.join('\n') + '\n');
  }
}

export const paneServerMapper = new PaneServerMapper();
