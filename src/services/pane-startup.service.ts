/**
 * Pane Startup Service
 * Monitors pane properties and automatically starts emacs servers for editor panes
 */

import { EventEmitter } from 'events';
import { exec } from 'child_process';
import { promisify } from 'util';
import { logger } from '../utils/logger.js';
import { paneEmacsManager } from './pane-emacs-manager.service.js';

const execAsync = promisify(exec);

export class PaneStartupService extends EventEmitter {
  private monitorInterval: NodeJS.Timeout | null = null;
  private readonly checkInterval = 10000; // 10 seconds
  private knownEditorPanes = new Set<string>();

  constructor() {
    super();
    logger.info('PaneStartupService initialized');
  }

  /**
   * Start monitoring pane properties for automatic emacs startup
   */
  async startMonitoring(): Promise<void> {
    if (this.monitorInterval) {
      logger.warn('Pane monitoring already active');
      return;
    }

    logger.info('Starting pane property monitoring for automatic emacs startup');
    
    // Initial scan
    await this.scanForEditorPanes();
    
    // Set up periodic scanning
    this.monitorInterval = setInterval(async () => {
      try {
        await this.scanForEditorPanes();
      } catch (error) {
        logger.error('Error during pane scanning:', error);
      }
    }, this.checkInterval);

    this.emit('monitoring-started');
  }

  /**
   * Stop monitoring pane properties
   */
  stopMonitoring(): void {
    if (this.monitorInterval) {
      clearInterval(this.monitorInterval);
      this.monitorInterval = null;
      logger.info('Stopped pane property monitoring');
      this.emit('monitoring-stopped');
    }
  }

  /**
   * Scan for panes with editor role and ensure emacs servers are running
   */
  private async scanForEditorPanes(): Promise<void> {
    try {
      // Get all panes with role=editor
      const { stdout } = await execAsync(`
        tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}" | while read -r pane; do
          role=$(tmux show-options -pqv -t "$pane" @role 2>/dev/null || echo "")
          source=$(tmux show-options -pqv -t "$pane" @source 2>/dev/null || echo "")
          
          if [ "$role" = "editor" ]; then
            echo "\${pane}|\${source}"
          fi
        done
      `);

      const editorPanes = stdout.trim().split('\n').filter(line => line.trim());
      const currentEditorPanes = new Set<string>();

      for (const line of editorPanes) {
        if (!line.trim()) continue;
        
        const [paneId, source] = line.split('|');
        currentEditorPanes.add(paneId);

        // Check if this is a new editor pane
        if (!this.knownEditorPanes.has(paneId)) {
          logger.info(`Found new editor pane: ${paneId} (source: ${source})`);
          
          try {
            // Start emacs server for this pane
            await paneEmacsManager.getOrCreateServer(paneId);
            logger.info(`Emacs server started for editor pane: ${paneId}`);
            this.emit('emacs-server-started', { paneId, source });
          } catch (error) {
            logger.error(`Failed to start emacs server for pane ${paneId}:`, error);
          }
        }
      }

      // Update known panes
      this.knownEditorPanes = currentEditorPanes;

    } catch (error) {
      logger.error('Failed to scan for editor panes:', error);
    }
  }

  /**
   * Manually trigger a scan for editor panes
   */
  async triggerScan(): Promise<void> {
    logger.info('Manual scan triggered');
    await this.scanForEditorPanes();
  }

  /**
   * Get list of known editor panes
   */
  getKnownEditorPanes(): string[] {
    return Array.from(this.knownEditorPanes);
  }

  /**
   * Check if a specific pane is being monitored
   */
  isMonitoringPane(paneId: string): boolean {
    return this.knownEditorPanes.has(paneId);
  }

  /**
   * Shutdown the service
   */
  async shutdown(): Promise<void> {
    this.stopMonitoring();
    this.removeAllListeners();
    logger.info('PaneStartupService shutdown complete');
  }
}

// Export singleton instance
export const paneStartupService = new PaneStartupService();