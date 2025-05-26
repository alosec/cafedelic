// Simple output routing service for directing output to specific tmux panes
import { exec } from 'child_process';
import { promisify } from 'util';
import { configManager } from '../config/cafedelic.config.js';
import { logger } from '../utils/logger.js';

const execAsync = promisify(exec);

export interface RoutingResult {
  success: boolean;
  destination: string;
  message: string;
}

export class OutputRouterService {
  /**
   * Get the destination pane for a given output type
   */
  private getDestination(type: string = 'default'): string {
    const config = configManager.getConfig();
    const outputConfig = config.output || { defaultPane: '9:0.2', assignments: {} };
    
    // Check for specific assignment first
    if (outputConfig.assignments && outputConfig.assignments[type]) {
      return outputConfig.assignments[type];
    }
    
    // Fall back to default
    return outputConfig.defaultPane || '9:0.2';
  }

  /**
   * Check if a tmux pane exists
   */
  private async paneExists(paneId: string): Promise<boolean> {
    try {
      await execAsync(`tmux list-panes -t "${paneId}" 2>/dev/null`);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Route content to a specific pane
   */
  async routeToPane(content: string, type: string = 'editor-output'): Promise<RoutingResult> {
    const destination = this.getDestination(type);
    
    try {
      // Check if destination pane exists
      if (!await this.paneExists(destination)) {
        logger.info(`Destination pane ${destination} does not exist`);
        return {
          success: false,
          destination,
          message: `Pane ${destination} not found`
        };
      }

      // Route content using tmux run-shell
      const escapedContent = content.replace(/'/g, "'\"'\"'");
      await execAsync(`tmux run-shell -t "${destination}" 'echo "${escapedContent}"'`);
      
      logger.info(`Routed output to pane ${destination}`, { type, contentLength: content.length });
      
      return {
        success: true,
        destination,
        message: `Output routed to ${destination}`
      };
    } catch (error) {
      const err = error as Error;
      logger.error('Failed to route output', { destination, error: err.message });
      return {
        success: false,
        destination,
        message: `Routing failed: ${err.message}`
      };
    }
  }

  /**
   * Get the current destination for a given type
   */
  getDestinationInfo(type: string = 'default'): { destination: string; source: string } {
    const destination = this.getDestination(type);
    const config = configManager.getConfig();
    const outputConfig = config.output || { defaultPane: '9:0.2', assignments: {} };
    
    const source = (outputConfig.assignments && outputConfig.assignments[type]) 
      ? 'assignment' 
      : 'default';
    
    return { destination, source };
  }

  /**
   * Find a pane by its cafedelic role
   */
  async findPaneByRole(role: string): Promise<string | null> {
    try {
      // Look for panes with @cafedelic_role set to the specified role
      const { stdout } = await execAsync(
        `tmux list-panes -a -F '#{pane_id} #{@cafedelic_role}' | grep "${role}" | awk '{print $1}' | head -1`
      );
      
      const paneId = stdout.trim();
      if (paneId) {
        logger.info(`Found pane ${paneId} with role ${role}`);
        return paneId;
      }
      
      return null;
    } catch {
      return null;
    }
  }

  /**
   * Assign a cafedelic role to a pane
   */
  async assignPaneRole(paneId: string, role: string): Promise<boolean> {
    try {
      await execAsync(`tmux set-option -t '${paneId}' @cafedelic_role '${role}'`);
      logger.info(`Assigned role ${role} to pane ${paneId}`);
      return true;
    } catch (error) {
      const err = error as Error;
      logger.error(`Failed to assign role to pane`, { paneId, role, error: err.message });
      return false;
    }
  }
}

// Singleton instance
export const outputRouter = new OutputRouterService();
