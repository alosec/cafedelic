import { EventEmitter } from 'events';
import { logger } from '../utils/logger.js';
import { executeCommand } from '../utils/shell.js';

export interface PaneAssignment {
  role: 'editor' | 'activity' | 'logs' | 'terminal';
  destination: string; // "session:window.pane" format (e.g., "0:0.1")
  active: boolean;
  lastAssigned: Date;
  emacsServerPid?: number;
  description?: string;
}

export interface AssignmentResult {
  success: boolean;
  destination: string;
  message: string;
  serverStarted?: boolean;
  error?: string;
}

export interface RoutingConfig {
  assignments: Record<string, PaneAssignment>;
  defaultDestination?: string;
  autoStartEmacs: boolean;
}

/**
 * Manages dynamic pane assignments and output routing
 * Replaces hard-coded destinations with flexible user-configurable routing
 */
export class RoutingManager extends EventEmitter {
  private assignments: Map<string, PaneAssignment> = new Map();
  private config: RoutingConfig;

  constructor() {
    super();
    this.config = {
      assignments: {},
      autoStartEmacs: true
    };
    
    // Load any existing assignments
    this.loadAssignments();
  }

  /**
   * Set the editor destination pane
   */
  async setEditorDestination(paneSpec: string): Promise<AssignmentResult> {
    try {
      // Validate pane specification format (session:window.pane)
      if (!this.isValidPaneSpec(paneSpec)) {
        return {
          success: false,
          destination: paneSpec,
          message: 'Invalid pane specification. Use format: session:window.pane (e.g., "0:0.1")',
          error: 'Invalid format'
        };
      }

      // Check if pane exists
      const paneExists = await this.verifyPaneExists(paneSpec);
      if (!paneExists) {
        return {
          success: false,
          destination: paneSpec,
          message: `Pane ${paneSpec} does not exist`,
          error: 'Pane not found'
        };
      }

      // Create assignment
      const assignment: PaneAssignment = {
        role: 'editor',
        destination: paneSpec,
        active: true,
        lastAssigned: new Date(),
        description: 'Cafedelic editor output destination'
      };

      // Start emacs server if configured
      let serverStarted = false;
      if (this.config.autoStartEmacs) {
        try {
          await this.startEmacsServer(paneSpec);
          serverStarted = true;
          logger.info(`Started emacs server for pane ${paneSpec}`);
        } catch (error) {
          logger.warn(`Failed to start emacs server for pane ${paneSpec}`, { error });
          // Don't fail the assignment, just warn
        }
      }

      // Store assignment
      this.assignments.set('editor', assignment);
      this.saveAssignments();

      // Emit event
      this.emit('assignment-changed', { role: 'editor', assignment });

      return {
        success: true,
        destination: paneSpec,
        message: `Editor destination set to pane ${paneSpec}`,
        serverStarted
      };

    } catch (error) {
      logger.error('Failed to set editor destination', { paneSpec, error });
      return {
        success: false,
        destination: paneSpec,
        message: 'Failed to set editor destination',
        error: (error as Error).message
      };
    }
  }

  /**
   * Route output to the appropriate destination
   */
  async routeOutput(content: string, role: string = 'editor'): Promise<void> {
    const assignment = this.assignments.get(role);
    
    if (!assignment || !assignment.active) {
      // No assignment, log warning and skip
      logger.warn(`No active assignment for role: ${role}`, { content: content.substring(0, 100) });
      return;
    }

    try {
      // Send content to the assigned pane using tmux
      await this.sendToPane(assignment.destination, content);
      
      // Update last used timestamp
      assignment.lastAssigned = new Date();
      this.assignments.set(role, assignment);
      
    } catch (error) {
      logger.error(`Failed to route output to ${assignment.destination}`, { role, error });
      
      // Mark assignment as inactive if routing fails consistently
      assignment.active = false;
      this.assignments.set(role, assignment);
    }
  }

  /**
   * Get current assignments
   */
  getAssignments(): Record<string, PaneAssignment> {
    const result: Record<string, PaneAssignment> = {};
    for (const [role, assignment] of this.assignments) {
      result[role] = { ...assignment };
    }
    return result;
  }

  /**
   * Clear assignment for a role
   */
  clearAssignment(role: string): boolean {
    const existed = this.assignments.has(role);
    this.assignments.delete(role);
    this.saveAssignments();
    
    if (existed) {
      this.emit('assignment-cleared', { role });
    }
    
    return existed;
  }

  /**
   * Get the destination for a specific role
   */
  getDestination(role: string): string | null {
    const assignment = this.assignments.get(role);
    return assignment && assignment.active ? assignment.destination : null;
  }

  // Private helper methods

  private isValidPaneSpec(paneSpec: string): boolean {
    // Format: session:window.pane (e.g., "0:0.1", "main:dev.2")
    return /^[^:]+:\d+\.\d+$/.test(paneSpec);
  }

  private async verifyPaneExists(paneSpec: string): Promise<boolean> {
    try {
      // Use tmux list-panes to check if pane exists
      const result = await executeCommand(`tmux list-panes -t ${paneSpec} -F '#{pane_id}' 2>/dev/null`);
      return result.code === 0 && result.stdout.trim().length > 0;
    } catch {
      return false;
    }
  }

  private async startEmacsServer(paneSpec: string): Promise<void> {
    // Use the existing pane server script
    const scriptPath = '/home/alex/code/cafedelic/scripts/emacs/pane-server/start-pane-emacs.sh';
    const result = await executeCommand(`${scriptPath} ${paneSpec}`);
    if (result.code !== 0) {
      throw new Error(`Failed to start emacs server: ${result.stderr}`);
    }
  }

  private async sendToPane(paneSpec: string, content: string): Promise<void> {
    // Escape content for tmux send-keys
    const escapedContent = content.replace(/'/g, "'\"'\"'");
    const result = await executeCommand(`tmux send-keys -t ${paneSpec} '${escapedContent}' Enter`);
    if (result.code !== 0) {
      throw new Error(`Failed to send content to pane: ${result.stderr}`);
    }
  }

  private loadAssignments(): void {
    // For now, keep assignments in memory
    // Future: could load from file or database
    logger.debug('Loading routing assignments from memory');
  }

  private saveAssignments(): void {
    // For now, keep assignments in memory  
    // Future: could save to file or database
    logger.debug('Saving routing assignments to memory');
  }
}

// Export singleton instance
export const routingManager = new RoutingManager();
