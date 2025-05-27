import { routingManager, AssignmentResult } from '../services/routing-manager.service.js';
import { logger } from '../utils/logger.js';

export interface SetEditorDestinationArgs {
  paneSpec: string;
  description?: string;
}

/**
 * Set the editor destination pane for cafedelic output
 * Allows users to dynamically configure where cafedelic sends its output
 */
export async function setEditorDestination(args: SetEditorDestinationArgs): Promise<AssignmentResult> {
  try {
    logger.info('Setting editor destination', { paneSpec: args.paneSpec });
    
    const result = await routingManager.setEditorDestination(args.paneSpec);
    
    if (result.success) {
      logger.info('Editor destination set successfully', result);
    } else {
      logger.warn('Failed to set editor destination', result);
    }
    
    return result;
    
  } catch (error) {
    logger.error('Error setting editor destination', { args, error });
    
    return {
      success: false,
      destination: args.paneSpec,
      message: 'Internal error setting editor destination',
      error: (error as Error).message
    };
  }
}

/**
 * Get current routing assignments
 */
export async function getRoutingAssignments(): Promise<{
  success: boolean;
  assignments: Record<string, any>;
  message: string;
}> {
  try {
    const assignments = routingManager.getAssignments();
    
    return {
      success: true,
      assignments,
      message: `Found ${Object.keys(assignments).length} active routing assignments`
    };
    
  } catch (error) {
    logger.error('Error getting routing assignments', { error });
    
    return {
      success: false,
      assignments: {},
      message: 'Failed to get routing assignments'
    };
  }
}

/**
 * Clear routing assignment for a role
 */
export async function clearRoutingAssignment(args: { role: string }): Promise<{
  success: boolean;
  message: string;
}> {
  try {
    const cleared = routingManager.clearAssignment(args.role);
    
    return {
      success: cleared,
      message: cleared 
        ? `Cleared routing assignment for role: ${args.role}`
        : `No assignment found for role: ${args.role}`
    };
    
  } catch (error) {
    logger.error('Error clearing routing assignment', { args, error });
    
    return {
      success: false,
      message: 'Failed to clear routing assignment'
    };
  }
}
