/**
 * Property-Based Emacs Executor
 * Uses source and role properties for intelligent pane routing
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const execAsync = promisify(exec);

// Path to scripts directory
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const SCRIPTS_DIR = join(__dirname, '../scripts');

interface FileOperation {
  type: 'file' | 'directory';
  path: string;
  source?: string;  // Where did this operation come from?
}

/**
 * Find best pane for a role with optional source preference
 */
async function findBestPane(role: string, preferredSource?: string): Promise<string | null> {
  try {
    const scriptPath = join(SCRIPTS_DIR, 'pane-properties/find-best-pane-for-role.sh');
    const args = [role];
    if (preferredSource) args.push(preferredSource);
    
    const command = `"${scriptPath}" ${args.map(arg => `"${arg}"`).join(' ')}`;
    const { stdout } = await execAsync(command);
    
    return stdout.trim();
  } catch (error) {
    console.error(`Failed to find pane for role=${role}:`, error);
    return null;
  }
}

/**
 * Create a property-aware Emacs executor
 */
export function propertyBasedEmacsExecutor(options: {
  emacsScriptPath: string;
  defaultRole?: string;
  defaultSource?: string;
} = {
  emacsScriptPath: join(SCRIPTS_DIR, 'emacs/open-claude-file.sh'),
  defaultRole: 'editor',
  defaultSource: 'claude-desktop'
}) {
  return async (op: FileOperation): Promise<void> => {
    try {
      // Determine source (could come from pipeline context)
      const source = op.source || options.defaultSource;
      
      // Find best pane for editor role
      const targetPane = await findBestPane(options.defaultRole || 'editor', source);
      
      if (!targetPane) {
        console.error(`No pane found with role='${options.defaultRole}'`);
        return;
      }
      
      // Execute the Emacs command in the found pane
      const command = `${options.emacsScriptPath} "${op.path}" "${targetPane}"`;
      await execAsync(command);
      
      console.log(`Opened ${op.path} in ${targetPane} (source: ${source}, role: ${options.defaultRole})`);
    } catch (error) {
      console.error('Failed to execute Emacs command:', error);
    }
  };
}

/**
 * Example: Create different executors for different sources
 */
export const claudeDesktopEmacsExecutor = propertyBasedEmacsExecutor({
  emacsScriptPath: join(SCRIPTS_DIR, 'emacs/open-claude-file.sh'),
  defaultSource: 'claude-desktop',
  defaultRole: 'editor'
});

export const userEmacsExecutor = propertyBasedEmacsExecutor({
  emacsScriptPath: join(SCRIPTS_DIR, 'emacs/open-claude-file.sh'),
  defaultSource: 'user',
  defaultRole: 'editor'
});

// Usage in pipeline:
// pipe(
//   mcpLogWatcher(),
//   fileOperationTransform,
//   claudeDesktopEmacsExecutor
// );
