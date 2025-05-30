/**
 * Emacs Executor
 * Executes file operations in Emacs
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import { FileAction } from '../transforms/types.js';

const execAsync = promisify(exec);

export async function openInEmacs(action: FileAction, options?: { readOnly?: boolean }): Promise<void> {
  const scriptBase = `${process.cwd()}/scripts/emacs/pane-server`;
  const paneId = process.env.CAFEDELIC_EMACS_PANE || '0:1.2';
  
  console.log(`[EXECUTE] ${action.type} ${action.path} -> pane ${paneId}${options?.readOnly ? ' (read-only)' : ''}`);
  
  switch (action.type) {
    case 'read':
    case 'write': {
      const script = `${scriptBase}/open-file-in-pane.sh`;
      const readOnlyFlag = options?.readOnly ? 'READ_ONLY=1' : '';
      await execAsync(`${readOnlyFlag} bash ${script} "${action.path}" "${paneId}"`);
      break;
    }
    
    case 'list': {
      const script = `${scriptBase}/open-directory-in-pane.sh`;
      await execAsync(`bash ${script} "${action.path}" "${paneId}"`);
      break;
    }
  }
}