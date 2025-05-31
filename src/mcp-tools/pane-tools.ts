/**
 * MCP Tools for Pane Management
 * Provides tools for naming, reading, and interacting with tmux panes
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const execAsync = promisify(exec);
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Path to scripts directory
const SCRIPTS_DIR = join(__dirname, '../../scripts');

/**
 * Execute a shell script with error handling
 */
async function runScript(scriptPath: string, args: string[] = []): Promise<{ stdout: string; stderr: string }> {
  try {
    const fullPath = join(SCRIPTS_DIR, scriptPath);
    const command = `"${fullPath}" ${args.map(arg => `"${arg}"`).join(' ')}`;
    const { stdout, stderr } = await execAsync(command);
    return { stdout: stdout.trim(), stderr: stderr.trim() };
  } catch (error: any) {
    throw new Error(error.stderr || error.message);
  }
}

/**
 * Pane naming tools
 */
export const paneNamingTools = {
  assignNameToPane: async (session: string, window: string | number, pane: number, name: string) => {
    const result = await runScript('pane-management/assign-name.sh', [
      session,
      window.toString(),
      pane.toString(),
      name
    ]);
    return { success: true, message: result.stdout };
  },

  readPaneByName: async (name: string, lines: number = 100) => {
    const result = await runScript('pane-management/read-pane.sh', [name, lines.toString()]);
    return { 
      name,
      lines,
      output: result.stdout 
    };
  },

  sendKeysToPane: async (name: string, text: string) => {
    const result = await runScript('pane-management/send-keys-to-pane.sh', [name, text]);
    return { success: true, message: result.stdout };
  },

  listNamedPanes: async () => {
    const result = await runScript('pane-management/list-named-panes.sh');
    return { panes: result.stdout };
  }
};

/**
 * Pane interaction tools
 */
export const paneInteractionTools = {
  sendSpecialKeyToPane: async (name: string, key: string) => {
    const result = await runScript('pane-management/send-special-key.sh', [name, key]);
    return { success: true, message: result.stdout };
  },

  sendCtrlCToPane: async (name: string, doubleTap: boolean = false) => {
    const result = await runScript('pane-management/send-ctrl-c.sh', [
      name, 
      doubleTap ? 'true' : 'false'
    ]);
    return { success: true, message: result.stdout };
  }
};


