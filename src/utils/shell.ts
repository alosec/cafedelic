import { spawn, exec as execCallback } from 'child_process';
import { promisify } from 'util';

const exec = promisify(execCallback);

export interface ShellResult {
  stdout: string;
  stderr: string;
  code: number;
}

// Execute a shell command and return the result
export async function executeCommand(command: string): Promise<ShellResult> {
  try {
    const { stdout, stderr } = await exec(command);
    return {
      stdout: stdout.toString(),
      stderr: stderr.toString(),
      code: 0
    };
  } catch (error: any) {
    return {
      stdout: error.stdout?.toString() || '',
      stderr: error.stderr?.toString() || error.message,
      code: error.code || 1
    };
  }
}

// Check if a command exists
export async function commandExists(command: string): Promise<boolean> {
  const result = await executeCommand(`which ${command}`);
  return result.code === 0;
}

// Check if tmux is installed
export async function checkTmuxInstalled(): Promise<boolean> {
  return await commandExists('tmux');
}

// Check if a tmux session exists
export async function tmuxSessionExists(sessionName: string): Promise<boolean> {
  const result = await executeCommand(`tmux has-session -t "${sessionName}" 2>/dev/null`);
  return result.code === 0;
}
