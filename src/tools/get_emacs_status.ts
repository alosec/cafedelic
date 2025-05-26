// Get Emacs Status MCP Tool
import { emacsService } from '../services/emacs.service.js';
import { configManager } from '../config/cafedelic.config.js';
import { logger } from '../utils/logger.js';

export interface GetEmacsStatusArgs {
  detailed?: boolean; // Include detailed configuration info
}

export async function getEmacsStatus(args: GetEmacsStatusArgs = {}): Promise<any> {
  try {
    const config = configManager.getConfig();
    const healthCheck = await emacsService.checkEmacsHealth();
    const pendingOpens = emacsService.getPendingOpens();
    
    let statusText = `🔍 Emacs Integration Status

**Daemon Status**: ${healthCheck.isRunning ? '✅ Running' : '❌ Not Running'}
${healthCheck.message}

**Auto-Open**: ${config.emacs.autoOpen ? '✅ Enabled' : '❌ Disabled'}

**Pending Operations**: ${pendingOpens.length}
${pendingOpens.length > 0 ? pendingOpens.map(file => `  - ${file}`).join('\n') : '  None'}

**Performance Settings**:
  - Max Concurrent Opens: ${config.performance.maxConcurrentOpens}
  - Batch Delay: ${config.performance.batchDelay}ms
  - Daemon Timeout: ${config.emacs.daemonTimeout}ms`;

    if (args.detailed) {
      statusText += `

**Supported File Extensions**:
${config.emacs.supportedExtensions.map(ext => `  ${ext}`).join(', ')}

**Scripts Path**: ${config.emacs.scriptsPath}

**Logging**:
  - Level: ${config.logging.level}
  - File Access Logging: ${config.logging.enableFileAccess ? 'enabled' : 'disabled'}`;
    }

    logger.info('Emacs status requested', { 
      daemonRunning: healthCheck.isRunning, 
      autoOpenEnabled: config.emacs.autoOpen,
      pendingCount: pendingOpens.length
    });
    
    return {
      content: [{
        type: 'text',
        text: statusText
      }]
    };
  } catch (error) {
    const err = error as Error;
    logger.error('Failed to get emacs status', { error: err.message });
    
    return {
      content: [{
        type: 'text',
        text: `❌ Failed to get emacs status: ${err.message}`
      }]
    };
  }
}
