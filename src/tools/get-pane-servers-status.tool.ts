// get-pane-servers-status.tool.ts - Get status of all pane-specific emacs servers

import { paneEmacsManager } from '../services/pane-emacs-manager.service.js';
import { paneServerMapper } from '../utils/pane-server-mapping.js';
import { configManager } from '../config/cafedelic.config.js';

export const name = 'get_pane_servers_status';

export const description = 'Get the status of all pane-specific emacs servers';

export const inputSchema = {
  type: 'object',
  properties: {},
  required: []
};

export async function handler(): Promise<any> {
  try {
    const config = configManager.getConfig();
    const currentMode = config.emacs.mode || 'daemon';
    
    // Get server status
    const servers = await paneEmacsManager.getServerStatus();
    const mappings = await paneServerMapper.getAllMappings();
    
    // Convert Map to array for JSON serialization
    const serverList = Array.from(servers.entries()).map(([paneId, server]) => ({
      ...server,
      startTime: server.startTime.toISOString(),
      lastCheck: server.lastCheck?.toISOString()
    }));
    
    return {
      mode: currentMode,
      defaultPane: config.emacs.paneServers?.defaultPane,
      servers: serverList,
      mappings: mappings.map(m => ({
        ...m,
        timestamp: new Date(m.timestamp).toISOString()
      })),
      summary: {
        totalServers: serverList.length,
        readyServers: serverList.filter(s => s.status === 'ready').length,
        errorServers: serverList.filter(s => s.status === 'error').length
      }
    };
    
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error'
    };
  }
}
