/**
 * Routing Configuration for WTE Pipelines
 * Reads routing config and provides dynamic destinations
 */

import { readFile } from 'fs/promises';
import { homedir } from 'os';
import { join } from 'path';

const ROUTING_CONFIG_PATH = join(homedir(), '.cafedelic', 'routing-config');
const PANE_NAMES_PATH = join(homedir(), '.cafedelic', 'pane-names');

export interface RoutingConfig {
  [outputType: string]: string; // type -> pane name
}

export interface PaneMapping {
  [paneName: string]: string; // name -> coordinates
}

/**
 * Load routing configuration from file
 */
export async function loadRoutingConfig(): Promise<RoutingConfig> {
  try {
    const content = await readFile(ROUTING_CONFIG_PATH, 'utf-8');
    const config: RoutingConfig = {};
    
    content.split('\n').forEach(line => {
      const [type, paneName] = line.split('=');
      if (type && paneName) {
        config[type] = paneName;
      }
    });
    
    return config;
  } catch (error) {
    // No config file yet
    return {};
  }
}

/**
 * Load pane name mappings
 */
export async function loadPaneNames(): Promise<PaneMapping> {
  try {
    const content = await readFile(PANE_NAMES_PATH, 'utf-8');
    const mapping: PaneMapping = {};
    
    content.split('\n').forEach(line => {
      const [coords, name] = line.split('=');
      if (coords && name) {
        mapping[name] = coords;
      }
    });
    
    return mapping;
  } catch (error) {
    return {};
  }
}

/**
 * Get target pane coordinates for an output type
 */
export async function getTargetPane(outputType: string): Promise<string | null> {
  const routing = await loadRoutingConfig();
  const paneName = routing[outputType];
  
  if (!paneName) {
    return null; // No routing configured for this type
  }
  
  const paneNames = await loadPaneNames();
  return paneNames[paneName] || null;
}

/**
 * Create a routing-aware transform
 */
export function createRoutingTransform(defaultType: string = 'files') {
  return async (operation: any) => {
    const targetPane = await getTargetPane(operation.type || defaultType);
    return {
      ...operation,
      targetPane
    };
  };
}
