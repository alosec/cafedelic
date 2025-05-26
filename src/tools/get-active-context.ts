import { logger } from '../utils/logger.js';
import { ActivityStore } from '../services/activity.store.js';

// Activity store will be injected
let activityStore: ActivityStore;

export function setActivityStore(store: ActivityStore) {
  activityStore = store;
}

export async function getActiveContext(args: {
  include_activity?: boolean;
  lookback_minutes?: number;
}) {
  const includeActivity = args.include_activity !== false;
  const lookbackMinutes = args.lookback_minutes || 5;
  
  logger.info('Get active context called', { includeActivity, lookbackMinutes });
  
  // Get real activity data
  const summary = activityStore.getSummary(lookbackMinutes);
  
  let response = `Active Context Summary (last ${lookbackMinutes} minutes)\n\n`;
  
  if (includeActivity && summary.recentActivities.length > 0) {
    response += `Recent Activity:\n`;
    // Show last 10 activities
    const activities = summary.recentActivities.slice(-10);
    response += activities.join('\n') + '\n\n';
  } else if (includeActivity) {
    response += `No recent activity detected.\n\n`;
  }
  
  if (summary.filesUnderReview.length > 0) {
    response += `Files Under Review:\n`;
    response += summary.filesUnderReview.map(f => `- ${f}`).join('\n') + '\n\n';
  }
  
  if (summary.detectedPattern) {
    response += `Detected Pattern: ${summary.detectedPattern}\n\n`;
  }
  
  if (summary.commandCounts && Object.keys(summary.commandCounts).length > 0) {
    response += `Activity Summary:\n`;
    for (const [command, count] of Object.entries(summary.commandCounts)) {
      response += `- ${command}: ${count} times\n`;
    }
  }
  
  return {
    content: [{
      type: 'text',
      text: response
    }]
  };
}
