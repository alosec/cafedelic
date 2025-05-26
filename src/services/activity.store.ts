export interface Activity {
  timestamp: number;
  raw: any;
  translated: string;
}

export interface ActivitySummary {
  recentActivities: string[];
  filesUnderReview: string[];
  commandCounts: Record<string, number>;
  detectedPattern?: string;
}

export class ActivityStore {
  private activities: Activity[] = [];
  private maxSize = 1000;

  add(activity: Activity) {
    this.activities.push(activity);
    
    // Maintain size limit
    if (this.activities.length > this.maxSize) {
      this.activities.shift();
    }
  }

  getRecent(minutes: number = 5): Activity[] {
    const cutoff = Date.now() - (minutes * 60 * 1000);
    return this.activities.filter(a => a.timestamp > cutoff);
  }

  getSummary(minutes: number = 5): ActivitySummary {
    const recent = this.getRecent(minutes);
    
    // Extract file names from activities
    const filesUnderReview = new Set<string>();
    const commandCounts: Record<string, number> = {};
    
    for (const activity of recent) {
      // Count commands
      const command = activity.raw.command;
      commandCounts[command] = (commandCounts[command] || 0) + 1;
      
      // Extract file references
      const args = activity.raw.args || {};
      if (args.path && typeof args.path === 'string') {
        const fileName = args.path.split('/').pop();
        if (fileName) filesUnderReview.add(fileName);
      }
      if (args.file_path && typeof args.file_path === 'string') {
        const fileName = args.file_path.split('/').pop();
        if (fileName) filesUnderReview.add(fileName);
      }
    }
    
    // Detect patterns
    let detectedPattern: string | undefined;
    if (commandCounts.write_file > 3) {
      detectedPattern = 'Heavy file editing';
    } else if (commandCounts.search_code > 3) {
      detectedPattern = 'Code exploration';
    } else if (commandCounts.execute_command > 3) {
      detectedPattern = 'Testing or building';
    }
    
    return {
      recentActivities: recent.map(a => a.translated),
      filesUnderReview: Array.from(filesUnderReview),
      commandCounts,
      detectedPattern
    };
  }

  clear() {
    this.activities = [];
  }
}
