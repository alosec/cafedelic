#!/bin/bash
# Quick fix to add socket name passing to EmacsService

echo "Creating fix branch..."
cd /home/alex/code/cafedelic
git checkout -b fix/emacs-socket-passing

echo "Applying fix to emacs.service.ts..."
cat > /tmp/emacs-service-fix.patch << 'EOF'
--- a/src/services/emacs.service.ts
+++ b/src/services/emacs.service.ts
@@ -7,6 +7,7 @@ import * as path from 'path';
 import { configManager } from '../config/cafedelic.config.js';
 import { logger } from '../utils/logger.js';
 import { outputRouter } from './output-router.service.js';
+import { emacsDaemonManager } from './emacs-daemon-manager.service.js';
 
 const execAsync = promisify(exec);
 
@@ -65,8 +66,14 @@ export class EmacsService extends EventEmitter {
     const scriptPath = path.join(this.scriptsPath, 'open-claude-file-v2.sh');
     const command = `bash "${scriptPath}" "${filePath}"`;
     
+    // Get socket name from daemon manager
+    const daemonStatus = await emacsDaemonManager.getStatus();
+    const env = {
+      ...process.env,
+      CAFEDELIC_SOCKET_NAME: daemonStatus.socketName || ''
+    };
+    
     try {
-      const { stdout, stderr } = await execAsync(command, {
+      const { stdout, stderr } = await execAsync(command, {
         timeout: configManager.getConfig().emacs.daemonTimeout
+        timeout: configManager.getConfig().emacs.daemonTimeout,
+        env
       });
@@ -130,8 +137,14 @@ export class EmacsService extends EventEmitter {
     const scriptPath = path.join(this.scriptsPath, 'open-dired-v2.sh');
     const command = `bash "${scriptPath}" "${directoryPath}"`;
     
+    // Get socket name from daemon manager
+    const daemonStatus = await emacsDaemonManager.getStatus();
+    const env = {
+      ...process.env,
+      CAFEDELIC_SOCKET_NAME: daemonStatus.socketName || ''
+    };
+    
     try {
-      const { stdout, stderr } = await execAsync(command, {
+      const { stdout, stderr } = await execAsync(command, {
         timeout: configManager.getConfig().emacs.daemonTimeout
+        timeout: configManager.getConfig().emacs.daemonTimeout,
+        env
       });
EOF

echo "Fix created. To apply:"
echo "1. Review the patch above"
echo "2. Apply manually to src/services/emacs.service.ts"
echo "3. Add import for emacsDaemonManager"
echo "4. Pass env with CAFEDELIC_SOCKET_NAME to execAsync"
