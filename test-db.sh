#!/bin/bash

# Test the database initialization
echo "🧪 Testing Cafedelic database initialization..."

cd /home/alex/code/cafedelic

# Run a simple test
node -e "
import { persistence } from './dist/src/services/persistence.service.js';

async function test() {
    try {
        console.log('Initializing database...');
        await persistence.initialize();
        console.log('✅ Database initialized successfully!');
        
        // Check if database file exists
        const fs = await import('fs');
        if (fs.existsSync('./cafedelic.db')) {
            console.log('✅ Database file created: cafedelic.db');
        }
        
        await persistence.close();
        console.log('✅ Database closed successfully!');
    } catch (error) {
        console.error('❌ Test failed:', error);
        process.exit(1);
    }
}

test();
"

echo "🎉 Test complete!"
