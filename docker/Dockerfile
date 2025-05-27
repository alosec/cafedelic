FROM debian:12-slim

# Set environment to avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive
ENV NODE_MAJOR=20

# Install base dependencies
RUN apt-get update && apt-get install -y \
    curl \
    gnupg \
    ca-certificates \
    git \
    tmux \
    build-essential \
    python3 \
    && rm -rf /var/lib/apt/lists/*

# Add NodeSource repository and install Node.js
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_MAJOR}.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy package files first for better layer caching
COPY package*.json ./

# Install dependencies (including native builds for sqlite3)
RUN npm ci

# Copy TypeScript config and source files
COPY tsconfig.json ./
COPY index.ts ./
COPY src/ ./src/
COPY scripts/ ./scripts/

# Build TypeScript
RUN npm run build

# Create directories for runtime
RUN mkdir -p /app/logs /app/data

# Set environment variables for cafedelic
ENV CAFEDELIC_SERVER_NAME=red
ENV CAFEDELIC_PORT=3001
ENV CAFEDELIC_DATA_DIR=/app/data
ENV CAFEDELIC_LOG_DIR=/app/logs

# Copy startup script
COPY docker/startup.sh /usr/local/bin/startup.sh
RUN chmod +x /usr/local/bin/startup.sh

# Expose port for future HTTP API
EXPOSE 3001

# Use startup script as entrypoint
ENTRYPOINT ["/usr/local/bin/startup.sh"]