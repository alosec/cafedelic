#!/bin/bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Building Cafedelic Docker image...${NC}"

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Error: Docker is not installed${NC}"
    exit 1
fi

# Get script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Change to project root
cd "$PROJECT_ROOT"

# Build the Docker image
echo -e "${YELLOW}Building Docker image 'cafedelic:latest'...${NC}"
docker build -f docker/Dockerfile -t cafedelic:latest .

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Docker image built successfully!${NC}"
    echo -e "${GREEN}Image: cafedelic:latest${NC}"
    
    # Show image info
    docker images cafedelic:latest
else
    echo -e "${RED}✗ Docker build failed${NC}"
    exit 1
fi

echo -e "\n${GREEN}To run the container:${NC}"
echo "  cd docker && docker-compose up"