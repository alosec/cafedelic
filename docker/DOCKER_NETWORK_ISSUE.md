# Docker Network Issue - Local Development

## Problem Description

When building Docker images on certain local development machines, the build process fails during `apt-get update` with DNS resolution errors:

```
Temporary failure resolving 'deb.debian.org'
```

The build hangs with repeated "Ign:" messages trying to reach Debian package repositories.

## Root Cause

This appears to be related to Docker's DNS resolution or network configuration on certain systems. The Docker daemon may not properly inherit or configure DNS settings during the build process.

## Solution

Use Docker's host network mode during the build process by creating a `docker-compose.override.yml` file:

```yaml
services:
  cafedelic-red:
    build:
      context: ..
      dockerfile: docker/Dockerfile
      network: host
```

This file is automatically picked up by Docker Compose and merges with the main `docker-compose.yml` file.

## Important Notes

- This fix only affects the build process, not running containers
- Running containers continue to use normal Docker networking
- The `docker-compose.override.yml` file should not be committed to version control if this is a local-only issue
- Modern Docker Compose does not require the `version` field (it's deprecated)

## Alternative Solutions

If the host network mode doesn't work, you can try:

1. Manually setting DNS in the Dockerfile:
   ```dockerfile
   RUN echo "nameserver 8.8.8.8" > /etc/resolv.conf
   ```

2. Configuring Docker daemon DNS settings in `/etc/docker/daemon.json`:
   ```json
   {
     "dns": ["8.8.8.8", "8.8.4.4"]
   }
   ```

3. Using a different package mirror in the Dockerfile

## References

- Similar issue resolved in austin_pedicab_rides_v4 project (commit 7080f8c)
- Docker networking documentation: https://docs.docker.com/network/