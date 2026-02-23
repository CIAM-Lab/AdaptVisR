# Docker Environment for AdaptVisR

This directory contains Docker configuration for running AdaptVisR in a reproducible environment.

## Quick Start

### Build the Docker image

```bash
cd /path/to/AdaptVisR
docker build -t adaptvisr -f docker/Dockerfile .
