#!/bin/bash
# AdaptVisR Docker Entrypoint Script

# Activate Python virtual environment if needed
if [ -f "/workspace/venv/bin/activate" ]; then
    source /workspace/venv/bin/activate
fi

# Set R library path
export R_LIBS_USER=/workspace/R/library

# Print welcome message
echo "========================================"
echo "AdaptVisR - CIAM Framework"
echo "Causal Intervention Attention Mechanism"
echo "MICCAI 2025 Paper Implementation"
echo "========================================"
echo ""
echo "Environment:"
echo "  R version: $(R --version | head -n 1)"
echo "  Python: $(python3 --version)"
echo "  PyTorch: $(python3 -c 'import torch; print(torch.__version__)' 2>/dev/null || echo 'Not installed')"
echo "  TensorFlow: $(python3 -c 'import tensorflow as tf; print(tf.__version__)' 2>/dev/null || echo 'Not installed')"
echo ""
echo "Working directory: $(pwd)"
echo ""

# Execute the command passed to docker run
exec "$@"
