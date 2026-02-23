# AdaptVisR: Reproducible environment for CIAM framework
# Based on rocker/tidyverse for R with Python integration

FROM rocker/tidyverse:4.3

LABEL maintainer="Chen Ye <chen.ye@hieu.edu.cn>"
LABEL description="AdaptVisR: Causal Intervention Attention Mechanism for Trustworthy Visual Recognition"
LABEL version="1.0.0"

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC
ENV RETICULATE_PYTHON=/usr/bin/python3
ENV TORCH_HOME=/workspace/.torch

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Build tools
    build-essential \
    cmake \
    g++ \
    make \
    # Python
    python3-pip \
    python3-dev \
    python3-venv \
    # R dependencies
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # System utilities
    wget \
    curl \
    git \
    vim \
    nano \
    htop \
    # Clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Python packages for deep learning
RUN pip3 install --no-cache-dir \
    # Deep learning frameworks
    torch==2.1.0 \
    torchvision==0.16.0 \
    tensorflow==2.13.0 \
    # Scientific computing
    numpy==1.24.3 \
    scipy==1.10.1 \
    pandas==2.0.3 \
    scikit-learn==1.3.0 \
    # Visualization
    matplotlib==3.7.2 \
    seaborn==0.12.2 \
    # Mutual information estimation
    minepy==1.2.6 \
    # Utilities
    tqdm==4.65.0 \
    joblib==1.3.2

# Install R packages for deep learning and visualization
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    install.packages(c( \
        # Deep learning in R
        'torch', \
        'reticulate', \
        'keras', \
        'tensorflow', \
        # Data manipulation
        'dplyr', \
        'tidyr', \
        'purrr', \
        'data.table', \
        # Visualization
        'ggplot2', \
        'plotly', \
        'gridExtra', \
        'RColorBrewer', \
        # Interpretability
        'shapviz', \
        'iml', \
        # Statistics
        'moments', \
        'MASS', \
        # Testing
        'testthat', \
        # Documentation
        'knitr', \
        'rmarkdown', \
        'roxygen2', \
        'pkgdown', \
        # Utilities
        'remotes', \
        'devtools', \
        'usethis', \
        'here' \
    ), dependencies = TRUE)"

# Install development versions of key packages
RUN R -e "remotes::install_github('mlr-org/mlr3')"
RUN R -e "remotes::install_github('mlr-org/mlr3learners')"
RUN R -e "remotes::install_github('mlr-org/mlr3pipelines')"

# Create workspace directory
WORKDIR /workspace

# Copy AdaptVisR package
COPY . /workspace/AdaptVisR

# Install AdaptVisR package
RUN R -e "remotes::install_local('/workspace/AdaptVisR', dependencies = TRUE, force = TRUE)"

# Create directory for cached models
RUN mkdir -p /workspace/.torch

# Set up entrypoint
COPY docker/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Set default command
CMD ["R"]

# Expose ports for Jupyter/RStudio if needed
EXPOSE 8787 8888
