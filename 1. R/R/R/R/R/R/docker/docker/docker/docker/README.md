Mount local directory
# Mount current directory to /workspace
docker run -it --rm -v $(pwd):/workspace adaptvisr
Container Details
Base image: rocker/tidyverse:4.3 (R 4.3 with tidyverse)
• Python: 3.10 with PyTorch 2.1.0 and TensorFlow 2.13.0
• R packages: torch, keras, shapviz, ggplot2, and more
• Working directory: /workspace
Environment Variables
￼
￼
Variable
Description
Default
RETICULATE_PYTHON
Python path
/usr/bin/python3
TORCH_HOME
PyTorch cache
/workspace/.torch
Volumes
• /workspace - Mount your code here
• /workspace/.torch - PyTorch model cache
Ports
• 8787 - RStudio Server (if installed)
• 8888 - Jupyter Lab (if installed)
text
￼
￼
￼
￼
提交信息：`Add docker README`

---

## 最终 docker 文件夹结构

创建完成后，您的 `docker/` 文件夹应该包含：
￼
￼
docker/
├── Dockerfile
├── entrypoint.sh
└── README.md
text
￼
复制
￼
￼
下载
￼
---

## 验证

现在您的仓库应该有：
- ✅ `R/` 文件夹（6个R文件）
- ✅ `docker/` 文件夹（3个文件）
- ✅ 根目录文件（README.md, LICENSE, .gitignore, DESCRIPTION, NAMESPACE, CITATION.cff）
