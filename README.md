# STGNN‑HTR: A reproducible spatio-temporal graph neural network framework for multi-source hand trauma risk prediction

## Title
STGNN‑HTR: A reproducible spatio-temporal graph neural network framework for multi-source hand trauma risk prediction

## Description
This repository contains the official implementation of STGNN‑HTR for hand trauma risk prediction across 14 cities in Hunan, China (2011–2023). The framework integrates hybrid graph construction (geographic + economic similarity), graph convolutional networks (GCN) for spatial dependencies, gated recurrent units (GRU) for temporal dynamics, and multi-head attention for adaptive feature fusion.

**Key features:**
- Hybrid graph construction (geographic adjacency + economic similarity based on cosine similarity > 80th percentile)
- Spatial modeling with 2-layer GCN
- Temporal modeling with GRU
- Multi-head attention (4 heads) for adaptive feature fusion
- GNNExplainer-based interpretability
- Full reproducibility: complete code, preprocessed data, and pre-trained weights

## Dataset Information
- **Source:** Hunan Provincial Health Commission & Hunan Provincial Bureau of Statistics
- **Processed dataset DOI:** https://doi.org/10.5281/zenodo.19217481
- **Spatial units:** 14 prefecture-level cities in Hunan Province, China (Changsha, Zhuzhou, Xiangtan, Yueyang, Changde, Hengyang, Chenzhou, Yiyang, Loudi, Yongzhou, Shaoyang, Huaihua, Xiangxi, Zhangjiajie)
- **Temporal coverage:** 2011–2023 (13 years, annual data)
- **Target variable:** Hand trauma incidence rate (per 100,000 population)
- **Features (9 total):** Resident population, population density, road network density, manufacturing/construction employee ratio, secondary industry GDP share, GDP per capita, physician density (per 1,000 population), college education proportion
- **Data split (chronological):** Training (2011–2019), Validation (2020–2021), Testing (2022–2023)
- **Preprocessing:** Linear interpolation for missing education data; min-max scaling to [0,1]

## Code Information
- Main training script: `src/train.py`
- Model definition: `src/model.py`
- Graph construction: `src/graph_builder.py`
- Interpretability (GNNExplainer): `scripts/run_gnnexplainer.py`
- Evaluation metrics: `src/utils.py`
- Configuration files: `configs/default.yaml`
- Visualization notebooks: `notebooks/`

## Usage Instructions

### Prerequisites
- Python 3.9+
- PyTorch 1.13+
- PyTorch Geometric 2.3+
- NVIDIA GPU (recommended; tested on Tesla T4 with 16GB memory)

### Installation
```bash
git clone https://github.com/CIAM-Lab/stgnn-hand-injury.git
cd stgnn-hand-injury
pip install -r requirements.txt# AdaptVisR
Causal Intervention Attention Mechanism (CIAM): A Unified Framework for Trustworthy Visual Recognition under Domain Shift. Official implementation in R with full-stack adaptive learning system. MICCAI 2025 paper.
Data Preparation
Download the processed dataset from Zenodo (DOI: 10.5281/zenodo.19217481) and place files in the data/ directory.
Training
python src/train.py --config configs/default.yaml
Reproducing Paper Results
# Table 2: Baseline comparison
python scripts/run_baselines.py

# Table 3: Ablation studies
python scripts/run_ablation.py

# Figure 2: Sensitivity analysis
python scripts/run_sensitivity.py

# Interpretability analysis
python scripts/run_gnnexplainer.py
Requirements
Create requirements.txt with:
torch==1.13.0
torch-geometric==2.3.0
numpy==1.23.5
pandas==1.5.3
scikit-learn==1.2.2
matplotlib==3.6.3
jupyter==1.0.0
tqdm==4.65.0
pyyaml==6.0
Methodology
Graph construction: Hybrid adjacency matrix A = A^geo ∨ A^eco, where A^geo is geographic adjacency and A^eco is economic similarity based on cosine similarity (threshold: 80th percentile)

Spatial modeling: Two-layer Graph Convolutional Network (GCN) with ReLU activation

Temporal modeling: GRU processing sequence of spatial embeddings (T=5 time steps)

Adaptive fusion: Multi-head self-attention (4 heads) over GRU outputs

Output: Fully connected layer predicting next-year incidence (MSE loss with L2 regularization)

Citation
If you use this code or data, please cite:
@article{chen2026stgnn,
  title={STGNN‑HTR: A reproducible spatio-temporal graph neural network framework for multi-source hand trauma risk prediction},
  author={Chen, Ye and Qin, Xiaoqun and Chen, Shouping},
  journal={PeerJ Computer Science},
  year={2026}
}
License
MIT License - see LICENSE file for details.

Contact
Chen Ye: 295256318@qq.com

Xiaoqun Qin (Corresponding author): 1094426728@qq.com

Acknowledgments
Supported by Provincial Health Research Program Grant No. 25C1230. We thank the Hunan Provincial Health Commission and Bureau of Statistics for providing the data.
