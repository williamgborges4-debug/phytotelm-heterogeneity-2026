# Introduction

This repository provides the data and R scripts needed to reproduce all analyses and results reported in the paper "Vegetation heterogeneity buffers phytotelm microclimate but has limited effects on litter breakdown." All analyses were conducted in R 4.3.1 (R Core Team, 2021).

# Downloading the Files

All files required to reproduce the analyses are available in this repository. To get started, download all scripts (R files) and data files (.csv files) listed below and save them in the same local directory.

# Description of Code Files in the Repository

- **0.5 INPUTATION.R** – Phylogenetic imputation of missing leaf traits, generating the complete trait dataset used in subsequent scripts.
- **1. FUNCTIONS.R** – Auxiliary functions used in other scripts.
- **2.DATA.R** – Import and organize the raw data, generating the input objects/tables for analysis.
- **3.ANALYSIS.R** – Statistical analyses and calculations of the derived metrics/variables used in the study.
- **4.SEM.R** – Adjustment of structural equation models (SEM) and production of associated outputs.

# How to Use

Run the scripts in numerical order:

1. INPUTATION
2. FUNCTIONS.R
3. DATA.R
4. ANALYSIS.R
5. SEM.R

# Reference

R Core Team. (2021). R: A language and environment for statistical computing [Software]. R Foundation for Statistical Computing.
