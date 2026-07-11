# SMDAP Severe Mental Disorder AI-Assisted Diagnosis and Prognosis Prediction System

<div align="center">

![R](https://img.shields.io/badge/R-4.5-blue?logo=r)
![Shiny](https://img.shields.io/badge/Shiny-1.9-green?logo=r)
![License](https://img.shields.io/badge/License-MIT-yellow)
![Version](https://img.shields.io/badge/Version-2.0-orange)

**An Intelligent Multi-Omics and Machine Learning System for Severe Mental Disorders**

[Features](#-features) • [System Architecture](#-system-architecture) • [Quick Start](#-quick-start) • [Module Description](#-module-description) • [Tech Stack](#-tech-stack)

</div>

---

## 📖 Project Overview

**SMDAP** (Severe Mental Disorder - AI Diagnosis) is an intelligent auxiliary diagnosis and prognosis prediction system for **Schizophrenia** and **Bipolar Disorder**. The system integrates **molecular biology markers** (gene expression, lncRNA, metabolites) with **clinical features** using machine learning algorithms to provide clinicians with disease risk assessment and treatment response predictions.

### Core Values

- 🧬 **Multi-dimensional Data Integration**: Combines gene expression, lncRNA, metabolites, and clinical scale data
- 🤖 **Multi-algorithm Ensemble**: Logistic Regression, Random Forest, SVM, KPLS, and more
- 📊 **High Interpretability**: SHAP value analysis to visualize feature contributions
- 🏥 **Clinically Oriented**: Stratified risk output and auxiliary treatment recommendations

---

## ✨ Key Features

### Bipolar Disorder Assessment

| Module | Function | Data Source | Model |
|--------|----------|-------------|-------|
| Disease Risk Diagnosis | Assess risk based on gene expression profile | Peripheral blood mRNA expression | KPLS / LASSO |
| Prognosis & Treatment Response | Predict treatment response based on clinical features | Clinical scales + psychological assessment | Logistic / RF / SVM |
| Relapse Risk Prediction | Predict relapse risk based on metabolic markers | Metabolomics data | Logistic Regression |

### Schizophrenia Assessment

| Module | Function | Data Source | Model |
|--------|----------|-------------|-------|
| Adolescent Risk Prediction | Early-onset schizophrenia risk assessment | Peripheral blood mRNA expression | Logistic Regression |
| Adult Risk Prediction | Adult schizophrenia risk assessment | Peripheral blood lncRNA expression | Logistic Regression |
| Adult Treatment Response | Predict response to antipsychotic drugs | Pre-treatment lncRNA expression | Logistic Regression |

---

## 🏗️ System Architecture
**SMDAP**
├── app.R                      # Main application entry
├── modules/                   # Functional modules
│   ├── home.R                 # Homepage
│   ├── bipolar_risk.R         # Bipolar - Disease Risk (Gene Data)
│   ├── bipolar_clinical.R     # Bipolar - Treatment Response (Clinical)
│   ├── bipolar_metabolic.R    # Bipolar - Relapse Risk (Metabolites)
│   ├── scz_adolescent.R       # Schizophrenia - Adolescent Risk
│   ├── scz_adult_risk.R       # Schizophrenia - Adult Risk
│   └── scz_treatment.R        # Schizophrenia - Treatment Response
├── models/                    # Pre-trained models
│   ├── final_model.Rdata      # KPLS core model
│   ├── logit_model.rds        # Logistic Regression model
│   ├── randomForest_model.rds # Random Forest model
│   ├── svm_model.rds          # SVM model
│   └── threshold.rds          # Classification thresholds
├── local_data/          # Gene annotation database
│   └── local_gene_mapping.rds # Ensembl gene mapping table
└── data/                      # Sample data
└── sample_data.csv        # Example gene expression data

---
## 🚀 Quick Start
### Requirements
- R 4.0+
- RStudio (recommended)

### Install Dependencies

Run in R console:

```r
# Core packages
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyalert",
  "shinycssloaders",
  "DT",
  "plotly",
  "dplyr",
  "MASS"
))

# Machine learning packages
install.packages(c(
  "randomForest",
  "e1071",
  "glmnet",
  "nnet",
  "pROC",
  "ROCR",
  "epiR"
))

# Gene annotation package
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("biomaRt")
Run download script
source("download_gene_mapping.R")
download_local_gene_mapping()
# In R console
shiny::runApp()
Or click Run App in RStudio.
📋 Module Description
1. Homepage
System overview and navigation to all modules.
2. Bipolar Disorder Assessment
2.1 Disease Risk Diagnosis (Gene Data)

Input: CSV/TSV gene expression file (Ensembl ID + expression values)
Processing: Gene annotation → Normalization → Feature matching
Output: Risk probability + binary classification

2.2 Treatment Response Prediction (Clinical Features)

Input: 16 clinical features (Age, Sex, YMRS, HAMD, GAF, etc.)
Output: Probability of good prognosis + clinical recommendations

2.3 Relapse Risk Prediction (Metabolites)

Input: Acetone, O-acetyl glycoprotein, phosphocholine peak areas
Output: Relapse risk probability

3. Schizophrenia Assessment
3.1 Adolescent Risk Prediction

Input: CCL3, IL1β, CXCL8, CXCL10 mRNA expression
Output: Disease probability + risk level

3.2 Adult Risk Prediction

Input: Gomafu, AK096174, AK123097, ENST000005098041 lncRNA expression
Output: Disease probability + risk level

3.3 Adult Treatment Response Prediction

Input: ENST000005098041, AK123097, uc011dma.1 lncRNA expression
Output: Probability of treatment effectiveness
🛠️ Tech Stack
Framework

Shiny: Web application framework
shinydashboard: Dashboard UI components

Machine Learning

glmnet: LASSO/Ridge regression
randomForest: Random Forest
e1071: SVM
nnet: Neural networks
pROC/ROCR: Model evaluation

Data Processing

biomaRt: Gene annotation
DT: Interactive tables
plotly: Interactive visualizations

Interpretability

SHAP: Feature contribution analysis


📊 Input Data Examples
Gene Expression Format
csvgene_id,expression
ENSG00000141510,15.2
ENSG00000146648,8.7
ENSG00000130203,21.3
Clinical Features (filled via UI form)

Patient ID, Age, Sex, YMRS score, HAMD score, GAF score, etc.


🔬 Model Performance
ModelAUCSensitivitySpecificityKPLS0.890.850.82Logistic Regression0.870.830.80Random Forest0.910.880.85SVM0.880.840.81
Note: Based on internal validation datasets.

⚠️ Disclaimer

This system is intended for clinical auxiliary reference only and should not be used as the sole basis for diagnosis or treatment decisions.
All predictions must be interpreted by qualified psychiatrists in conjunction with face-to-face evaluation, medical history, and clinical scales.
The developers assume no legal responsibility for any medical decisions made based on this system.


📝 Version History

v2.0 (2026-07) — Modular refactoring, full integration of Bipolar and Schizophrenia modules
v1.0 (2025-12) — Initial release with depression relapse risk prediction
👨‍💻 Developers
School of Public Health, Shanxi Medical University
