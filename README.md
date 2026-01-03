# nhanes-adverse-pregnancy-risk-prediction
Survey-weighted NHANES (2017–Mar 2020) analysis testing whether clustered adverse pregnancy history signals early multi-system multimorbidity (≥2 chronic conditions) and PHQ-9 ≥10 among ever-pregnant U.S. women aged 20–44. Reproducible R pipeline + tables/figures.
# NHANES Adverse Pregnancy History & Early Multimorbidity Risk Signal (2017–March 2020)

**Sunday A. Adetunji, MD**  
Department of Biostatistics and Epidemiology, Oregon State University  
Faculty of Clinical Sciences, Obafemi Awolowo University  
Email: adetunjs@oregonstate.edu  
ORCID: https://orcid.org/0000-0001-9321-9957  

---

## Overview

This repository contains a fully reproducible analysis of **NHANES 2017–March 2020 pre-pandemic** data evaluating whether **adverse pregnancy history** (as a clustered life-course risk signal) is associated with:

1. **Early multi-system multimorbidity** (≥2 cardiometabolic–cardiovascular–renal conditions), and  
2. **Moderate-to-severe depressive symptoms** (PHQ-9 ≥10),

among **ever-pregnant U.S. women aged 20–44 years**, using **survey-weighted** methods to produce nationally representative estimates.

### Key exposure (Adverse pregnancy history index)
Binary indicator for **≥1 adverse reproductive feature**, defined using NHANES reproductive and pregnancy history variables:
- Pregnancy loss (history of miscarriage/stillbirth, as available)
- Gestational diabetes
- Macrosomic birth (≥9 lb)
- High parity (≥3 live births)
- Very early (<20 years) or late (≥35 years) age at first birth

### Primary outcome
**Multimorbidity (≥2 conditions)** across:
- Hypertension
- Hyperlipidemia
- Diabetes
- Cardiovascular disease (CVD)
- Kidney disease

### Secondary outcome
**PHQ-9 ≥10** (moderate-to-severe depressive symptoms)

### Core analytic approach
- Descriptive epidemiology with NHANES design features
- Survey-weighted prevalence estimates (with 95% CIs)
- Survey-weighted logistic regression (adjusted odds ratios; aORs)

---

## Repository structure

- `data/`  
  Local folder for NHANES public-use `.XPT` files (downloaded by the user).  
  *Best practice:* do not commit raw data to GitHub; instead, download from CDC/NCHS using the official URLs listed in `data_urls/nhanes_data_urls.txt`.

- `r_scripts/`  
  Analysis scripts (data assembly, derivations, models, tables, figures).  
  - `nhanes_repro_multimorbidity_study1.R` (main pipeline)

- `tables/`  
  Manuscript-ready HTML/PNG outputs (e.g., Table 1–3; supplementary tables).

- `figures/`  
  Manuscript-ready figures (e.g., prevalence plots; forest plot).

---

## Data source (public-use)

Data are from **NHANES 2017–March 2020 pre-pandemic**. NHANES suspended field operations in March 2020; therefore, data collected from 2019 to March 2020 were combined with 2017–2018 to create the nationally representative pre-pandemic files. See CDC/NCHS guidance and release notes for details.

All dataset URLs needed to reproduce this project are listed in:

- `data_urls/nhanes_data_urls.txt`

---

## Reproducibility: Quick start

### 1) Clone the repository
```bash
git clone https://github.com/drsunday-ade/nhanes-adverse-pregnancy-risk-prediction.git
cd nhanes-adverse-pregnancy-risk-prediction
2) Create a local data folder and download NHANES files
Create:

bash
Copy code
mkdir -p data data_urls
Download each .XPT file into data/ using the URLs listed in:

data_urls/nhanes_data_urls.txt

(You can use a browser download or command line tools like curl.)

Example (macOS/Linux):

bash
Copy code
curl -L -o data/P_DEMO.xpt "PASTE_P_DEMO_XPT_URL_HERE"
3) Run the main analysis
From the project root:

bash
Copy code
Rscript r_scripts/nhanes_repro_multimorbidity_study1.R
Expected outputs:

Tables written to tables/

Figures written to figures/

Software environment
R (recommended: ≥ 4.2)

Typical packages used in NHANES survey workflows include:

tidyverse, haven, survey, srvyr

broom, gtsummary, gt

ggplot2

(Exact package calls are in the script.)

Outputs (manuscript-facing)
This repository is structured to generate:

Table 1: Sample characteristics by adverse pregnancy history

Table 2: Survey-weighted adjusted logistic model for multimorbidity

Table 3: Survey-weighted adjusted logistic model for PHQ-9 ≥10

Figure 1: Weighted prevalence of multimorbidity by adverse pregnancy history

Figure 2: Weighted prevalence of PHQ-9 ≥10 by adverse pregnancy history

Figure 3 / Figure S1: Forest plot of adjusted odds ratios for both outcomes

Suggested citation
If you use or adapt this code, please cite:

Adetunji, S. A. (2026). NHANES adverse pregnancy history and early multimorbidity risk signal (NHANES 2017–March 2020) [Code repository]. GitHub.

(Optionally mint a Zenodo DOI for the repository and replace this with the DOI-based citation.)

Disclaimer
This is an independent, academic reproducibility repository. It is not an official CDC/NCHS product. Users are responsible for correct interpretation of NHANES design variables and adherence to NHANES analytic guidance.

