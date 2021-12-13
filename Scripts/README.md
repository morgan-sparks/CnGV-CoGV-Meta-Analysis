-----
## QAQC

### QAQC GxE visual check.R
This script plots reaction norms for a visual check of the QA/QC plots for CnGV and CoGV in "effect size calculation.R". Figures should not have positive and negative values, they should only be one or the other, positive and negative is indicative of GxE response. As such, those with GxE will be removed from data final data in "effect size calculation.R"

### QAQC.R
This script checks to see if papers in the meta data match papers in the raw data and vice versa.

### analysed _papers_check.R
QAQC script to check that conover papers and web of science check are in the full list (all papers) of analyzed papers

-----
## Generate data files

### effect size calculation.R
Script that takes raw data file and computes different effect size estimate files for different analyses (random effects models, metaregression, temperature and latitude analysis, and compensation analysis. Also has a few built in QAQC checks.

-----
## Meta-analytic models

### Bayesian models 
Scripts are in [model_scripts](https://github.com/morgan-sparks/CnGV-CoGV-Meta-Analysis/tree/main/Scripts/model%20scripts)

### frequentist metareg mod.R
Run the same random effects and metaregression models in frequentist framework with *metafor* package. Results in supplementary material.

-----
## General Analysis and Figures

### Critical Review Map.R
R script to create map in fig. 1

### compensation analysis.R
Script for compensation analysis (Fig. 5)

### elevation_latitude_temp_analysis.R
Script for analysis and figure for elevation and latitude analyses, as well as Fig. 4.

### make phylo tree.R
Script to creat variance-covariance matrices from species lists in the model data files. Prints figure of phylogenetic tree based on data for inclusion in supplementary materials.

### metaregression plots.R
Script to take .rds file from metaregression model and create Fig. 3

### random effect models plots.R
Script to take .rds file from counter- and cogradient random effects models and make Fig. 2

-----
## Supplementary Materials and other additional scripts

### supplementary materials.R
Script to create many supplementary materials not included in the other scripts here

