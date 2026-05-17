# Couple LPA Analysis and Latent Profile Prediction

This repository contains reproducible R code and a de-identified public dataset for latent profile analysis (LPA), profile-level group comparisons, BCH-style distal outcome comparisons, and latent profile prediction using repeated cross-validation.

## Repository structure

```text
.
├── R/
│   ├── 01_reproducible_analysis.R      # Main GitHub-ready analysis script
│   └── legacy_original_analysis.R      # Original uploaded script kept for traceability
├── data/
│   └── couple_raw_data_n300_20260219_public.csv
├── figures/                            # Generated figures are saved here
├── results/                            # Generated tables are saved here
├── CODEBOOK.md
├── requirements.R
├── .gitignore
└── README.md
```

## Data note

The public CSV removes the original respondent `ID` column and exact `Age` column. The grouped age variable `Age_2` is retained. Before uploading the repository publicly, confirm that data sharing is consistent with the study consent form, IRB approval, and journal data availability policy.

## How to run

Install the required R packages first:

```r
source("requirements.R")
```

Then run the main script from the repository root:

```r
source("R/01_reproducible_analysis.R")
```

The script reads the CSV from `data/`, writes result tables to `results/`, and saves figures to `figures/`.

## Main analyses

The main script performs descriptive summaries, z-score standardization, correlation heatmap generation, demographic regrouping, 1- to 9-profile LPA model comparison using a varying-variance and zero-covariance specification, extraction of the retained 3-profile solution, class-wise descriptive statistics, ANOVA and Scheffé-style post-hoc comparisons, chi-square tests for demographic differences, BCH-style Wald tests using posterior probabilities, and repeated 5-fold × 20 cross-validation for Elastic Net multinomial regression, XGBoost, and MLP profile prediction.

## Reproducibility

The main script avoids `setwd()` and uses paths relative to the repository root. Random seeds are fixed for the LPA prediction workflow where applicable. Package versions should be recorded with `sessionInfo()` after running the script.

## License

No license has been assigned. Add a license only after deciding how the code and dataset may be reused.
