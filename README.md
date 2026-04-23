Prognostic assessment of lung resection surgery (R version)

This repository rewrites the original `prog_assess_lung_resection` workflow in R
with the same project structure and pipeline steps:

- data loading
- mortality label creation and missing-value handling
- feature engineering (BMI, age group, one-hot encoding)
- logistic regression training
- evaluation and diagnostics (ROC, calibration, VIF)
- output tables/figures/model export

## Run

1. Put source data at `data/raw/LungData.xlsx`
2. Install required packages (see `SETUP.md`)
3. Execute:

```r
source("main.R"); main()
```