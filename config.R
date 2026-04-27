DATA_RAW <- "data/raw/LungData.xlsx"
DATA_PROCESSED <- "data/processed/lung_clean.rds"
OUTPUT_TABLES <- "output/tables"
OUTPUT_FIGURES <- "output/figures"
MODEL_PATH <- "models/logistic_final.rds"
FULL_MODEL_PATH <- "models/logistic_full_diagnostic.rds"
LOG_PATH <- "logs"

RANDOM_SEED <- 42
TEST_SIZE <- 0.2
TARGET <- "MORTALITY_30D"

CATEGORICAL_FEATURES <- c(
  "Age_Group", "SEX", "Race",
  "BMI_Group",
  "DIABETES", "SMOKE", "ETOH",
  "DYSPNEA", "DNR", "FNCTSTATUS",
  "HXCOPD", "ASCITES", "ESOVAR",
  "HXCHF", "HXMI", "PRVPCI", "PRVPCS", "HXANGINA", "HYPERMED",
  "HXPVD", "RESTPAIN",
  "RENAFAIL", "DIALYSIS",
  "IMPSENS", "COMA", "HEMI",
  "HXTIA", "CVA", "CVANO", "TUMORCNS", "Para", "QUAD",
  "DISCANCR",
  "STEROID", "WTLOSS", "BLEEDDIS", "TRANSFUS",
  "CHEMO", "RADIO",
  "Leukocytosis", "Anemia",
  "ASACLAS"
)

NUMERICAL_FEATURES <- c(
  "Age", "HEIGHT", "WEIGHT", "BMI", "PACKS",
  "PRSODM",
  "PRBUN", "PRCREAT", "PRALBUM", "PRBILI", "PRSGOT", "PRALKPH",
  "PRWBC", "PRHCT", "PRPLATE", "PRPTT", "PRINR", "RBC"
)

REDUCED_FEATURES <- c(
  "Age",
  "SEX",
  "HXCOPD",
  "Dyspnea_any",
  "Functional_dependence",
  "ASA_simple",
  "DISCANCR",
  "PRALBUM",
  "PRCREAT",
  "PRHCT"
)
