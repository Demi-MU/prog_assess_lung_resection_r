DATA_RAW <- "data/raw/LungData.xlsx"
DATA_PROCESSED <- "data/processed/lung_clean.rds"
OUTPUT_TABLES <- "output/tables"
OUTPUT_FIGURES <- "output/figures"
MODEL_PATH <- "models/logistic_final.rds"
LOG_PATH <- "logs"

RANDOM_SEED <- 42
TEST_SIZE <- 0.2
TARGET <- "MORTALITY_30D"

CATEGORICAL_FEATURES <- c(
  "PRNCPTX", "Open vs VATS", "Resection", "PODIAG", "PODIAGTX", "TRANST",
  "Age_Group", "SEX", "Race", "ANESTHES", "ATTEND", "Attend vs Resident",
  "Thoracic Surgeon", "SURGSPEC", "BMI_Group", "DIABETES", "SMOKE", "ETOH",
  "DYSPNEA", "DNR", "FNCTSTATUS", "VENTILAT", "HXCOPD", "CPNEUMON", "ASCITES",
  "ESOVAR", "HXCHF", "HXMI", "PRVPCI", "PRVPCS", "HXANGINA", "HYPERMED",
  "HXPVD", "RESTPAIN", "RENAFAIL", "DIALYSIS", "IMPSENS", "COMA", "HEMI",
  "HXTIA", "CVA", "CVANO", "TUMORCNS", "Para", "QUAD", "DISCANCR", "WNDINF",
  "STEROID", "WTLOSS", "BLEEDDIS", "TRANSFUS", "CHEMO", "RADIO", "PrOper30",
  "Leukocytosis", "Anemia", "WNDCLAS", "ASACLAS", "Transfusion", "Wound Infection",
  "Pneumonia", "PULEMBOL", "Prologned/re Intubation", "RENAINSF", "Renal Failure",
  "NEURODEF", "Stroke", "CDARREST", "CDMI", "OTHBLEED", "OTHDVT", "Sepsis/Shock",
  "RETURNOR", "Death"
)

NUMERICAL_FEATURES <- c(
  "CaseID", "CPT", "WORKRVU", "Age", "HEIGHT", "WEIGHT", "BMI", "PACKS",
  "PRSODM", "PRBUN", "PRCREAT", "PRALBUM", "PRBILI", "PRSGOT", "PRALKPH",
  "PRWBC", "PRHCT", "PRPLATE", "PRPTT", "PRINR", "PGY", "RBC", "ANESURG",
  "SURGANE", "DPATRM", "ANETIME", "OPTIME", "TOTSLOS", "DSDtoHD", "DOptoDis",
  "Days to f/u"
)
