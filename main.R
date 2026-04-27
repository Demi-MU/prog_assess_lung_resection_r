source("config.R")
source("src/data_loader.R")
source("src/data_cleaner.R")
source("src/feature_engineering.R")
source("src/model_builder.R")
source("src/model_diagnostics.R")
source("src/reporting.R")

ensure_dirs <- function() {
  dirs <- c(
    LOG_PATH,
    dirname(MODEL_PATH),
    OUTPUT_FIGURES,
    OUTPUT_TABLES,
    dirname(DATA_PROCESSED)
  )
  for (d in dirs) {
    if (!is.na(d) && nzchar(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
}

main <- function() {
  ensure_dirs()
  set.seed(RANDOM_SEED)

  df_raw <- load_raw_data(DATA_RAW)
  save_missingness_top(
    df_raw,
    csv_path = file.path(OUTPUT_TABLES, "missingness_top.csv"),
    fig_path = file.path(OUTPUT_FIGURES, "missingness_top.png"),
    top_n = 30,
    title = "Missingness (raw data, top 30)"
  )

  df <- df_raw
  df <- create_mortality_30d(df)
  df <- clean_mortality(df)
  df <- create_bmi(df)
  df <- group_age(df)
  df <- handle_missing(df, threshold = 0.5)

  features <- unique(c(CATEGORICAL_FEATURES, NUMERICAL_FEATURES))
  features <- intersect(features, names(df))
  X <- df[, features, drop = FALSE]
  X <- get_dummies(X, intersect(CATEGORICAL_FEATURES, names(X)))

  if (!(TARGET %in% names(df))) {
    stop(sprintf("Target '%s' is not available in data.", TARGET))
  }
  y <- as.integer(df[[TARGET]])

  save_outcome_distribution(
    y,
    csv_path = file.path(OUTPUT_TABLES, "outcome_distribution.csv"),
    fig_path = file.path(OUTPUT_FIGURES, "outcome_distribution.png"),
    title = sprintf("Outcome distribution (%s)", TARGET)
  )

  split <- split_data(X, y, test_size = TEST_SIZE, random_state = RANDOM_SEED)
  filtered <- drop_problematic_columns(split$X_train, split$y_train, min_nonzero = 5)
  X_train_f <- filtered$X_train
  X_test_f <- apply_column_filter(split$X_test, filtered$keep_cols)
  model <- train_logistic(X_train_f, split$y_train)

  eval <- evaluate_model(model, X_test_f, split$y_test)
  y_proba <- eval$y_proba

  save_predictions(
    X_test = X_test_f,
    y_test = split$y_test,
    y_proba = y_proba,
    csv_path = file.path(OUTPUT_TABLES, "predictions_test.csv")
  )

  save_roc_plot(
    y_true = split$y_test,
    y_proba = y_proba,
    fig_path = file.path(OUTPUT_FIGURES, "roc_test.png"),
    title = "ROC curve (test)"
  )

  save_metrics_summary(
    y_true = split$y_test,
    y_proba = y_proba,
    csv_path = file.path(OUTPUT_TABLES, "metrics_summary_test.csv"),
    bootstrap_ci = TRUE,
    seed = RANDOM_SEED
  )

  coef_vec <- stats::coef(model)
  coef_vec <- coef_vec[!names(coef_vec) %in% "(Intercept)"]
  intercept <- as.numeric(stats::coef(model)[["(Intercept)"]])

  save_logistic_coefficients(
    feature_names = names(coef_vec),
    coef = as.numeric(coef_vec),
    intercept = intercept,
    csv_path = file.path(OUTPUT_TABLES, "logistic_coefficients.csv"),
    forest_fig_path = file.path(OUTPUT_FIGURES, "forest_logistic_top.png"),
    top_n = 30,
    title = "Logistic regression (top 30 by |coef|)"
  )

  calibration_plot(
    split$y_test,
    y_proba,
    file.path(OUTPUT_FIGURES, "calibration.png")
  )

  vif <- vif_test(model)
  readr::write_csv(vif, file.path(OUTPUT_TABLES, "vif.csv"))
  saveRDS(model, MODEL_PATH)
  saveRDS(df, DATA_PROCESSED)

  message(sprintf("Test AUC: %.3f", eval$auc))
  message("Confusion matrix:")
  print(eval$report)
}

if (sys.nframe() == 0) {
  main()
}
