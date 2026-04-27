source("config.R")
source("src/data_loader.R")
source("src/data_cleaner.R")
source("src/feature_engineering.R")
source("src/model_builder.R")
source("src/model_diagnostics.R")
source("src/reporting.R")

ensure_dirs <- function() {
  dirs <- unique(c(
    LOG_PATH,
    dirname(MODEL_PATH),
    if (exists("FULL_MODEL_PATH")) dirname(FULL_MODEL_PATH) else NA_character_,
    OUTPUT_FIGURES,
    OUTPUT_TABLES,
    dirname(DATA_PROCESSED)
  ))

  for (d in dirs) {
    if (!is.na(d) && nzchar(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
}

prepare_model_matrix <- function(df, features, categorical_features) {
  features <- intersect(features, names(df))
  X <- df[, features, drop = FALSE]
  X <- get_dummies(X, intersect(categorical_features, names(X)))
  X
}

model_coefficient_summary <- function(model) {
  coef_vec <- stats::coef(model)
  coef_vec <- coef_vec[!names(coef_vec) %in% "(Intercept)"]
  intercept <- as.numeric(stats::coef(model)[["(Intercept)"]])

  coef_summary <- as.data.frame(summary(model)$coefficients)
  coef_summary$feature <- rownames(coef_summary)
  coef_summary <- coef_summary[coef_summary$feature != "(Intercept)", , drop = FALSE]
  coef_summary <- coef_summary[match(names(coef_vec), coef_summary$feature), , drop = FALSE]
  coef_summary$conf_low <- coef_summary$Estimate - 1.96 * coef_summary$`Std. Error`
  coef_summary$conf_high <- coef_summary$Estimate + 1.96 * coef_summary$`Std. Error`

  list(
    feature_names = names(coef_vec),
    coef = as.numeric(coef_vec),
    intercept = intercept,
    std_error = coef_summary$`Std. Error`,
    z_value = coef_summary$`z value`,
    p_value = coef_summary$`Pr(>|z|)`,
    conf_low = coef_summary$conf_low,
    conf_high = coef_summary$conf_high
  )
}

print_confusion_from_metrics <- function(metrics, threshold_type, model_label) {
  row <- metrics[metrics$threshold_type == threshold_type, , drop = FALSE]
  if (nrow(row) == 0) return(invisible(NULL))

  cm <- matrix(
    c(row$tn, row$fp, row$fn, row$tp),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(truth = c("0", "1"), pred = c("0", "1"))
  )
  message(sprintf(
    "%s confusion matrix (%s threshold = %.6f):",
    model_label,
    threshold_type,
    row$threshold
  ))
  print(cm)
}

write_reduced_final_aliases <- function() {
  aliases <- c(
    "model_predictors.csv" = "reduced_model_predictors.csv",
    "predictions_test.csv" = "reduced_predictions_test.csv",
    "metrics_summary_test.csv" = "reduced_metrics_summary_test.csv",
    "logistic_coefficients.csv" = "reduced_logistic_coefficients.csv",
    "vif.csv" = "reduced_vif.csv",
    "model_warnings.csv" = "reduced_model_warnings.csv"
  )

  for (dest in names(aliases)) {
    src <- file.path(OUTPUT_TABLES, aliases[[dest]])
    if (file.exists(src)) {
      file.copy(src, file.path(OUTPUT_TABLES, dest), overwrite = TRUE)
    }
  }

  fig_aliases <- c(
    "roc_test.png" = "reduced_roc_test.png",
    "calibration.png" = "reduced_calibration.png",
    "forest_logistic_top.png" = "reduced_forest_logistic_top.png"
  )

  for (dest in names(fig_aliases)) {
    src <- file.path(OUTPUT_FIGURES, fig_aliases[[dest]])
    if (file.exists(src)) {
      file.copy(src, file.path(OUTPUT_FIGURES, dest), overwrite = TRUE)
    }
  }
}

run_logistic_analysis <- function(
  model_label,
  X,
  y,
  filter_problematic = TRUE,
  model_path = NULL,
  top_n = 30
) {
  split <- split_data(X, y, test_size = TEST_SIZE, random_state = RANDOM_SEED)

  if (filter_problematic) {
    filtered <- drop_problematic_columns(split$X_train, split$y_train, min_nonzero = 5)
    X_train <- filtered$X_train
    X_test <- apply_column_filter(split$X_test, filtered$keep_cols)
  } else {
    X_train <- split$X_train
    X_test <- split$X_test
  }

  model_warnings <- character(0)
  model <- withCallingHandlers(
    train_logistic(X_train, split$y_train),
    warning = function(w) {
      model_warnings <<- c(model_warnings, conditionMessage(w))
    }
  )
  eval <- evaluate_model(model, X_test, split$y_test, threshold = 0.5)
  y_proba <- eval$y_proba
  tag <- tolower(model_label)

  readr::write_csv(
    data.frame(predictor = names(X_train), stringsAsFactors = FALSE),
    file.path(OUTPUT_TABLES, sprintf("%s_model_predictors.csv", tag))
  )
  readr::write_csv(
    data.frame(warning = unique(model_warnings), stringsAsFactors = FALSE),
    file.path(OUTPUT_TABLES, sprintf("%s_model_warnings.csv", tag))
  )

  save_predictions(
    X_test = X_test,
    y_test = split$y_test,
    y_proba = y_proba,
    csv_path = file.path(OUTPUT_TABLES, sprintf("%s_predictions_test.csv", tag))
  )

  save_roc_plot(
    y_true = split$y_test,
    y_proba = y_proba,
    fig_path = file.path(OUTPUT_FIGURES, sprintf("%s_roc_test.png", tag)),
    title = sprintf("ROC curve (%s model, test)", model_label)
  )

  metrics <- save_metrics_summary(
    y_true = split$y_test,
    y_proba = y_proba,
    csv_path = file.path(OUTPUT_TABLES, sprintf("%s_metrics_summary_test.csv", tag)),
    bootstrap_ci = TRUE,
    seed = RANDOM_SEED
  )

  coef_info <- model_coefficient_summary(model)
  save_logistic_coefficients(
    feature_names = coef_info$feature_names,
    coef = coef_info$coef,
    intercept = coef_info$intercept,
    std_error = coef_info$std_error,
    z_value = coef_info$z_value,
    p_value = coef_info$p_value,
    conf_low = coef_info$conf_low,
    conf_high = coef_info$conf_high,
    csv_path = file.path(OUTPUT_TABLES, sprintf("%s_logistic_coefficients.csv", tag)),
    forest_fig_path = file.path(OUTPUT_FIGURES, sprintf("%s_forest_logistic_top.png", tag)),
    top_n = top_n,
    title = sprintf("Logistic regression (%s model)", model_label)
  )

  calibration_plot(
    split$y_test,
    y_proba,
    file.path(OUTPUT_FIGURES, sprintf("%s_calibration.png", tag))
  )

  vif <- vif_test(model)
  readr::write_csv(vif, file.path(OUTPUT_TABLES, sprintf("%s_vif.csv", tag)))

  if (!is.null(model_path)) {
    saveRDS(model, model_path)
  }

  message(sprintf("%s model test AUC: %.3f", model_label, eval$auc))
  print_confusion_from_metrics(metrics, "0.5", model_label)
  print_confusion_from_metrics(metrics, "youden", model_label)

  list(
    model = model,
    metrics = metrics,
    vif = vif,
    predictors = names(X_train),
    warnings = unique(model_warnings),
    split = split,
    y_proba = y_proba
  )
}

fit_reduced_allcases_model <- function(X, y) {
  model_warnings <- character(0)
  model <- withCallingHandlers(
    train_logistic(X, y),
    warning = function(w) {
      model_warnings <<- c(model_warnings, conditionMessage(w))
    }
  )

  coef_info <- model_coefficient_summary(model)
  save_logistic_coefficients(
    feature_names = coef_info$feature_names,
    coef = coef_info$coef,
    intercept = coef_info$intercept,
    std_error = coef_info$std_error,
    z_value = coef_info$z_value,
    p_value = coef_info$p_value,
    conf_low = coef_info$conf_low,
    conf_high = coef_info$conf_high,
    csv_path = file.path(OUTPUT_TABLES, "reduced_logistic_coefficients_allcases.csv"),
    forest_fig_path = file.path(OUTPUT_FIGURES, "reduced_forest_logistic_allcases.png"),
    top_n = 30,
    title = "Logistic regression (reduced model, all cases)"
  )

  readr::write_csv(
    data.frame(warning = unique(model_warnings), stringsAsFactors = FALSE),
    file.path(OUTPUT_TABLES, "reduced_allcases_model_warnings.csv")
  )

  list(model = model, warnings = unique(model_warnings))
}

main <- function() {
  ensure_dirs()
  set.seed(RANDOM_SEED)

  df_raw <- load_raw_data(DATA_RAW)
  df_raw <- standardize_missing_values(df_raw)
  df_raw <- drop_noncase_summary_rows(df_raw)

  save_missingness_top(
    df_raw,
    csv_path = file.path(OUTPUT_TABLES, "missingness_top.csv"),
    fig_path = file.path(OUTPUT_FIGURES, "missingness_top.png"),
    top_n = 30,
    title = "Missingness (case-level raw data, top 30)"
  )

  df <- df_raw
  df <- create_mortality_30d(df, target = TARGET)

  outcome_audit <- create_outcome_audit_table(df, target = TARGET)
  readr::write_csv(outcome_audit, file.path(OUTPUT_TABLES, "outcome_audit.csv"))

  outcome_definitions <- create_outcome_definition_distribution(df)
  save_outcome_definition_distribution(
    outcome_definitions,
    csv_path = file.path(OUTPUT_TABLES, "outcome_definition_distribution.csv"),
    fig_path = file.path(OUTPUT_FIGURES, "outcome_definition_distribution.png"),
    title = "Strict vs Death-primary outcome distribution"
  )

  df <- clean_mortality(df, TARGET)
  df_case_level <- df
  df <- create_bmi(df)
  df <- group_age(df)
  df <- create_collapsed_clinical_features(df)
  df <- handle_missing(df, threshold = 0.5)

  if (!(TARGET %in% names(df))) {
    stop(sprintf("Target '%s' is not available in data.", TARGET))
  }
  y <- as.integer(df[[TARGET]])

  save_outcome_distribution(
    y,
    csv_path = file.path(OUTPUT_TABLES, "outcome_distribution.csv"),
    fig_path = file.path(OUTPUT_FIGURES, "outcome_distribution.png"),
    title = sprintf("Death-primary outcome distribution (%s)", TARGET)
  )

  full_features <- unique(c(CATEGORICAL_FEATURES, NUMERICAL_FEATURES))
  X_full <- prepare_model_matrix(df, full_features, CATEGORICAL_FEATURES)
  full_results <- run_logistic_analysis(
    model_label = "full",
    X = X_full,
    y = y,
    filter_problematic = TRUE,
    model_path = FULL_MODEL_PATH,
    top_n = 30
  )

  missing_reduced <- setdiff(REDUCED_FEATURES, names(df))
  if (length(missing_reduced) > 0) {
    stop(sprintf(
      "Reduced model feature(s) missing after preprocessing: %s",
      paste(missing_reduced, collapse = ", ")
    ))
  }

  reduced_categorical <- c(
    "SEX",
    "HXCOPD",
    "Dyspnea_any",
    "Functional_dependence",
    "ASA_simple",
    "DISCANCR"
  )
  X_reduced <- prepare_model_matrix(df, REDUCED_FEATURES, reduced_categorical)
  reduced_results <- run_logistic_analysis(
    model_label = "reduced",
    X = X_reduced,
    y = y,
    filter_problematic = FALSE,
    model_path = MODEL_PATH,
    top_n = 30
  )

  reduced_allcases_results <- fit_reduced_allcases_model(X_reduced, y)

  complication_vars <- c(
    "Wound Infection", "Pneumonia", "PULEMBOL", "Prologned/re Intubation",
    "RENAINSF", "Renal Failure", "Stroke", "CDARREST", "CDMI", "OTHBLEED",
    "OTHDVT", "Sepsis/Shock", "RETURNOR"
  )
  complication_association <- save_complication_mortality_association(
    df = df_case_level,
    y = as.integer(df_case_level[[TARGET]]),
    complications = complication_vars,
    csv_path = file.path(OUTPUT_TABLES, "complication_mortality_association.csv")
  )

  write_reduced_final_aliases()

  saveRDS(df, DATA_PROCESSED)

  invisible(list(
    full = full_results,
    reduced = reduced_results,
    reduced_allcases = reduced_allcases_results,
    complication_association = complication_association,
    outcome_definitions = outcome_definitions,
    outcome_audit = outcome_audit
  ))
}

if (sys.nframe() == 0) {
  main()
}
