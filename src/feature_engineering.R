create_bmi <- function(df, weight_col = "WEIGHT", height_col = "HEIGHT") {
  if (all(c(weight_col, height_col) %in% names(df))) {
    weight <- suppressWarnings(as.numeric(df[[weight_col]]))
    height <- suppressWarnings(as.numeric(df[[height_col]]))
    df$BMI <- weight / (height^2) * 703
  }
  df
}

group_age <- function(df, age_col = "Age") {
  if (age_col %in% names(df)) {
    age_val <- suppressWarnings(as.numeric(df[[age_col]]))
    df$Age_Group <- cut(
      age_val,
      breaks = c(0, 50, 70, 120),
      labels = c("<50", "50-70", ">70"),
      include.lowest = TRUE
    )
  }
  df
}

create_collapsed_clinical_features <- function(df) {
  if ("DYSPNEA" %in% names(df)) {
    dyspnea <- toupper(trimws(as.character(df$DYSPNEA)))
    dyspnea_any <- rep(NA_character_, length(dyspnea))
    dyspnea_any[dyspnea %in% c("NO", "NONE", "N")] <- "None"
    dyspnea_any[!is.na(dyspnea) & nzchar(dyspnea) & is.na(dyspnea_any)] <- "Any"
    df$Dyspnea_any <- factor(dyspnea_any, levels = c("None", "Any"))
  }

  if ("FNCTSTATUS" %in% names(df)) {
    fn <- toupper(trimws(as.character(df$FNCTSTATUS)))
    dependent <- fn %in% c("PARTIALLY DEPENDENT", "TOTALLY DEPENDENT")
    df$Functional_dependence <- factor(
      ifelse(dependent, "Dependent", "Independent/unknown"),
      levels = c("Independent/unknown", "Dependent")
    )
  }

  if ("ASACLAS" %in% names(df)) {
    asa <- trimws(as.character(df$ASACLAS))
    asa_simple <- rep("1-2/unknown", length(asa))
    asa_simple[grepl("^3", asa)] <- "3"
    asa_simple[grepl("^4", asa)] <- "4"
    df$ASA_simple <- factor(asa_simple, levels = c("1-2/unknown", "3", "4"))
  }

  df
}

get_dummies <- function(df, categorical_list) {
  use_cols <- intersect(categorical_list, names(df))
  if (length(use_cols) == 0) return(df)

  fastDummies::dummy_cols(
    df,
    select_columns = use_cols,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )
}
