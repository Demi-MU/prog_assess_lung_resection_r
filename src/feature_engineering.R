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
