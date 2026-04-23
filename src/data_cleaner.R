clean_mortality <- function(df) {
  df
}

handle_missing <- function(df, threshold = 0.5) {
  missing_ratio <- colMeans(is.na(df))
  keep_cols <- names(missing_ratio[missing_ratio <= threshold])
  df_clean <- df[, keep_cols, drop = FALSE]

  for (col in names(df_clean)) {
    if (is.numeric(df_clean[[col]]) || is.integer(df_clean[[col]])) {
      med <- stats::median(df_clean[[col]], na.rm = TRUE)
      if (is.na(med)) med <- 0
      df_clean[[col]][is.na(df_clean[[col]])] <- med
    } else {
      non_na <- df_clean[[col]][!is.na(df_clean[[col]])]
      if (length(non_na) == 0) {
        df_clean[[col]][is.na(df_clean[[col]])] <- "Unknown"
      } else {
        mode_val <- names(sort(table(non_na), decreasing = TRUE))[1]
        df_clean[[col]][is.na(df_clean[[col]])] <- mode_val
      }
      df_clean[[col]] <- as.factor(df_clean[[col]])
    }
  }

  df_clean
}

create_mortality_30d <- function(
  df,
  death_day_col = NULL,
  death_day_cols = c("DOpertoD", "DOPTODIS", "DOptoDis", "DOptoD", "DOpertoDis")
) {
  if (is.null(death_day_col)) {
    death_day_col <- intersect(death_day_cols, names(df))[1]
  }
  if (is.na(death_day_col) || !nzchar(death_day_col) || !(death_day_col %in% names(df))) {
    stop(
      "No valid death-day column found. Tried: ",
      paste(death_day_cols, collapse = ", "),
      "\nAvailable columns (first 30): ",
      paste(utils::head(names(df), 30), collapse = ", ")
    )
  }

  doper <- suppressWarnings(as.numeric(df[[death_day_col]]))
  # NA/invalid -> treat as alive (0) to avoid dropping target during missingness filtering.
  df$MORTALITY_30D <- ifelse(!is.na(doper) & doper >= 0 & doper <= 30, 1L, 0L)
  df$MORTALITY_30D <- as.integer(df$MORTALITY_30D)
  df
}
