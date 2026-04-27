standardize_missing_values <- function(df) {
  sentinel_values <- c("", "-99", "NULL", "NA", "N/A")

  for (col in names(df)) {
    x <- df[[col]]

    if (is.numeric(x) || is.integer(x)) {
      x[x == -99] <- NA
      df[[col]] <- x
      next
    }

    if (is.character(x) || is.factor(x)) {
      x_chr <- trimws(as.character(x))
      is_sentinel <- toupper(x_chr) %in% sentinel_values
      x_chr[is_sentinel] <- NA_character_
      df[[col]] <- x_chr
    }
  }

  df
}

drop_noncase_summary_rows <- function(df, caseid_col = "CaseID") {
  if (!(caseid_col %in% names(df))) {
    return(df)
  }

  case_id <- suppressWarnings(as.numeric(as.character(df[[caseid_col]])))
  noncase <- !is.na(case_id) & case_id == 0
  if (any(noncase)) {
    message(sprintf(
      "Dropping %d non-case summary row(s) with %s = 0.",
      sum(noncase),
      caseid_col
    ))
  }

  df[!noncase, , drop = FALSE]
}

.parse_death_status <- function(death) {
  death_chr <- toupper(trimws(as.character(death)))
  death_chr[death_chr %in% c("", "NULL", "NA", "N/A", "-99")] <- NA_character_

  out <- rep(NA_integer_, length(death_chr))

  yes_labels <- c("Y", "YES", "DIED", "DEAD", "DEATH", "1", "TRUE")
  no_labels <- c("N", "NO", "ALIVE", "0", "FALSE")

  out[death_chr %in% yes_labels] <- 1L
  out[death_chr %in% no_labels] <- 0L
  out
}

.first_existing_col <- function(df, candidates) {
  col <- intersect(candidates, names(df))[1]
  if (is.na(col) || !nzchar(col)) NA_character_ else col
}

.strict_mortality_30d <- function(death_status, days_to_death) {
  strict <- rep(NA_integer_, length(death_status))
  strict[death_status == 0L] <- 0L
  strict[
    death_status == 1L &
      !is.na(days_to_death) &
      days_to_death >= 0 &
      days_to_death <= 30
  ] <- 1L
  strict[
    death_status == 1L &
      !is.na(days_to_death) &
      days_to_death > 30
  ] <- 0L
  strict
}

.mortality_qc_flag <- function(death_status, days_to_death) {
  flag <- rep("death_status_unparsed", length(death_status))
  flag[death_status == 0L & is.na(days_to_death)] <- "alive_no_death_day"
  flag[death_status == 0L & !is.na(days_to_death)] <- "alive_with_death_day"
  flag[
    death_status == 1L &
      !is.na(days_to_death) &
      days_to_death >= 0 &
      days_to_death <= 30
  ] <- "death_day_0_30"
  flag[
    death_status == 1L &
      !is.na(days_to_death) &
      days_to_death > 30
  ] <- "death_day_gt_30"
  flag[death_status == 1L & is.na(days_to_death)] <- "death_day_missing"
  flag[
    death_status == 1L &
      !is.na(days_to_death) &
      days_to_death < 0
  ] <- "death_day_negative"
  flag
}

clean_mortality <- function(df, target = "MORTALITY_30D") {
  if (!(target %in% names(df))) {
    stop(sprintf("Target '%s' is not available in data.", target))
  }

  y <- suppressWarnings(as.integer(as.character(df[[target]])))
  invalid <- is.na(y) | !(y %in% c(0L, 1L))
  if (any(invalid)) {
    message(sprintf(
      "Dropping %d rows with missing or invalid %s.",
      sum(invalid),
      target
    ))
  }

  df <- df[!invalid, , drop = FALSE]
  df[[target]] <- y[!invalid]
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
  target = "MORTALITY_30D",
  death_col = "Death",
  death_day_col = NULL,
  death_day_cols = c("DOpertoD", "DOPTODIS", "DOptoDis", "DOptoD", "DOpertoDis")
) {
  if (!(death_col %in% names(df))) {
    stop(sprintf("Required outcome source column '%s' was not found in the raw data.", death_col))
  }

  death_status <- .parse_death_status(df[[death_col]])

  if (is.null(death_day_col)) {
    death_day_col <- .first_existing_col(df, death_day_cols)
  }

  if (!is.na(death_day_col) && death_day_col %in% names(df)) {
    days_to_death <- suppressWarnings(as.numeric(df[[death_day_col]]))
  } else {
    message("No valid death-day column found; using Death column for outcome and QC.")
    days_to_death <- rep(NA_real_, nrow(df))
  }

  df[[target]] <- death_status
  df
}

create_outcome_audit_table <- function(
  df,
  target = "MORTALITY_30D",
  death_col = "Death",
  death_day_col = "DOpertoD",
  discharge_day_col = "DOptoDis"
) {
  if (!(death_col %in% names(df))) {
    stop(sprintf("Required outcome source column '%s' was not found.", death_col))
  }

  death_status <- .parse_death_status(df[[death_col]])
  days_to_death <- if (death_day_col %in% names(df)) {
    suppressWarnings(as.numeric(df[[death_day_col]]))
  } else {
    rep(NA_real_, nrow(df))
  }

  out <- data.frame(
    CaseID = if ("CaseID" %in% names(df)) df$CaseID else NA,
    Death = df[[death_col]],
    DOpertoD = if (death_day_col %in% names(df)) df[[death_day_col]] else NA,
    DOptoDis = if (discharge_day_col %in% names(df)) df[[discharge_day_col]] else NA,
    MORTALITY_30D = if (target %in% names(df)) df[[target]] else death_status,
    qc_flag = .mortality_qc_flag(death_status, days_to_death),
    stringsAsFactors = FALSE
  )

  out
}

create_outcome_definition_distribution <- function(
  df,
  death_col = "Death",
  death_day_col = "DOpertoD"
) {
  if (!(death_col %in% names(df))) {
    stop(sprintf("Required outcome source column '%s' was not found.", death_col))
  }

  death_status <- .parse_death_status(df[[death_col]])
  days_to_death <- if (death_day_col %in% names(df)) {
    suppressWarnings(as.numeric(df[[death_day_col]]))
  } else {
    rep(NA_real_, nrow(df))
  }
  strict <- .strict_mortality_30d(death_status, days_to_death)

  build_counts <- function(values, definition) {
    class <- ifelse(is.na(values), "NA", as.character(values))
    counts <- as.data.frame(table(class = class), stringsAsFactors = FALSE)
    names(counts) <- c("class", "count")
    counts$definition <- definition
    counts$proportion <- counts$count / sum(counts$count)
    counts[, c("definition", "class", "count", "proportion")]
  }

  rbind(
    build_counts(strict, "strict_death_and_DOpertoD_0_30"),
    build_counts(death_status, "death_primary")
  )
}
