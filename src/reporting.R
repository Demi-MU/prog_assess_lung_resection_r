.ensure_parent_dir <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

save_outcome_distribution <- function(y, csv_path, fig_path, title = "Outcome distribution") {
  .ensure_parent_dir(csv_path)
  .ensure_parent_dir(fig_path)

  counts <- as.data.frame(table(class = y), stringsAsFactors = FALSE)
  names(counts) <- c("class", "count")
  counts$proportion <- counts$count / sum(counts$count)
  readr::write_csv(counts, csv_path)

  p <- ggplot2::ggplot(counts, ggplot2::aes(x = class, y = count)) +
    ggplot2::geom_col(fill = "#4C78A8") +
    ggplot2::labs(title = title, x = "Class", y = "Count") +
    ggplot2::theme_minimal()

  ggplot2::ggsave(fig_path, p, width = 5, height = 4, dpi = 200)
}

save_outcome_definition_distribution <- function(
  distribution,
  csv_path,
  fig_path,
  title = "Outcome distribution by definition"
) {
  .ensure_parent_dir(csv_path)
  .ensure_parent_dir(fig_path)

  readr::write_csv(distribution, csv_path)

  plot_df <- distribution
  plot_df$class <- factor(plot_df$class, levels = c("0", "1", "NA"))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = class,
    y = count,
    fill = definition
  )) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = title, x = "Class", y = "Count", fill = "Definition") +
    ggplot2::theme_minimal()

  ggplot2::ggsave(fig_path, p, width = 7, height = 4, dpi = 200)
}

save_missingness_top <- function(
  df, csv_path, fig_path, top_n = 30, title = "Missingness (top variables)"
) {
  .ensure_parent_dir(csv_path)
  .ensure_parent_dir(fig_path)

  miss_rate <- colMeans(is.na(df))
  miss <- data.frame(
    variable = names(miss_rate),
    missing_rate = as.numeric(miss_rate),
    stringsAsFactors = FALSE
  )
  miss <- miss[order(-miss$missing_rate), , drop = FALSE]
  miss <- utils::head(miss, top_n)
  miss$missing_count <- vapply(miss$variable, function(v) sum(is.na(df[[v]])), numeric(1))
  readr::write_csv(miss, csv_path)

  p <- ggplot2::ggplot(miss, ggplot2::aes(
    x = missing_rate,
    y = stats::reorder(variable, missing_rate)
  )) +
    ggplot2::geom_col(fill = "#F58518") +
    ggplot2::labs(title = title, x = "Missing rate", y = NULL) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(fig_path, p, width = 10, height = max(4, 0.28 * nrow(miss)), dpi = 200)
}

bootstrap_auc_ci <- function(y_true, y_proba, n_boot = 2000, seed = 42, alpha = 0.05) {
  set.seed(seed)
  n <- length(y_true)
  aucs <- numeric(0)

  for (i in seq_len(n_boot)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    yt <- y_true[idx]
    yp <- y_proba[idx]
    if (length(unique(yt)) < 2) next
    roc_obj <- pROC::roc(yt, yp, quiet = TRUE)
    aucs <- c(aucs, as.numeric(pROC::auc(roc_obj)))
  }

  if (length(aucs) == 0) return(c(NA_real_, NA_real_))
  stats::quantile(aucs, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
}

youden_threshold <- function(y_true, y_proba) {
  roc_obj <- pROC::roc(y_true, y_proba, quiet = TRUE)
  thr <- pROC::coords(roc_obj, x = "best", best.method = "youden", ret = "threshold")
  as.numeric(unlist(thr, use.names = FALSE))[1]
}

save_roc_plot <- function(y_true, y_proba, fig_path, title = "ROC curve") {
  .ensure_parent_dir(fig_path)

  roc_obj <- pROC::roc(y_true, y_proba, quiet = TRUE)
  auc <- as.numeric(pROC::auc(roc_obj))
  df <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_line(color = "#4C78A8", linewidth = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::labs(title = title, x = "False Positive Rate", y = "True Positive Rate") +
    ggplot2::annotate("text", x = 0.75, y = 0.1, label = sprintf("AUC=%.3f", auc)) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(fig_path, p, width = 5, height = 5, dpi = 200)
  auc
}

save_predictions <- function(X_test, y_test, y_proba, csv_path) {
  .ensure_parent_dir(csv_path)
  out <- data.frame(y_true = as.integer(y_test), y_proba = as.numeric(y_proba))
  readr::write_csv(out, csv_path)
}

save_logistic_coefficients <- function(
  feature_names,
  coef,
  intercept,
  std_error = NULL,
  z_value = NULL,
  p_value = NULL,
  conf_low = NULL,
  conf_high = NULL,
  csv_path,
  forest_fig_path,
  top_n = 30,
  title = "Logistic regression coefficients (OR)"
) {
  .ensure_parent_dir(csv_path)
  .ensure_parent_dir(forest_fig_path)

  align_numeric <- function(x) {
    if (is.null(x)) return(rep(NA_real_, length(feature_names)))
    as.numeric(x)
  }

  std_error <- align_numeric(std_error)
  z_value <- align_numeric(z_value)
  p_value <- align_numeric(p_value)
  conf_low <- align_numeric(conf_low)
  conf_high <- align_numeric(conf_high)

  dfc <- data.frame(
    feature = feature_names,
    coef = as.numeric(coef),
    std_error = std_error,
    z_value = z_value,
    p_value = p_value,
    conf_low_logit = conf_low,
    conf_high_logit = conf_high,
    odds_ratio = exp(as.numeric(coef)),
    or_ci_low = exp(conf_low),
    or_ci_high = exp(conf_high),
    abs_coef = abs(as.numeric(coef)),
    stringsAsFactors = FALSE
  )
  dfc <- dfc[order(-dfc$abs_coef), , drop = FALSE]
  readr::write_csv(dfc, csv_path)

  plot_df <- dfc[
    is.finite(dfc$odds_ratio) &
      is.finite(dfc$or_ci_low) &
      is.finite(dfc$or_ci_high) &
      dfc$odds_ratio > 0 &
      dfc$or_ci_low > 0 &
      dfc$or_ci_high > 0,
    ,
    drop = FALSE
  ]
  plot_df <- plot_df[order(-plot_df$abs_coef), , drop = FALSE]
  plot_df <- utils::head(plot_df, top_n)
  plot_df <- plot_df[order(plot_df$odds_ratio), , drop = FALSE]
  plot_df$feature_plot <- factor(plot_df$feature, levels = plot_df$feature)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = odds_ratio,
    y = feature_plot
  )) +
    ggplot2::geom_segment(
      ggplot2::aes(x = or_ci_low, xend = or_ci_high, y = feature_plot, yend = feature_plot),
      color = "#54A24B",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(color = "#54A24B", size = 2) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(title = title, x = "Odds ratio (log scale)", y = NULL) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    forest_fig_path,
    p,
    width = 10,
    height = max(4, 0.28 * nrow(plot_df)),
    dpi = 200
  )
}

parse_binary_complication <- function(x) {
  x_chr <- toupper(trimws(as.character(x)))
  x_chr[x_chr %in% c("", "NULL", "NA", "N/A", "-99")] <- NA_character_

  yes <- c("Y", "YES", "1", "TRUE", "OCCURRENCE", "COMPLICATION")
  no <- c("N", "NO", "0", "FALSE", "NO COMPLICATION", "NONE")

  out <- rep(NA_integer_, length(x_chr))
  out[x_chr %in% yes] <- 1L
  out[x_chr %in% no] <- 0L
  out[!is.na(x_chr) & is.na(out)] <- 1L
  out
}

save_complication_mortality_association <- function(
  df,
  y,
  complications,
  csv_path
) {
  .ensure_parent_dir(csv_path)

  rows <- lapply(complications, function(complication) {
    if (!(complication %in% names(df))) {
      return(data.frame(
        complication = complication,
        n_with_complication = NA_integer_,
        deaths_with_complication = NA_integer_,
        mortality_rate_with_complication = NA_real_,
        n_without_complication = NA_integer_,
        deaths_without_complication = NA_integer_,
        mortality_rate_without_complication = NA_real_,
        odds_ratio = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_value = NA_real_,
        note = "variable_not_found",
        stringsAsFactors = FALSE
      ))
    }

    x <- parse_binary_complication(df[[complication]])
    complete <- !is.na(x) & !is.na(y)
    x <- x[complete]
    yy <- y[complete]

    if (length(x) == 0 || length(unique(x)) < 2) {
      return(data.frame(
        complication = complication,
        n_with_complication = sum(x == 1L),
        deaths_with_complication = sum(x == 1L & yy == 1L),
        mortality_rate_with_complication = NA_real_,
        n_without_complication = sum(x == 0L),
        deaths_without_complication = sum(x == 0L & yy == 1L),
        mortality_rate_without_complication = NA_real_,
        odds_ratio = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_value = NA_real_,
        note = "insufficient_binary_variation",
        stringsAsFactors = FALSE
      ))
    }

    a <- sum(x == 1L & yy == 1L)
    b <- sum(x == 1L & yy == 0L)
    c <- sum(x == 0L & yy == 1L)
    d <- sum(x == 0L & yy == 0L)
    tab <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
    ft <- stats::fisher.test(tab)

    data.frame(
      complication = complication,
      n_with_complication = a + b,
      deaths_with_complication = a,
      mortality_rate_with_complication = if ((a + b) > 0) a / (a + b) else NA_real_,
      n_without_complication = c + d,
      deaths_without_complication = c,
      mortality_rate_without_complication = if ((c + d) > 0) c / (c + d) else NA_real_,
      odds_ratio = as.numeric(ft$estimate),
      ci_low = as.numeric(ft$conf.int[1]),
      ci_high = as.numeric(ft$conf.int[2]),
      p_value = ft$p.value,
      note = "",
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  readr::write_csv(out, csv_path)
  out
}

save_metrics_summary <- function(
  y_true,
  y_proba,
  csv_path,
  threshold = NULL,
  bootstrap_ci = TRUE,
  seed = 42
) {
  .ensure_parent_dir(csv_path)

  roc_obj <- pROC::roc(y_true, y_proba, quiet = TRUE)
  auc <- as.numeric(pROC::auc(roc_obj))
  ci_low <- NA_real_
  ci_high <- NA_real_
  if (bootstrap_ci) {
    ci <- bootstrap_auc_ci(y_true, y_proba, seed = seed)
    ci_low <- as.numeric(ci[1])
    ci_high <- as.numeric(ci[2])
  }

  brier <- mean((as.numeric(y_true) - as.numeric(y_proba))^2)
  thresholds <- if (is.null(threshold)) {
    c("0.5" = 0.5, "youden" = youden_threshold(y_true, y_proba))
  } else {
    stats::setNames(as.numeric(threshold), "custom")
  }
  thresholds[!is.finite(thresholds) | is.na(thresholds)] <- 0.5

  rows <- lapply(names(thresholds), function(threshold_type) {
    thr <- as.numeric(thresholds[[threshold_type]])
    y_pred <- as.integer(y_proba >= thr)

    cm <- table(
      truth = factor(y_true, levels = c(0, 1)),
      pred = factor(y_pred, levels = c(0, 1))
    )

    data.frame(
      threshold_type = threshold_type,
      threshold = thr,
      tn = as.integer(cm["0", "0"]),
      fp = as.integer(cm["0", "1"]),
      fn = as.integer(cm["1", "0"]),
      tp = as.integer(cm["1", "1"]),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- data.frame(
    auc = auc,
    auc_ci_low = ci_low,
    auc_ci_high = ci_high,
    brier = brier,
    out,
    row.names = NULL
  )

  readr::write_csv(out, csv_path)
  out
}
