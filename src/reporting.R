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
    aucs <- c(aucs, as.numeric(pROC::auc(yt, yp)))
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
  csv_path,
  forest_fig_path,
  top_n = 30,
  title = "Logistic regression coefficients (OR)"
) {
  .ensure_parent_dir(csv_path)
  .ensure_parent_dir(forest_fig_path)

  dfc <- data.frame(
    feature = feature_names,
    coef = as.numeric(coef),
    odds_ratio = exp(as.numeric(coef)),
    abs_coef = abs(as.numeric(coef)),
    stringsAsFactors = FALSE
  )
  dfc <- dfc[order(-dfc$abs_coef), , drop = FALSE]
  readr::write_csv(dfc, csv_path)

  plot_df <- utils::head(dfc, top_n)
  plot_df <- plot_df[order(plot_df$odds_ratio), , drop = FALSE]

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = odds_ratio,
    y = stats::reorder(feature, odds_ratio)
  )) +
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

save_metrics_summary <- function(
  y_true,
  y_proba,
  csv_path,
  threshold = NULL,
  bootstrap_ci = TRUE,
  seed = 42
) {
  .ensure_parent_dir(csv_path)

  auc <- as.numeric(pROC::auc(y_true, y_proba))
  ci_low <- NA_real_
  ci_high <- NA_real_
  if (bootstrap_ci) {
    ci <- bootstrap_auc_ci(y_true, y_proba, seed = seed)
    ci_low <- as.numeric(ci[1])
    ci_high <- as.numeric(ci[2])
  }

  brier <- mean((as.numeric(y_true) - as.numeric(y_proba))^2)
  thr <- if (is.null(threshold)) youden_threshold(y_true, y_proba) else as.numeric(threshold)
  if (is.na(thr) || !is.finite(thr)) thr <- 0.5
  y_pred <- as.integer(y_proba >= thr)

  cm <- table(
    truth = factor(y_true, levels = c(0, 1)),
    pred = factor(y_pred, levels = c(0, 1))
  )
  tn <- as.integer(cm["0", "0"])
  fp <- as.integer(cm["0", "1"])
  fn <- as.integer(cm["1", "0"])
  tp <- as.integer(cm["1", "1"])

  out <- data.frame(
    auc = auc,
    auc_ci_low = ci_low,
    auc_ci_high = ci_high,
    brier = brier,
    threshold = thr,
    tn = tn,
    fp = fp,
    fn = fn,
    tp = tp
  )
  readr::write_csv(out, csv_path)
  out
}
