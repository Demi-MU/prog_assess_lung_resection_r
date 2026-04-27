vif_test <- function(model) {
  car_out <- tryCatch(
    {
      v <- car::vif(model)
      if (is.matrix(v)) {
        vif_col <- grep("GVIF\\^", colnames(v), value = TRUE)
        if (length(vif_col) == 0) {
          vif_adj <- v[, "GVIF"]^(1 / (2 * v[, "Df"]))
        } else {
          vif_adj <- v[, vif_col[1]]
        }
        data.frame(
          variable = rownames(v),
          GVIF = as.numeric(v[, "GVIF"]),
          Df = as.numeric(v[, "Df"]),
          VIF = as.numeric(vif_adj),
          row.names = NULL
        )
      } else {
        data.frame(variable = names(v), VIF = as.numeric(v), row.names = NULL)
      }
    },
    error = function(e) NULL
  )

  if (!is.null(car_out) && nrow(car_out) > 0) {
    return(car_out[order(-car_out$VIF), , drop = FALSE])
  }

  mm <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  if (is.null(mm) || ncol(mm) <= 2) {
    return(data.frame(variable = character(), VIF = numeric()))
  }

  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  mm <- mm[stats::complete.cases(mm), , drop = FALSE]
  if (nrow(mm) < 3 || ncol(mm) < 2) {
    return(data.frame(variable = character(), VIF = numeric()))
  }

  variance <- apply(mm, 2, stats::var)
  mm <- mm[, is.finite(variance) & variance > 0, drop = FALSE]
  if (ncol(mm) < 2) {
    return(data.frame(variable = character(), VIF = numeric()))
  }

  values <- numeric(ncol(mm))
  for (j in seq_len(ncol(mm))) {
    y <- mm[, j]
    x <- mm[, -j, drop = FALSE]
    fit <- tryCatch(
      stats::lm.fit(x = cbind(`(Intercept)` = 1, x), y = y),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      values[j] <- NA_real_
      next
    }

    rss <- sum(fit$residuals^2, na.rm = TRUE)
    tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    if (!is.finite(tss) || tss <= 0) {
      values[j] <- NA_real_
      next
    }

    r_squared <- 1 - rss / tss
    r_squared <- min(max(r_squared, 0), 1)
    values[j] <- if (r_squared >= 1) Inf else 1 / (1 - r_squared)
  }

  out <- data.frame(
    variable = colnames(mm),
    VIF = values,
    row.names = NULL
  )
  out[order(-out$VIF), , drop = FALSE]
}

calibration_plot <- function(y_true, y_proba, output_path, bins = 10) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  df <- data.frame(y_true = as.numeric(y_true), y_proba = as.numeric(y_proba))
  breaks <- seq(0, 1, length.out = bins + 1)
  df$bin <- cut(df$y_proba, breaks = breaks, include.lowest = TRUE)

  cal <- stats::aggregate(
    cbind(y_true, y_proba) ~ bin,
    data = df,
    FUN = mean
  )

  p <- ggplot2::ggplot(cal, ggplot2::aes(x = y_proba, y = y_true)) +
    ggplot2::geom_line(color = "#4C78A8") +
    ggplot2::geom_point(color = "#4C78A8") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = "Predicted Probability",
      y = "Observed Proportion",
      title = "Calibration Plot"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(output_path, p, width = 5, height = 5, dpi = 200)
}
