vif_test <- function(model) {
  out <- tryCatch(
    {
      v <- car::vif(model)
      data.frame(variable = names(v), VIF = as.numeric(v), row.names = NULL)
    },
    error = function(e) {
      data.frame(variable = character(), VIF = numeric())
    }
  )
  out
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
