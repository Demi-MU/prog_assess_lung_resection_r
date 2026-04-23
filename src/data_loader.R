load_raw_data <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("Raw data file not found: %s", filepath))
  }

  df <- readxl::read_excel(filepath)
  message(sprintf("Loaded raw data: %d rows x %d cols", nrow(df), ncol(df)))
  as.data.frame(df)
}
