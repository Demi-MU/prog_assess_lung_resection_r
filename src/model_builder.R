split_data <- function(X, y, test_size = 0.2, random_state = 42) {
  set.seed(random_state)
  if (length(unique(stats::na.omit(y))) < 2) {
    stop("Target has <2 classes; cannot train a classifier.")
  }

  idx <- rsample::initial_split(
    data.frame(y = y, row_id = seq_len(nrow(X))),
    prop = 1 - test_size,
    strata = "y"
  )

  train_ids <- rsample::training(idx)$row_id
  test_ids <- rsample::testing(idx)$row_id

  list(
    X_train = X[train_ids, , drop = FALSE],
    X_test = X[test_ids, , drop = FALSE],
    y_train = y[train_ids],
    y_test = y[test_ids]
  )
}

train_logistic <- function(X_train, y_train) {
  train_df <- data.frame(y = y_train, X_train, check.names = FALSE)
  stats::glm(y ~ ., data = train_df, family = stats::binomial())
}

drop_problematic_columns <- function(X_train, y_train, min_nonzero = 5) {
  X <- X_train

  # Drop constant columns
  nunique <- vapply(X, function(col) length(unique(col)), integer(1))
  keep <- nunique > 1
  X <- X[, keep, drop = FALSE]

  # Drop very rare binary indicators (common after one-hot encoding)
  is_binary01 <- vapply(
    X,
    function(col) all(stats::na.omit(unique(col)) %in% c(0, 1)),
    logical(1)
  )
  if (any(is_binary01)) {
    ones <- vapply(X[, is_binary01, drop = FALSE], function(col) sum(col == 1, na.rm = TRUE), numeric(1))
    keep_bin <- ones >= min_nonzero & (nrow(X) - ones) >= min_nonzero
    drop_names <- names(keep_bin)[!keep_bin]
    if (length(drop_names) > 0) {
      X <- X[, setdiff(names(X), drop_names), drop = FALSE]
    }
  }

  # Drop columns that perfectly separate the outcome in training (complete separation)
  sep_drop <- character(0)
  for (nm in names(X)) {
    x <- X[[nm]]
    if (!all(stats::na.omit(unique(x)) %in% c(0, 1))) next
    a11 <- sum(x == 1 & y_train == 1, na.rm = TRUE)
    a10 <- sum(x == 1 & y_train == 0, na.rm = TRUE)
    a01 <- sum(x == 0 & y_train == 1, na.rm = TRUE)
    a00 <- sum(x == 0 & y_train == 0, na.rm = TRUE)
    if ((a11 > 0 && a10 == 0) || (a10 > 0 && a11 == 0) || (a01 > 0 && a00 == 0) || (a00 > 0 && a01 == 0)) {
      sep_drop <- c(sep_drop, nm)
    }
  }
  if (length(sep_drop) > 0) {
    X <- X[, setdiff(names(X), sep_drop), drop = FALSE]
  }

  list(X_train = X, keep_cols = names(X))
}

apply_column_filter <- function(X, keep_cols) {
  keep <- intersect(keep_cols, names(X))
  out <- X[, keep, drop = FALSE]
  # Ensure any missing columns are added back as 0s for safety
  missing <- setdiff(keep_cols, names(out))
  if (length(missing) > 0) {
    for (m in missing) out[[m]] <- 0
    out <- out[, keep_cols, drop = FALSE]
  }
  out
}

evaluate_model <- function(model, X_test, y_test, threshold = 0.5) {
  test_df <- data.frame(X_test, check.names = FALSE)
  y_proba <- as.numeric(stats::predict(model, newdata = test_df, type = "response"))
  y_pred <- as.integer(y_proba >= threshold)
  auc <- as.numeric(pROC::auc(y_test, y_proba))

  cm <- table(
    truth = factor(y_test, levels = c(0, 1)),
    pred = factor(y_pred, levels = c(0, 1))
  )

  list(
    auc = auc,
    report = cm,
    y_proba = y_proba,
    y_pred = y_pred
  )
}
