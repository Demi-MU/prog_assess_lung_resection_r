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
