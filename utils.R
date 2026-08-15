# ---------------------------------------------------------------
# Shared evaluation helpers
#
# Cost convention (from the STATLOG German Credit documentation):
#   - Classifying a BAD customer as GOOD costs 5
#   - Classifying a GOOD customer as BAD costs 1
#
# The positive class is 1 = good credit, so a false positive
# (predicting good for a bad customer) carries the cost of 5.
# ---------------------------------------------------------------

# Confusion-matrix metrics for 0/1 labels
evaluate_predictions <- function(actual, predicted) {
  actual <- factor(actual, levels = c(0, 1))
  predicted <- factor(predicted, levels = c(0, 1))
  cm <- table(Actual = actual, Predicted = predicted)

  TN <- cm["0", "0"]
  FP <- cm["0", "1"]
  FN <- cm["1", "0"]
  TP <- cm["1", "1"]

  list(
    confusion = cm,
    misclassification = (FP + FN) / sum(cm),
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    cost = 5 * FP + 1 * FN
  )
}

# Pick the probability threshold that minimizes expected misclassification
# cost. Thresholds are always selected on training data (never the test set)
# to avoid leakage.
choose_threshold <- function(actual, prob, thresholds = seq(0.01, 0.99, by = 0.01)) {
  costs <- sapply(thresholds, function(t) {
    evaluate_predictions(actual, ifelse(prob > t, 1, 0))$cost
  })
  thresholds[which.min(costs)]
}

# One row of the model-comparison table
metrics_row <- function(model, threshold, eval, auc = NA) {
  data.frame(
    Model = model,
    Threshold = threshold,
    Misclassification = round(eval$misclassification, 3),
    Sensitivity = round(eval$sensitivity, 3),
    Specificity = round(eval$specificity, 3),
    Cost = eval$cost,
    AUC = ifelse(is.na(auc), NA, round(as.numeric(auc), 3))
  )
}

save_metrics <- function(df, filename) {
  dir.create("results", showWarnings = FALSE)
  write.csv(df, file.path("results", filename), row.names = FALSE)
}

# Sensitivity / specificity / cost as a function of the decision threshold
plot_threshold_sweep <- function(actual, prob, chosen, main, file) {
  thresholds <- seq(0.01, 0.99, by = 0.01)
  metrics <- lapply(thresholds, function(t) {
    evaluate_predictions(actual, ifelse(prob > t, 1, 0))
  })
  sens <- sapply(metrics, `[[`, "sensitivity")
  spec <- sapply(metrics, `[[`, "specificity")
  cost <- sapply(metrics, `[[`, "cost")

  png(file, width = 2400, height = 1800, res = 300)
  par(mar = c(5, 5, 4, 6) + 0.1)
  plot(thresholds, sens,
    type = "l", lty = 1, ylim = c(0, 1),
    xlab = "Threshold", ylab = "Sensitivity / Specificity", main = main
  )
  lines(thresholds, spec, lty = 2)
  abline(v = chosen, col = "red")

  par(new = TRUE)
  plot(thresholds, cost, type = "l", lty = 3, axes = FALSE, xlab = "", ylab = "")
  axis(side = 4)
  mtext("Cost", side = 4, line = 3)

  legend("bottomright",
    lty = c(1, 2, 3, 1), col = c("black", "black", "black", "red"),
    legend = c("Sensitivity", "Specificity", "Cost", "Chosen threshold")
  )
  dev.off()
}
