# ---------------------------------------------------------------
# 6. Support vector machines
#
# Linear, polynomial, and radial-kernel SVMs. Hyperparameters are
# selected with 10-fold cross-validation on the TRAINING set only
# (the test set is never touched during tuning), minimizing the
# expected misclassification COST rather than the raw error rate.
# All kernels use class weights of 5:1 (bad:good) to reflect the
# asymmetric misclassification cost.
# ---------------------------------------------------------------

library(e1071)
library(pROC)
source("utils.R")

train.german <- read.csv("data/german_train.csv")
test.german <- read.csv("data/german_test.csv")

train.german$good_bad_credit <- factor(train.german$good_bad_credit, levels = c(0, 1))
test.german$good_bad_credit <- factor(test.german$good_bad_credit, levels = c(0, 1))

class.weights <- c("0" = 5, "1" = 1)

# Tuning criterion: mean per-observation cost (5 for a bad customer
# classified as good, 1 for a good customer classified as bad)
cost_error <- function(true, predicted) {
  cm <- table(
    factor(true, levels = c(0, 1)),
    factor(predicted, levels = c(0, 1))
  )
  (5 * cm["0", "1"] + 1 * cm["1", "0"]) / length(true)
}

tune_and_fit <- function(kernel, ranges) {
  set.seed(1)
  tune.out <- tune(
    svm,
    good_bad_credit ~ .,
    data = train.german,
    kernel = kernel,
    ranges = ranges,
    class.weights = class.weights,
    tunecontrol = tune.control(sampling = "cross", cross = 10, error.fun = cost_error)
  )
  cat(kernel, "SVM best parameters:\n")
  print(tune.out$best.parameters)

  fit.args <- c(
    list(
      formula = good_bad_credit ~ .,
      data = train.german,
      type = "C-classification",
      kernel = kernel,
      class.weights = class.weights,
      scale = TRUE,
      probability = TRUE
    ),
    as.list(tune.out$best.parameters)
  )
  do.call(svm, fit.args)
}

evaluate_svm <- function(fit, label) {
  pred <- predict(fit, test.german, probability = TRUE)
  prob.good <- attr(pred, "probabilities")[, "1"]

  eval <- evaluate_predictions(test.german$good_bad_credit, pred)
  roc.obj <- roc(test.german$good_bad_credit, prob.good,
    levels = c(0, 1), direction = "<", quiet = TRUE
  )
  print(eval$confusion)
  list(row = metrics_row(label, NA, eval, auc(roc.obj)), roc = roc.obj)
}

cost.range <- list(cost = c(0.1, 1, 5, 10, 100))

svm.linear <- tune_and_fit("linear", cost.range)
svm.poly <- tune_and_fit("polynomial", cost.range)
svm.radial <- tune_and_fit("radial", c(cost.range, list(gamma = 2^seq(-8, 0, by = 2))))

linear.res <- evaluate_svm(svm.linear, "SVM (linear)")
poly.res <- evaluate_svm(svm.poly, "SVM (polynomial)")
radial.res <- evaluate_svm(svm.radial, "SVM (radial)")

# ROC curve for the radial SVM on the test set
png("figures/roc_curve_svm_model.png", width = 2400, height = 1800, res = 300)
plot(radial.res$roc, main = "ROC curve: SVM (radial kernel)")
legend("bottomright", legend = paste("AUC =", round(auc(radial.res$roc), 3)), bty = "n")
dev.off()

svm.results <- rbind(linear.res$row, poly.res$row, radial.res$row)
print(svm.results)
save_metrics(svm.results, "svm_metrics.csv")
