# ---------------------------------------------------------------
# 5. Tree-based models
#
# (a) Classification tree, pruned via cross-validated
#     misclassification error
# (b) Random forest (500 trees)
#
# Decision thresholds are chosen to minimize expected cost using
# training-side probabilities only: in-sample probabilities for the
# tree and out-of-bag probabilities for the random forest. Metrics
# are reported on the held-out test set.
# ---------------------------------------------------------------

library(tree)
library(randomForest)
library(pROC)
source("utils.R")

train <- read.csv("data/german_train.csv")
test <- read.csv("data/german_test.csv")

train$good_bad_credit <- factor(train$good_bad_credit, levels = c(0, 1))
test$good_bad_credit <- factor(test$good_bad_credit, levels = c(0, 1))

# --- Classification tree ----------------------------------------

fit <- tree(good_bad_credit ~ ., data = train)

set.seed(1)
cv_fit <- cv.tree(fit, FUN = prune.misclass)
best_size <- cv_fit$size[which.min(cv_fit$dev)]
pruned_tree <- prune.misclass(fit, best = best_size)

png("figures/tree_size_vs_error.png", width = 2400, height = 1800, res = 300)
plot(cv_fit$size, cv_fit$dev,
  type = "b",
  xlab = "Number of terminal nodes",
  ylab = "Cross-validated misclassification error",
  main = "Tree size vs. cross-validated error"
)
abline(v = best_size, lty = 2, col = "red")
dev.off()

png("figures/pruned_tree.png", width = 2400, height = 1800, res = 300)
plot(pruned_tree)
text(pruned_tree, pretty = 0, cex = 0.8)
title(paste("Pruned classification tree (", best_size, "terminal nodes )"))
dev.off()

tree_prob_train <- predict(pruned_tree, newdata = train)[, "1"]
tree_prob_test <- predict(pruned_tree, newdata = test)[, "1"]

tree_eval_default <- evaluate_predictions(
  test$good_bad_credit, ifelse(tree_prob_test > 0.5, 1, 0)
)

tree_t <- choose_threshold(train$good_bad_credit, tree_prob_train)
tree_eval_tuned <- evaluate_predictions(
  test$good_bad_credit, ifelse(tree_prob_test > tree_t, 1, 0)
)

roc_tree <- roc(test$good_bad_credit, tree_prob_test,
  levels = c(0, 1), direction = "<", quiet = TRUE
)
print(tree_eval_tuned$confusion)

# --- Random forest ----------------------------------------------

set.seed(1)
rf_fit <- randomForest(good_bad_credit ~ ., data = train, ntree = 500)

# Out-of-bag probabilities (no newdata) for unbiased threshold selection
rf_prob_oob <- predict(rf_fit, type = "prob")[, "1"]
rf_prob_test <- predict(rf_fit, newdata = test, type = "prob")[, "1"]

rf_eval_default <- evaluate_predictions(
  test$good_bad_credit, ifelse(rf_prob_test > 0.5, 1, 0)
)

rf_t <- choose_threshold(train$good_bad_credit, rf_prob_oob)
rf_eval_tuned <- evaluate_predictions(
  test$good_bad_credit, ifelse(rf_prob_test > rf_t, 1, 0)
)

roc_rf <- roc(test$good_bad_credit, rf_prob_test,
  levels = c(0, 1), direction = "<", quiet = TRUE
)
print(rf_eval_tuned$confusion)

# --- ROC comparison ----------------------------------------------

png("figures/roc_curves_tree_rf.png", width = 2400, height = 1800, res = 300)
plot(roc_rf, col = "red", lwd = 2, main = "ROC curves: tree vs. random forest")
plot(roc_tree, col = "darkgreen", lwd = 2, add = TRUE)
legend("bottomright",
  legend = c(
    paste("Random forest AUC =", round(auc(roc_rf), 3)),
    paste("Pruned tree AUC =", round(auc(roc_tree), 3))
  ),
  col = c("red", "darkgreen"), lwd = 2
)
dev.off()

# --- Results -----------------------------------------------------

tree.results <- rbind(
  metrics_row("Classification tree", 0.5, tree_eval_default, auc(roc_tree)),
  metrics_row("Classification tree (cost-tuned)", tree_t, tree_eval_tuned, auc(roc_tree)),
  metrics_row("Random forest", 0.5, rf_eval_default, auc(roc_rf)),
  metrics_row("Random forest (cost-tuned)", rf_t, rf_eval_tuned, auc(roc_rf))
)
print(tree.results)
save_metrics(tree.results, "tree_metrics.csv")
