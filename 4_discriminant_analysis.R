# ---------------------------------------------------------------
# 4. Discriminant analysis (LDA and QDA)
#
# Checks LDA/QDA assumptions (marginal normality via QQ plots,
# homogeneity of covariance via Box's M), then fits both models
# on standardized predictors. QDA is also evaluated with a
# cost-minimizing threshold chosen on the training data.
# ---------------------------------------------------------------

library(MASS)
library(pROC)
library(biotools)
source("utils.R")

train <- read.csv("data/german_train.csv")
test <- read.csv("data/german_test.csv")

# Standardize using training means/SDs only
train_x <- scale(train[, -which(names(train) == "good_bad_credit")])
test_x <- scale(test[, -which(names(test) == "good_bad_credit")],
  center = attr(train_x, "scaled:center"),
  scale = attr(train_x, "scaled:scale")
)

train_scaled <- data.frame(train_x, label = factor(train$good_bad_credit, levels = c(0, 1)))
test_scaled <- data.frame(test_x, label = factor(test$good_bad_credit, levels = c(0, 1)))

# --- Assumption checks ------------------------------------------

# Marginal normality of the continuous predictors, by class
png("figures/lda_qda_qq_plots.png", width = 2400, height = 1600, res = 300)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (var in c("age", "credit_amount", "duration")) {
  for (class in c(1, 0)) {
    x <- train[[var]][train$good_bad_credit == class]
    qqnorm(x, main = paste0(var, " (", ifelse(class == 1, "good", "bad"), " credit)"), cex.main = 0.9)
    qqline(x)
  }
}
dev.off()
# Deviations from normality are modest; LDA/QDA are reasonably
# robust to this, so we proceed.

# Homogeneity of covariance across classes (Box's M)
boxm <- boxM(
  train[, -which(names(train) == "good_bad_credit")],
  train$good_bad_credit
)
print(boxm)
# Box's M rejects equal covariance matrices, which favors QDA
# (class-specific covariances) over LDA in principle.

# --- LDA ---------------------------------------------------------

lda_fit <- lda(label ~ ., data = train_scaled)
lda_pred <- predict(lda_fit, newdata = test_scaled)

lda_eval <- evaluate_predictions(test_scaled$label, lda_pred$class)
lda_auc <- auc(roc(test_scaled$label, lda_pred$posterior[, "1"],
  levels = c(0, 1), direction = "<", quiet = TRUE
))
print(lda_eval$confusion)

# --- QDA ---------------------------------------------------------

qda_fit <- qda(label ~ ., data = train_scaled)

qda_prob_train <- predict(qda_fit, newdata = train_scaled)$posterior[, "1"]
qda_prob_test <- predict(qda_fit, newdata = test_scaled)$posterior[, "1"]

# Default 0.5 threshold
qda_eval_default <- evaluate_predictions(
  test_scaled$label, ifelse(qda_prob_test > 0.5, 1, 0)
)

# Cost-minimizing threshold chosen on training data
qda_t <- choose_threshold(train_scaled$label, qda_prob_train)
qda_eval_tuned <- evaluate_predictions(
  test_scaled$label, ifelse(qda_prob_test > qda_t, 1, 0)
)
qda_auc <- auc(roc(test_scaled$label, qda_prob_test,
  levels = c(0, 1), direction = "<", quiet = TRUE
))
print(qda_eval_tuned$confusion)

# --- Results -----------------------------------------------------

discriminant.results <- rbind(
  metrics_row("LDA", 0.5, lda_eval, lda_auc),
  metrics_row("QDA", 0.5, qda_eval_default, qda_auc),
  metrics_row("QDA (cost-tuned)", qda_t, qda_eval_tuned, qda_auc)
)
print(discriminant.results)
save_metrics(discriminant.results, "discriminant_metrics.csv")
