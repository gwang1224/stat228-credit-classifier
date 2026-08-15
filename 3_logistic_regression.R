# ---------------------------------------------------------------
# 3. Logistic regression
#
# Three logistic models are compared:
#   (a) AIC stepwise-selected model
#   (b) BIC stepwise-selected model
#   (c) Cost-weighted model (bad-credit applicants weighted 5x,
#       matching the dataset's 5:1 misclassification cost)
#
# Decision thresholds are chosen on the TRAINING data only
# (minimizing expected cost), then all metrics are reported on
# the held-out test set.
# ---------------------------------------------------------------

library(pROC)
source("utils.R")

train.german <- read.csv("data/german_train.csv")
test.german <- read.csv("data/german_test.csv")

fit.logit <- glm(good_bad_credit ~ ., data = train.german, family = binomial)

# Stepwise selection
aic.model <- step(fit.logit, direction = "both", trace = FALSE)
bic.model <- step(fit.logit, direction = "both", k = log(nrow(train.german)), trace = FALSE)

cat("AIC model:", deparse(formula(aic.model)), "\n")
cat("  AIC =", AIC(aic.model), " BIC =", BIC(aic.model),
    " coefficients =", length(coef(aic.model)), "\n\n")
cat("BIC model:", deparse(formula(bic.model)), "\n")
cat("  AIC =", AIC(bic.model), " BIC =", BIC(bic.model),
    " coefficients =", length(coef(bic.model)), "\n\n")

# Cost-weighted logistic regression: weight bad-credit applicants 5x
# so the fit itself reflects the asymmetric misclassification cost.
train.weights <- ifelse(train.german$good_bad_credit == 0, 5, 1)
weighted.model <- glm(formula(aic.model),
  data = train.german, family = binomial, weights = train.weights
)

evaluate_glm <- function(model, label, figure_file) {
  prob.train <- predict(model, newdata = train.german, type = "response")
  prob.test <- predict(model, newdata = test.german, type = "response")

  threshold <- choose_threshold(train.german$good_bad_credit, prob.train)
  plot_threshold_sweep(
    train.german$good_bad_credit, prob.train, threshold,
    main = paste0(label, ": threshold selection (training data)"),
    file = figure_file
  )

  eval <- evaluate_predictions(
    test.german$good_bad_credit,
    ifelse(prob.test > threshold, 1, 0)
  )
  roc.obj <- roc(test.german$good_bad_credit, prob.test,
    levels = c(0, 1), direction = "<", quiet = TRUE
  )

  print(eval$confusion)
  list(row = metrics_row(label, threshold, eval, auc(roc.obj)), roc = roc.obj)
}

aic.res <- evaluate_glm(aic.model, "Logistic (AIC)", "figures/aic_glm_threshold.png")
bic.res <- evaluate_glm(bic.model, "Logistic (BIC)", "figures/bic_glm_threshold.png")
wtd.res <- evaluate_glm(weighted.model, "Logistic (cost-weighted)", "figures/weighted_glm_threshold.png")

# ROC curve for the AIC model on the test set
png("figures/roc_curve_logistic_model.png", width = 2400, height = 1800, res = 300)
plot(aic.res$roc, main = "ROC curve: logistic regression (AIC model)")
legend("bottomright", legend = paste("AUC =", round(auc(aic.res$roc), 3)), bty = "n")
dev.off()

logistic.results <- rbind(aic.res$row, bic.res$row, wtd.res$row)
print(logistic.results)
save_metrics(logistic.results, "logistic_metrics.csv")
