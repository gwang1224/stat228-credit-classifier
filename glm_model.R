# ---------------------------------------
# GLM Model
# ---------------------------------------

library(pROC)

# Load train/test data
test.german = read.csv("/Users/gracewang/stat228-credit-classifier/data/german_test.csv", header=TRUE)
train.german = read.csv("/Users/gracewang/stat228-credit-classifier/data/german_train.csv", header=TRUE)


# Fit logit model
fit.logit = glm(good_bad_credit ~ ., data=train.german, family=binomial)

# Perform stepwise variable selection w/ AIC
aic.model = step(fit.logit, direction = "both", trace=FALSE)

# BIC
n <- nrow(train.german)
bic.model <- step(fit.logit, direction = "both", k = log(n), trace=FALSE)

AIC(aic.model)
BIC(aic.model)
formula(aic.model)
length(coef(aic.model))


AIC(bic.model)
BIC(bic.model)
formula(bic.model)
length(coef(bic.model))


# Choosing a AIC threshold
sensitivity = rep(NA, 21)
specificity = rep(NA, 21)
cost = rep(NA, 21)
d.threshold = seq(from = 0, to = 1.0, by = 0.05)

for(i in 1:length(d.threshold))
{
  pi.hat = predict(aic.model, newdata = test.german, type = "response")
  Y.hat = ifelse(pi.hat > d.threshold[i], 1, 0)
  conf.matrix = table(
    factor(test.german$good_bad_credit, levels = c(0, 1)),
    factor(Y.hat, levels = c(0, 1))
  )
  
  TN = conf.matrix[1,1]
  FP = conf.matrix[1,2]
  FN = conf.matrix[2,1]
  TP = conf.matrix[2,2]
  
  cost[i] = conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5
  sensitivity[i] = TP / (TP + FN)
  specificity[i] = TN / (TN + FP)
}

sensitivity
specificity
cost

# Threshold for AIC
png("/Users/gracewang/stat228-credit-classifier/figures/aic_glm_threshold.png", width = 2400, height = 1800, res = 300)
# Left axis: sensitivity and specificity
par(mar = c(5, 5, 4, 6) + 0.1)
plot(d.threshold, sensitivity,
     type = "l",
     lty = 1,
     ylim = c(0, 1),
     xlab = "Threshold",
     ylab = "Sensitivity / Specificity",
     main = "AIC GLM Model: Sensitivity, Specificity, and Cost Across Thresholds")
lines(d.threshold, specificity, lty = 2)
abline(v=0.55, col="red")
# Right axis: cost
par(new = TRUE)
plot(d.threshold, cost,
     type = "l",
     lty = 3,
     axes = FALSE,
     xlab = "",
     ylab = "")

axis(side = 4)
mtext("Cost", side = 4, line = 3)


# Legend
legend("bottomright",
       lty = c(1, 2, 3),
       legend = c("Sensitivity", "Specificity", "Cost"))
dev.off()

# Optimal with threshold with 0.5 to keep sensitivity high without compromising specificity
Y2.hat = ifelse(pi.hat > 0.5, 1, 0)
conf.matrix = table(test.german$good_bad_credit,Y2.hat)
conf.matrix

# Misclassification error
mean(Y2.hat != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])

# Cost
conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5




# Choosing a BIC threshold
sensitivity = rep(NA, 21)
specificity = rep(NA, 21)
cost = rep(NA, 21)
d.threshold = seq(from = 0, to = 1.0, by = 0.05)

for(i in 1:length(d.threshold))
{
  pi.hat = predict(bic.model, newdata = test.german, type = "response")
  Y.hat = ifelse(pi.hat > d.threshold[i], 1, 0)
  conf.matrix = table(
    factor(test.german$good_bad_credit, levels = c(0, 1)),
    factor(Y.hat, levels = c(0, 1))
  )
  
  TN = conf.matrix[1,1]
  FP = conf.matrix[1,2]
  FN = conf.matrix[2,1]
  TP = conf.matrix[2,2]
  
  cost[i] = conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5
  sensitivity[i] = TP / (TP + FN)
  specificity[i] = TN / (TN + FP)
}

sensitivity
specificity
cost

# Threshold for BIC
png("/Users/gracewang/stat228-credit-classifier/figures/bic_glm_threshold.png", width = 2400, height = 1800, res = 300)
# Left axis: sensitivity and specificity
par(mar = c(5, 5, 4, 6) + 0.1)
plot(d.threshold, sensitivity,
     type = "l",
     lty = 1,
     ylim = c(0, 1),
     xlab = "Threshold",
     ylab = "Sensitivity / Specificity",
     main = "BIC GLM Model: Sensitivity, Specificity, and Cost Across Thresholds")
lines(d.threshold, specificity, lty = 2)

# Right axis: cost
par(new = TRUE)
plot(d.threshold, cost,
     type = "l",
     lty = 3,
     axes = FALSE,
     xlab = "",
     ylab = "")

axis(side = 4)
mtext("Cost", side = 4, line = 3)

abline(v=0.55, col="red")

legend("bottomright",
       lty = c(1, 2, 3),
       legend = c("Sensitivity", "Specificity", "Cost"))
dev.off()



# Optimal with threshold with 0.5 to keep sensitivity high without compromising specificity
Y2.hat = ifelse(pi.hat > 0.5, 1, 0)
conf.matrix = table(test.german$good_bad_credit,Y2.hat)
conf.matrix

# Misclassification error
mean(Y2.hat != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])

# Cost
conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5


# ROC curve
png("figures/roc_curve_logistic_model.png", width = 2400, height = 1800, res = 300)
plot(roc(test.german$good_bad_credit,pi.hat),main="ROC curve based on logistic model")
auc(test.german$good_bad_credit,pi.hat)
dev.off()


# Training with weights
# weight bad-credit applicants more heavily
train.german$cost_weight <- ifelse(train.german$good_bad_credit == 0, 5, 1)

# fit weighted logistic regression
fit.weighted <- glm(good_bad_credit ~ checking + duration + credit_history + savings + 
  employment + installment_pct_inc + property + age + other_installment + 
  foreign_worker + purpose_new_car + purpose_used_car + no_other_debtors + 
  coapplicant + housing_own,
  data = train.german,
  family = binomial,
  weights = cost_weight
)

# Choosing a weighted AIC threshold
sensitivity = rep(NA, 21)
specificity = rep(NA, 21)
cost = rep(NA, 21)
d.threshold = seq(from = 0, to = 1.0, by = 0.05)

for(i in 1:length(d.threshold))
{
  pi.hat = predict(fit.weighted, newdata = test.german, type = "response")
  Y.hat = ifelse(pi.hat > d.threshold[i], 1, 0)
  conf.matrix = table(
    factor(test.german$good_bad_credit, levels = c(0, 1)),
    factor(Y.hat, levels = c(0, 1))
  )
  
  TN = conf.matrix[1,1]
  FP = conf.matrix[1,2]
  FN = conf.matrix[2,1]
  TP = conf.matrix[2,2]
  
  cost[i] = conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5
  sensitivity[i] = TP / (TP + FN)
  specificity[i] = TN / (TN + FP)
}

sensitivity
specificity
cost

# Threshold for AIC
png("/Users/gracewang/stat228-credit-classifier/figures/weighted_glm_threshold.png", width = 2400, height = 1800, res = 300)
# Left axis: sensitivity and specificity
par(mar = c(5, 5, 4, 6) + 0.1)
plot(d.threshold, sensitivity,
     type = "l",
     lty = 1,
     ylim = c(0, 1),
     xlab = "Threshold",
     ylab = "Sensitivity / Specificity",
     main = "Weighted GLM Model: Sensitivity, Specificity, and Cost Across Thresholds")
lines(d.threshold, specificity, lty = 2)
abline(v=0.55, col="red")
# Right axis: cost
par(new = TRUE)
plot(d.threshold, cost,
     type = "l",
     lty = 3,
     axes = FALSE,
     xlab = "",
     ylab = "")

axis(side = 4)
mtext("Cost", side = 4, line = 3)


# Legend
legend("bottomright",
       lty = c(1, 2, 3),
       legend = c("Sensitivity", "Specificity", "Cost"))
dev.off()

# Optimal with threshold with 0.5 to keep sensitivity high without compromising specificity
Y2.hat = ifelse(pi.hat > 0.2, 1, 0)
conf.matrix = table(test.german$good_bad_credit,Y2.hat)
conf.matrix

# Misclassification error
mean(Y2.hat != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])

# Cost
conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5

