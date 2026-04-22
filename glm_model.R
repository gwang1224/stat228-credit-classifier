# ---------------------------------------
# GLM Model
# ---------------------------------------

library(pROC)

# Load train/test data
test.german = read.csv("data/german_test.csv", header=TRUE)
train.german = read.csv("data/german_train.csv", header=TRUE)


# Fit logit model
fit.logit = glm(good_bad_credit ~ ., data=train.german)

# Perform stepwise w/ AIC
step_model = step(fit.logit, direction = "both")


# Choosing a threshold

sensitivity = rep(NA, 21)
specificity = rep(NA, 21)
d.threshold = seq(from = 0, to = 1.0, by = 0.05)

for(i in 1:length(d.threshold))
{
  Y.hat = ifelse(pi.hat > d.threshold[i], 1, 0)
  conf.matrix = table(
    factor(Y.hat, levels = c(0, 1)),
    factor(test.german$good_bad_credit, levels = c(0, 1))
  )
  
  sensitivity[i] = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
  specificity[i] = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])
}

# Threshold image
png("figures/threshold_sensitivity_specificity.png", width = 800, height = 600)
plot(d.threshold, sensitivity, lty = 1, type = "l",
     ylim = c(0, 1), xlab = "Threshold", ylab = "Measure")
lines(d.threshold, specificity, lty = 2)
legend("bottomright", lty = c(1,2),
       legend = c("sensitivity","specificity"))
dev.off()


# Optimal with threshold with 0.5 to keep sensitivity high without compromising specificity
Y2.hat = ifelse(pi.hat > 0.5, 1, 0)
conf.matrix = table(Y2.hat, test.german$good_bad_credit)
conf.matrix

# Misclassification error
mean(Y2.hat != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])



# ROC curve
png("figures/roc_curve_logistic_model.png", width = 800, height = 600)
plot(roc(test.german$good_bad_credit,pi.hat),main="ROC curve based on logistic model")
auc(test.german$good_bad_credit,pi.hat)
dev.off()

