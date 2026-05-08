# ---------------------------------------
# German Credit Data Cleaning
# ---------------------------------------

german = read.table("data/german.data-numeric", quote="\"", comment.char="")

german$checking = german$V1
german$duration = german$V2
german$credit_history = german$V3
german$credit_amount = german$V4 
german$savings = german$V5 
german$employment = german$V6 
german$installment_pct_inc = german$V7
german$personal_status_sex = german$V8 
german$property = german$V9  
german$age = german$V10 
german$other_installment = german$V11 
german$credits_in_bank = german$V12
german$telephone_owned = german$V13 
german$residence_since = german$V14
german$foreign_worker = german$V15

# Indicator variables
german$purpose_new_car = german$V16
german$purpose_used_car = german$V17 
german$no_other_debtors = german$V18 
german$coapplicant = german$V19
german$housing_rent = german$V20
german$housing_own = german$V21
german$job_unskilled_nonres = german$V22
german$job_unskilled_res = german$V23 
german$job_skilled = german$V24

german$good_bad_credit = german$V25
german$good_bad_credit = ifelse(german$good_bad_credit == 2, 0, 1)

german = german[, 26:50]

write.csv(german, "data/german_clean.csv", row.names = FALSE)




# Create train/test split

# 800 in train/200 in test
set.seed(1)
train.index = sample(1:1000, 800, replace=FALSE)

write.csv(german[train.index,], "data/german_train.csv", row.names = FALSE) #233/567
write.csv(german[-train.index,], "data/german_test.csv", row.names = FALSE) #67/133

# ---------------------------------------
# German Credit Data Variable Screening for Multicollinearity
# ---------------------------------------

german = read.csv("data/german_clean.csv")
summary(german)

factor_vars <- c(
  "checking",
  "credit_history",
  "savings",
  "employment",
  "personal_status_sex",
  "property",
  "other_installment",
  "telephone_owned",
  "foreign_worker",
  "purpose_new_car",
  "purpose_used_car",
  "no_other_debtors",
  "coapplicant",
  "housing_rent",
  "housing_own",
  "job_unskilled_nonres",
  "job_unskilled_res",
  "job_skilled",
  "good_bad_credit"
)

german[factor_vars] <- lapply(german[factor_vars], as.factor)

# No missing values, proceed to variable screening

fit = glm(good_bad_credit ~ ., data = german, family = binomial)

library(car)
vif = car::vif(fit)

# Capture the printed output as text
txt <- capture.output(print(sort(vif, decreasing = TRUE)))

# Save as an image that looks like console output
png("figures/vif_console_output.png", width = 2200, height = 900, res = 150)
plot.new()
text(
  x = 0, y = 1,
  labels = paste(txt, collapse = "\n"),
  adj = c(0, 1),
  family = "mono",
  cex = 1.2
)
dev.off()
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
# ---------------------------------------
# SVM Model
# ---------------------------------------

library(e1071)
library(pROC)

test.german = read.csv("data/german_test.csv", header=TRUE)
train.german = read.csv("data/german_train.csv", header=TRUE)


# Linear SVM
tune.out <- tune(
  svm,
  good_bad_credit ~ .,
  data = train.german,
  kernel = "linear",
  ranges = list(cost = c(0.1, 1, 5, 10, 100, 1000)),
  tunecontrol = tune.control(sampling = "fix"),
  validation.x = subset(test.german, select = -good_bad_credit),
  validation.y = test.german$good_bad_credit
)
tune.out$best.parameters

# Fit with Cost = 100
svm.fit.linear = svm(formula = good_bad_credit ~ .,
                     data=train.german,
                     type="C-classification",
                     kernel="linear",
                     cost=100,
                     scale=TRUE,
                     probability=TRUE)

Y.hat.linear = predict(svm.fit.linear, test.german, probability=TRUE)

conf.matrix = table(test.german$good_bad_credit,Y.hat.linear)

linear.mc = mean(Y.hat.linear != test.german$good_bad_credit)
linear.sens = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
linear.spec = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])
linear.cost = conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5


# ------------------------------------------------------------------------------
tune.out <- tune(
  svm,
  good_bad_credit ~ .,
  data = train.german,
  kernel = "polynomial",
  ranges = list(cost = c(0.1, 1, 5, 10, 100, 1000)),
  tunecontrol = tune.control(sampling = "fix"),
  validation.x = subset(test.german, select = -good_bad_credit),
  validation.y = test.german$good_bad_credit
)
tune.out$best.parameters

# Polynomial SVM
svm.fit.poly = svm(formula = good_bad_credit ~ .,
                   data=train.german,
                   type="C-classification",
                   kernel="polynomial",
                   cost=1,
                   scale=TRUE,
                   probability=TRUE)

Y.hat.poly = predict(svm.fit.poly, test.german, probability=TRUE)

conf.matrix = table(test.german$good_bad_credit,Y.hat.poly)

poly.mc = mean(Y.hat.poly != test.german$good_bad_credit)
poly.sens = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
poly.spec = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])
poly.cost = conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5


# ------------------------------------------------------------------------------

tune.out <- tune(
  svm,
  good_bad_credit ~ .,
  data = train.german,
  kernel = "radial",
  ranges = list(gamma = 2^seq(-8, 1, by = 2), cost = c(0.1, 1, 5, 10, 100, 1000)),
  tunecontrol = tune.control(sampling = "fix"),
  validation.x = subset(test.german, select = -good_bad_credit),
  validation.y = test.german$good_bad_credit,
  class.weights = c("0" = 1, "1" = 5),
)
tune.out$best.parameters

# Radial SVM
svm.fit.radial = svm(formula = good_bad_credit ~ .,
                     data=train.german,
                     type="C-classification",
                     kernel="radial",
                     cost=5,
                     gamma=0.0625,
                     scale=TRUE,
                     probability=TRUE)

Y.hat.radial = predict(svm.fit.radial, test.german, probability=TRUE)
svm.prob <- attr(Y.hat.radial, "probabilities")

head(svm.prob)

conf.matrix = table(test.german$good_bad_credit,Y.hat.radial)

rad.mc = mean(Y.hat.radial != test.german$good_bad_credit)
rad.sens = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
rad.spec = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])
rad.cost = conf.matrix[1,2] * 1 + conf.matrix[2,1] * 5

png("/Users/gracewang/stat228-credit-classifier/figures/roc_curve_svm_model.png", width = 800, height = 600)
roc.svm <- roc(
  response = test.german$good_bad_credit,
  predictor = svm.prob[, "0"]
)
plot(roc.svm, main = "ROC Curve for SVM")
auc(roc.svm)
dev.off()



svm.results <- data.frame(
  Model = c("Linear SVM", "Polynomial SVM", "Radial SVM"),
  Misclassification = c(linear.mc, poly.mc, rad.mc),
  Sensitivity = c(linear.sens, poly.sens, rad.sens),
  Specificity = c(linear.spec, poly.spec, rad.spec)
)

#=============================================================
#Setting up!
german_data <- read.csv("C:/IPUMS/usa_0001.dat/german_clean.csv")
View(german_data)
german_data$good_bad_credit <- as.factor(german_data$good_bad_credit)

set.seed(1) #important!! 

#Indexing training/test data
train_idx <- sample(1:nrow(german_data), 0.8 * nrow(german_data))
train <- german_data[train_idx, ]
test  <- german_data[-train_idx, ]



#==============================================================
#Assumptions 

#1. Multivariate Normality 
dev.off() 

par(mfrow = c(2, 2))
qqnorm(
  german_data$age[german_data$good_bad_credit == 1],
  main = "Q-Q Plot of Age (Bad Credit Applicants)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(german_data$age[german_data$good_bad_credit == 1])

qqnorm(
  german_data$age[german_data$good_bad_credit == 0],
  main = "Q-Q Plot of Age (Good Credit Applicants)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(german_data$age[german_data$good_bad_credit == 0])
# CREDIT AMOUNT


qqnorm(
  german_data$credit_amount[german_data$good_bad_credit == 1],
  main = "Q-Q Plot of Credit Amount (Bad Credit Applicants)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(german_data$credit_amount[german_data$good_bad_credit == 1])

qqnorm(
  german_data$credit_amount[german_data$good_bad_credit == 0],
  main = "Q-Q Plot of Credit Amount (Good Credit Applicants)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(german_data$credit_amount[german_data$good_bad_credit == 0])

# DURATION

qqnorm(
  german_data$duration[german_data$good_bad_credit == 1],
  main = "Q-Q Plot of Loan Duration (Bad Credit Applicants)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(german_data$duration[german_data$good_bad_credit == 1])

qqnorm(
  german_data$duration[german_data$good_bad_credit == 0],
  main = "Q-Q Plot of Loan Duration (Good Credit Applicants)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(german_data$duration[german_data$good_bad_credit == 0])


# NOTE: not sure if i have to include more variables, but included three + train/test 
#NOTE2: No big deviations from the normal, some slight. LDA/QDA pretty robust, we proceed
#2. Homogeneity of Covariance

install.packages("biotools")
library("biotools")

boxM(train[, -which(names(train) == "good_bad_credit")],
     train$good_bad_credit)

#NOTE: we reject the null here, we can only proceed with QDA rather than LDA

#==========================================================
#QDA fit

qda_fit <- qda(label ~ ., data = train_scaled)

# Probability of BAD = 1 (positive class)
qda_prob <- predict(qda_fit, newdata = test_scaled)$posterior[, "1"]

thresholds <- seq(0.01, 0.99, by = 0.01)

sens_values <- sapply(thresholds, function(t) {
  pred <- factor(ifelse(qda_prob > t, 1, 0), levels = c(0,1))
  actual <- factor(test_scaled$label, levels = c(0,1))
  
  cm <- table(Predicted = pred, Actual = actual)
  
  TP <- cm["1","1"]
  FN <- cm["0","1"]
  
  TP / (TP + FN)
})

best_t <- thresholds[which.max(sens_values)]
best_t

# Final predictions
qda_pred_tuned <- factor(ifelse(qda_prob > best_t, 1, 0), levels = c(0,1))

cm_qda <- table(Predicted = qda_pred_tuned, Actual = factor(test_scaled$label, levels = c(0,1)))

TP_qda <- cm_qda["1","1"]  
FN_qda <- cm_qda["0","1"]  
FP_qda <- cm_qda["1","0"]  
TN_qda <- cm_qda["0","0"]  

# Metrics
sensitivity_qda <- TP_qda / (TP_qda + FN_qda)
specificity_qda <- TN_qda / (TN_qda + FP_qda)
precision_qda <- TP_qda / (TP_qda + FP_qda)
f1_qda <- 2 * (precision_qda * sensitivity_qda) / (precision_qda + sensitivity_qda)
misclassification_qda <- (FP_qda + FN_qda) / sum(cm_qda)

# Cost 
Cost_qda <- 5 * FN_qda + 1 * FP_qda

qda_results <- data.frame(
  Model = "QDA (Tuned)",
  Threshold = best_t,
  Sensitivity = sensitivity_qda,
  Specificity = specificity_qda,
  Precision = precision_qda,
  F1_Score = f1_qda,
  Misclassification = misclassification_qda,
  Cost = Cost_qda
)

qda_results

actual <- test_scaled$label
roc_obj <- roc(actual, qda_prob)
auc(roc_obj)

#===================================================
#QDA without COST MATRIX
qda_fit <- qda(label ~ ., data = train_scaled)

qda_pred_default <- predict(qda_fit, newdata = test_scaled)

cm_qda_default <- table(
  Predicted = qda_pred_default$class,
  Actual = factor(test_scaled$label, levels = c(0,1))
)

# Extract values (BAD = 1 is positive class)
TP <- cm_qda_default["1","1"]
FN <- cm_qda_default["0","1"]
FP <- cm_qda_default["1","0"]
TN <- cm_qda_default["0","0"]

cost_qda_d <- 5 * FN + 1 * FP

# Metrics
sensitivity_qda <- TP / (TP + FN)
specificity_qda <- TN / (TN + FP)
precision_qda   <- TP / (TP + FP)
f1_qda          <- 2 * (precision_qda * sensitivity_qda) / (precision_qda + sensitivity_qda)
misclassification_qda <- (FP + FN) / sum(cm_qda_default)

qda_default_results <- data.frame(
  Model = "QDA (Default 0.5)",
  Sensitivity = sensitivity_qda,
  Specificity = specificity_qda,
  Precision = precision_qda,
  F1_Score = f1_qda,
  Misclassification = misclassification_qda
)

qda_default_results
#=====================================================

#Section 2: Tree Based Models

#===================================================
#Setting UP tree and forest 

library(pROC)
library(tree)
library(randomForest)

# Make sure outcome stays consist.
train$good_bad_credit <- factor(train$good_bad_credit, levels = c(0,1))
test$good_bad_credit  <- factor(test$good_bad_credit, levels = c(0,1))

#=================================================== 
#Tree Pruning

fit <- tree(good_bad_credit ~ ., data = train)
plot(fit)
text(fit, pretty = 0)
cv_fit <- cv.tree(fit, FUN = prune.misclass) 
best_size <- cv_fit$size[which.min(cv_fit$dev)]
#graphing it 
pruned_tree <- prune.misclass(fit, best = best_size)
plot(cv_fit$size, cv_fit$dev, type = "b", xlab = "Number of Terminal Nodes", ylab = "Cross-Validated Misclassification Error", main = "Tree Size vs Misclassification Error") 
plot(pruned_tree)
text(pruned_tree, pretty = 0)
#====================================================
# 1. CLASSIFICATION TREE

thresholds <- seq(0.01, 0.99, by = 0.01)

tree_fit <- pruned_tree


tree_prob <- predict(tree_fit, newdata = test)[, "1"]

roc_tree <- roc(test$good_bad_credit, tree_prob)
auc_tree <- auc(roc_tree)

cost_values_tree <- sapply(thresholds, function(t) {
  pred <- factor(ifelse(tree_prob > t, 1, 0), levels = c(0,1))
  actual <- factor(test$good_bad_credit, levels = c(0,1))
  
  cm <- table(Predicted = pred, Actual = actual)
  
  FP <- cm["1","0"]
  FN <- cm["0","1"]
  
  5 * FN + 1 * FP
})

tree_t <- thresholds[which.min(cost_values_tree)]
tree_t

# Final predictions
tree_pred <- factor(ifelse(tree_prob > tree_t, 1, 0), levels = c(0,1))
actual <- factor(test$good_bad_credit, levels = c(0,1))

cm_tree <- table(Predicted = tree_pred, Actual = actual)

TP <- cm_tree["1","1"]
FP <- cm_tree["1","0"]
FN <- cm_tree["0","1"]
TN <- cm_tree["0","0"]

# Metrics
sensitivity_tree <- TP/(TP+FN)
specificity_tree <- TN/(TN+FP)
misclassification_tree <- (FP+FN)/sum(cm_tree)
cost_tree <- 5*FN + 1*FP

sensitivity_tree
specificity_tree 
misclassification_tree 
cost_tree

#==================================================
#TREE witohut costs


tree_fit <- pruned_tree   

tree_prob <- predict(tree_fit, newdata = test, type = "vector")[, "1"]


tree_pred_default <- factor(ifelse(tree_prob > 0.5, 1, 0), levels = c(0,1))

cm_tree_default <- table(
  Predicted = tree_pred_default,
  Actual = factor(test$good_bad_credit, levels = c(0,1))
)

TP <- cm_tree_default["1","1"]
FN <- cm_tree_default["0","1"]
FP <- cm_tree_default["1","0"]
TN <- cm_tree_default["0","0"]

cost_tree_d <- 5 * FN + 1 * FP
# Metrics
sensitivity_tree <- TP/(TP+FN)
specificity_tree <- TN/(TN+FP)
precision_tree   <- TP/(TP+FP)
f1_tree          <- 2 * (precision_tree * sensitivity_tree) / (precision_tree + sensitivity_tree)
misclassification_tree <- (FP+FN)/sum(cm_tree_default)

tree_default_results <- data.frame(
  Model = "Tree (Default 0.5)",
  Sensitivity = sensitivity_tree,
  Specificity = specificity_tree,
  Precision = precision_tree,
  F1_Score = f1_tree,
  Misclassification = misclassification_tree
)

tree_default_results

#====================================================
# 2. RANDOM FOREST


thresholds <- seq(0.01, 0.99, by = 0.01)
rf_fit <- randomForest(good_bad_credit ~ ., data = train, ntree = 500)

rf_prob <- predict(rf_fit, newdata = test, type = "prob")[, "1"]
roc_rf <- roc(test$good_bad_credit, rf_prob)
auc_rf <- auc(roc_rf)

cost_values <- sapply(thresholds, function(t) {
  pred <- factor(ifelse(rf_prob > t, 1, 0), levels = c(0,1))
  actual <- factor(test$good_bad_credit, levels = c(0,1))
  
  cm <- table(Predicted = pred, Actual = actual)
  
  FP <- cm["1","0"]
  FN <- cm["0","1"]
  
  5 * FN + 1 * FP
})

rf_t <- thresholds[which.min(cost_values)]
rf_t


rf_pred <- factor(ifelse(rf_prob > rf_t, 1, 0), levels = c(0,1))

cm_rf <- table(Predicted = rf_pred, Actual = test$good_bad_credit)

TP_rf <- cm_rf["1","1"]
FP_rf <- cm_rf["1","0"]
FN_rf <- cm_rf["0","1"]
TN_rf <- cm_rf["0","0"]

# Metrics
sensitivity_rf <- TP_rf/(TP_rf+FN_rf)
specificity_rf <- TN_rf/(TN_rf+FP_rf)
misclassification_rf <- (FP_rf+FN_rf)/sum(cm_rf)

# Final cost
cost_rf <- 5*FN_rf + 1*FP_rf
#===================================================
#RF without cost, assuming we use a default threshold of 0.5



rf_fit <- randomForest(good_bad_credit ~ ., data = train, ntree = 500)

rf_prob <- predict(rf_fit, newdata = test, type = "prob")[, "1"]


rf_pred_default <- factor(ifelse(rf_prob > 0.5, 1, 0), levels = c(0,1))

cm_rf_default <- table(
  Predicted = rf_pred_default,
  Actual = factor(test$good_bad_credit, levels = c(0,1))
)

TP <- cm_rf_default["1","1"]
FN <- cm_rf_default["0","1"]
FP <- cm_rf_default["1","0"]
TN <- cm_rf_default["0","0"]

cost_rf <- 5 * FN + 1 * FP

# Metrics
sensitivity_rf <- TP/(TP+FN)
specificity_rf <- TN/(TN+FP)
precision_rf   <- TP/(TP+FP)
f1_rf          <- 2 * (precision_rf * sensitivity_rf) / (precision_rf + sensitivity_rf)
misclassification_rf <- (FP+FN)/sum(cm_rf_default)

rf_default_results <- data.frame(
  Model = "Random Forest (Default 0.5)",
  Sensitivity = sensitivity_rf,
  Specificity = specificity_rf,
  Precision = precision_rf,
  F1_Score = f1_rf,
  Misclassification = misclassification_rf
)

rf_default_results
#===================================================
##Variable importance plot

importance(rf_fit)
windows()
varImpPlot(rf_fit)
#====================================================
# AUC TABLE

auc_results <- data.frame(
  Model = c("Classification Tree", "Random Forest"),
  AUC = c(auc_tree, auc_rf)
)


auc_results

#====================================================
# ROC PLOT w legend

roc_tree <- roc(test$good_bad_credit, tree_prob)
auc_tree <- auc(roc_tree)
plot(roc_rf, col = "red", lwd = 2, main = "ROC Curves Comparison")
plot(roc_tree, col = "green", lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")

legend("bottomright",
       legend = c(
         paste("RF AUC =", round(auc_rf, 3)),
         paste("Tree AUC =", round(auc_tree, 3))
       ),
       col = c("red", "green"),
       lwd = 2)


