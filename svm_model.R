# ---------------------------------------
# SVM Model
# ---------------------------------------

library(e1071)

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
                     scale=TRUE)

Y.hat.linear = predict(svm.fit.linear, test.german)

conf.matrix = table(test.german$good_bad_credit,Y.hat.linear)

linear.mc = mean(Y.hat.linear != test.german$good_bad_credit)
linear.sens = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
linear.spec = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])


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
                     scale=TRUE)

Y.hat.poly = predict(svm.fit.poly, test.german)

conf.matrix = table(test.german$good_bad_credit,Y.hat.poly)

poly.mc = mean(Y.hat.poly != test.german$good_bad_credit)
poly.sens = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
poly.spec = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])


# ------------------------------------------------------------------------------

tune.out <- tune(
  svm,
  good_bad_credit ~ .,
  data = train.german,
  kernel = "radial",
  ranges = list(gamma = 2^seq(-8, 1, by = 2), cost = c(0.1, 1, 5, 10, 100, 1000)),
  tunecontrol = tune.control(sampling = "fix"),
  validation.x = subset(test.german, select = -good_bad_credit),
  validation.y = test.german$good_bad_credit
)
tune.out$best.parameters

# Radial SVM
svm.fit.radial = svm(formula = good_bad_credit ~ .,
                   data=train.german,
                   type="C-classification",
                   kernel="radial",
                   cost=5,
                   gamma=0.0625,
                   scale=TRUE)

Y.hat.radial = predict(svm.fit.radial, test.german)

conf.matrix = table(test.german$good_bad_credit,Y.hat.radial)

rad.mc = mean(Y.hat.radial != test.german$good_bad_credit)
rad.sens = conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
rad.spec = conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])


svm.results <- data.frame(
  Model = c("Linear SVM", "Polynomial SVM", "Radial SVM"),
  Misclassification = c(linear.mc, poly.mc, rad.mc),
  Sensitivity = c(linear.sens, poly.sens, rad.sens),
  Specificity = c(linear.spec, poly.spec, rad.spec)
)

svm.results