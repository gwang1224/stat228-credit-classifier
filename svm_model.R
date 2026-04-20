# ---------------------------------------
# SVM Model
# ---------------------------------------

library(e1071)

test.german = read.csv("data/german_test.csv", header=TRUE)
train.german = read.csv("data/german_train.csv", header=TRUE)

# Linear SVM
svm.fit.linear = svm(formula = good_bad_credit ~ .,
                     data=train.german,
                     type="C-classification",
                     kernel="linear",
                     cost=1)

Y.hat.linear = predict(svm.fit.linear, test.german)

conf.matrix = table(test.german$good_bad_credit,Y.hat.linear)

# Misclassification
mean(Y.hat.linear != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])


# Polynomial SVM
svm.fit.poly = svm(formula = good_bad_credit ~ .,
                     data=train.german,
                     type="C-classification",
                     kernel="polynomial",
                     cost=1)

Y.hat.poly = predict(svm.fit.poly, test.german)

conf.matrix = table(test.german$good_bad_credit,Y.hat.poly)

# Misclassification
mean(Y.hat.poly != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])



# Radial SVM
svm.fit.radial = svm(formula = good_bad_credit ~ .,
                   data=train.german,
                   type="C-classification",
                   kernel="radial",
                   cost=1)

Y.hat.radial = predict(svm.fit.radial, test.german)

conf.matrix = table(test.german$good_bad_credit,Y.hat.radial)

# Misclassification
mean(Y.hat.radial != test.german$good_bad_credit)

# Sensitivity
conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])

# Specificity
conf.matrix[1,1] / (conf.matrix[1,1] + conf.matrix[2,1])
