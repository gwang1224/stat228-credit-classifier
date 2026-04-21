german_data <- read.csv("C:/IPUMS/usa_0001.dat/german_clean.csv")
View(german_data)
german_data$good_bad_credit <- as.factor(german_data$good_bad_credit)

set.seed(1)

train_idx <- sample(1:nrow(german_data), 0.7 * nrow(german_data))
train <- german_data[train_idx, ]
test  <- german_data[-train_idx, ]


#Standardizing
train_x <- scale(train[, -which(names(train) == "good_bad_credit")])
test_x  <- scale(test[, -which(names(test) == "good_bad_credit")],
                 center = attr(train_x, "scaled:center"),
                 scale  = attr(train_x, "scaled:scale"))

train_scaled <- data.frame(train_x, label = train$good_bad_credit)
test_scaled  <- data.frame(test_x, label = test$good_bad_credit)

#LDA fit 
library(MASS)
lda_fit <- lda(label ~ ., data = train_scaled)

lda_pred <- predict(lda_fit, newdata = test_scaled)

table(Predicted = lda_pred$class, Actual = test_scaled$label)
mean(lda_pred$class != test_scaled$label)


#QDA fit

qda_fit <- qda(label ~ ., data = train_scaled)
qda_pred <- predict(qda_fit, newdata = test_scaled)

table(Predicted = qda_pred$class, Actual = test_scaled$label)
mean(qda_pred$class != test_scaled$label)
