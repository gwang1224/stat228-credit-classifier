# ---------------------------------------
# German Credit Data Variable Screening for Multicollinearity
# ---------------------------------------

library(usdm)
library(car)

data <- read.csv("data/german_clean.csv", header = T)

model <- glm(good_bad_credit ~ ., data = data, family = binomial)

alias(model)
vifstep(model)
