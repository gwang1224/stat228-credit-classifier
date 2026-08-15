# ---------------------------------------------------------------
# 1. Data cleaning
#
# Reads the numeric version of the STATLOG German Credit dataset
# (1,000 applicants, 24 predictors), assigns readable column names,
# recodes the outcome (original coding: 1 = good, 2 = bad) to a
# 0/1 indicator (1 = good credit, 0 = bad credit), and writes a
# cleaned CSV plus a fixed 800/200 train/test split.
# ---------------------------------------------------------------

german <- read.table("data/german.data-numeric", quote = "\"", comment.char = "")

german$checking <- german$V1
german$duration <- german$V2
german$credit_history <- german$V3
german$credit_amount <- german$V4
german$savings <- german$V5
german$employment <- german$V6
german$installment_pct_inc <- german$V7
german$personal_status_sex <- german$V8
german$property <- german$V9
german$age <- german$V10
german$other_installment <- german$V11
german$credits_in_bank <- german$V12
german$telephone_owned <- german$V13
german$residence_since <- german$V14
german$foreign_worker <- german$V15

# Indicator variables
german$purpose_new_car <- german$V16
german$purpose_used_car <- german$V17
german$no_other_debtors <- german$V18
german$coapplicant <- german$V19
german$housing_rent <- german$V20
german$housing_own <- german$V21
german$job_unskilled_nonres <- german$V22
german$job_unskilled_res <- german$V23
german$job_skilled <- german$V24

# Outcome: 1 = good credit, 0 = bad credit
german$good_bad_credit <- ifelse(german$V25 == 2, 0, 1)

german <- german[, 26:50]

# Sanity check: the dataset has no missing values
stopifnot(sum(is.na(german)) == 0)

write.csv(german, "data/german_clean.csv", row.names = FALSE)

# Fixed 800/200 train/test split (train: 567 good / 233 bad,
# test: 133 good / 67 bad). All downstream scripts read these
# CSVs so every model is evaluated on the same held-out data.
set.seed(1)
train.index <- sample(1:1000, 800, replace = FALSE)

write.csv(german[train.index, ], "data/german_train.csv", row.names = FALSE)
write.csv(german[-train.index, ], "data/german_test.csv", row.names = FALSE)
