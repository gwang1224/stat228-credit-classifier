# ---------------------------------------
# German Credit Data Cleaning
# Reads in the raw German credit dataset, examines the original variables,
# renames V1-V20 into clearer feature names, splits V9 into separate
# gender and marital_status variables, removes the original raw columns,
# and exports the cleaned dataset as german_clean.csv for later analysis.
# ---------------------------------------

german <- read.table("data/german.data-numeric", quote="\"", comment.char="")
View(german)

summary(german$V1)
hist(german$V1)

#Step 1: replace variables V"" into clearer names

german$checking <- german$V1
german$duration <- german$V2
german$credit_history <- german$V3
german$purpose <- german$V4
german$credit_amount <- german$V5
german$employment <- german$V6
german$installment_rate <- german$V7

#V9 can be split into two extra variables


# german$gender <- ifelse(german$V9 %in% c("1", "3", "4"),
#                         "male", "female")
# german$gender <- as.factor(german$gender)
# german$marital_status <- dplyr::case_when(
#   german$V9 == "1" ~ "divorced_separated",
#   german$V9 == "2" ~ "married",  # simplifying this mixed category
#   german$V9 == "3" ~ "single",
#   german$V9 == "4" ~ "married_widowed",
#   german$V9 == "5" ~ "single"
# )
# german$marital_status <- as.factor(german$marital_status)
german$dis_inc <- german$V8
german$gender <- german$V9


german$other_debtors_gaurantors <- german$V10
german$residenceyr <- german$V11
german$property <- german$V12
german$age <- german$V13
german$other_installment <- german$V14
german$housing <- german$V15
german$credits_in_bank <- german$V16
german$job <- german$V17
german$people_liable <- german$V18
german$telephone_owned <- german$V19
german$foreign_worker <- german$V20
german$good_bad_credit <- german$V25

# Change predicted variable to 1/0, 1 = Good, 0 = Bad
german$good_bad_credit <- ifelse(german$good_bad_credit == "1", 1, 0)

#Getting rid of V1-20, 25
german <- german[, -c(1:20, 25)]
write.csv(german, "data/german_clean.csv", row.names = FALSE)
