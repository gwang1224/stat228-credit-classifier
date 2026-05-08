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
