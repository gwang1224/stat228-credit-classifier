# ---------------------------------------------------------------
# 2. Variable screening for multicollinearity
#
# Fits a full logistic regression and computes generalized
# variance inflation factors (GVIF). Saves the GVIF values to
# results/ and a bar chart to figures/.
# ---------------------------------------------------------------

library(car)

german <- read.csv("data/german_clean.csv")

factor_vars <- c(
  "checking", "credit_history", "savings", "employment",
  "personal_status_sex", "property", "other_installment",
  "telephone_owned", "foreign_worker", "purpose_new_car",
  "purpose_used_car", "no_other_debtors", "coapplicant",
  "housing_rent", "housing_own", "job_unskilled_nonres",
  "job_unskilled_res", "job_skilled", "good_bad_credit"
)
german[factor_vars] <- lapply(german[factor_vars], as.factor)

fit <- glm(good_bad_credit ~ ., data = german, family = binomial)

vif_values <- car::vif(fit)

# vif() returns a matrix (GVIF, Df, GVIF^(1/(2*Df))) when factors are
# present; the last column is the value comparable across predictors.
if (is.matrix(vif_values)) {
  adj_gvif <- sort(vif_values[, "GVIF^(1/(2*Df))"], decreasing = TRUE)
} else {
  adj_gvif <- sort(vif_values, decreasing = TRUE)
}

print(adj_gvif)

dir.create("results", showWarnings = FALSE)
write.csv(
  data.frame(Predictor = names(adj_gvif), Adjusted_GVIF = round(adj_gvif, 3)),
  "results/vif_values.csv",
  row.names = FALSE
)

png("figures/vif_barplot.png", width = 2400, height = 1800, res = 300)
par(mar = c(5, 11, 4, 2))
barplot(rev(adj_gvif),
  horiz = TRUE, las = 1, cex.names = 0.7,
  xlab = "Adjusted GVIF  (GVIF^(1/(2*Df)))",
  main = "Multicollinearity screening: adjusted GVIF by predictor"
)
abline(v = sqrt(5), lty = 2, col = "red")
dev.off()

# All adjusted GVIF values fall well below the common concern
# threshold (sqrt(5) ~ 2.24), so all predictors are retained.
