# ---------------------------------------
# German Credit Data Variable Screening for Multicollinearity
# ---------------------------------------

german = read.csv("data/german_clean.csv")
summary(german)

# No missing values, proceed to variable screening

fit = glm(good_bad_credit ~ ., data = german, family = binomial)

library(car)
vif = vif(fit)

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
