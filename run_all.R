# ---------------------------------------------------------------
# Run the full pipeline end to end and build the final
# model-comparison table (results/model_comparison.csv).
#
# Usage: Rscript run_all.R
# ---------------------------------------------------------------

scripts <- c(
  "1_data_cleaning.R",
  "2_variable_selection.R",
  "3_logistic_regression.R",
  "4_discriminant_analysis.R",
  "5_tree_models.R",
  "6_svm_models.R"
)

for (script in scripts) {
  cat("\n========", script, "========\n")
  status <- system2("Rscript", script)
  if (status != 0) stop("Script failed: ", script)
}

# Combine per-model metrics into one comparison table
metric_files <- c(
  "results/logistic_metrics.csv",
  "results/discriminant_metrics.csv",
  "results/tree_metrics.csv",
  "results/svm_metrics.csv"
)
comparison <- do.call(rbind, lapply(metric_files, read.csv))
comparison <- comparison[order(comparison$Cost), ]

write.csv(comparison, "results/model_comparison.csv", row.names = FALSE)

cat("\n======== Final model comparison (sorted by cost) ========\n")
print(comparison, row.names = FALSE)
