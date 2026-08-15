# Install the packages required by this project (skips any
# that are already installed).
#
# Usage: Rscript install_packages.R

packages <- c("car", "pROC", "e1071", "MASS", "tree", "randomForest", "biotools")

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
