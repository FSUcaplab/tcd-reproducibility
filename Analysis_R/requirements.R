# Installs the packages the pipeline needs. Run once:
#   Rscript Analysis_R/requirements.R

required_packages <- c(
  # data handling
  "readxl", "dplyr", "tibble",
  # figures
  "ggplot2", "patchwork", "ggtext",
  # mixed models and their summaries (step 07)
  "lme4", "car", "MuMIn"
)

missing <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing) == 0) {
  message("All required packages are installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
