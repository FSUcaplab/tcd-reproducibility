# Step 06 - Render every figure as PNG and PDF.
# Reads the statistics CSVs written by steps 02-04, so run those first.

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step(c("03_reliability_tables", "04_fig1_tables", "05_sensitivity_tables", "06_figures"))

required_csvs <- c(
  "BA_ICC_CCC_Statistics.csv",
  "Fig5_ICC_CCC_Sex_Menstrual_Stats.csv",
  "Fig6_ICC_CCC_Exclude_Unmatched_Females_Stats.csv"
)
missing_csvs <- required_csvs[!file.exists(file.path(step$config$output_dir, required_csvs))]
if (length(missing_csvs) > 0) {
  stop("Run steps 02 and 04 before rendering figures. Missing: ",
       paste(missing_csvs, collapse = ", "), call. = FALSE)
}

render_all_figures_r(step$df, step$config)

for (name in names(figure_dims)) {
  for (ext in c("png", "pdf")) {
    path <- file.path(step$config$output_dir, paste0(name, ".", ext))
    cat("Rendered: ", basename(path), " (", file.info(path)$size, " bytes)\n", sep = "")
  }
}
