# Test runner.
#
#   Rscript tests/run_tests.R
#
# Exits non-zero if anything fails, so it can gate a commit or a CI job.

.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) {
  dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE))
} else {
  file.path(getwd(), "tests")
}

source(file.path(.here, "assert.R"))

# Load the analysis modules under test.
analysis_r <- file.path(dirname(.here), "Analysis_R")
source(file.path(analysis_r, "R", "_setup.R"))
source_modules(
  c("02_table1_characteristics", "03_reliability_tables", "04_fig1_tables",
    "05_sensitivity_tables", "06_figures"),
  dir = analysis_r
)

cat("TCD reproducibility - test suite\n")
cat("R ", R.version.string, "\n", sep = "")

source(file.path(.here, "test_kernels.R"))
source(file.path(.here, "test_outputs.R"))

quit(status = report())
