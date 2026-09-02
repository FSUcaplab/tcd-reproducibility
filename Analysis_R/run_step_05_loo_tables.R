# Step 05 - Leave-one-participant-out influence analysis on ICC and CCC.

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step(c("03_reliability_tables", "05_sensitivity_tables"))

loo <- build_loo_influence_tables_r(step$df)

write_outputs(
  list(
    SupplementaryTable_Reliability_Influence_LOO         = loo$out,
    SupplementaryTable_Reliability_Influence_LOO_Flagged = loo$flagged
  ),
  step$config
)
