# Step 02 - Tables 2 and 3 (between-visit reliability of all 15 variables)
# and the Bland-Altman / ICC / CCC statistics behind Figures 2-4.

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step("03_reliability_tables")

tables <- build_tables_and_stats_r(step$df)

write_outputs(
  list(
    Table2_Baseline_All15_Reliability   = tables$table2,
    Table3_Min1_Min2_All15_Reliability  = tables$table3,
    BA_ICC_CCC_Statistics               = tables$stats,
    SupplementaryTable_BA_Statistics    = tables$stats
  ),
  step$config
)
