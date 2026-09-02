# Step 04 - Reliability split by sex and menstrual-cycle matching (Figure 5),
# and the same statistics with unmatched females excluded (Figure 6).

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step(c("03_reliability_tables", "05_sensitivity_tables"))

write_outputs(
  list(
    Fig5_ICC_CCC_Sex_Menstrual_Stats = build_sex_reliability_r(step$df),
    Fig6_ICC_CCC_Exclude_Unmatched_Females_Stats = build_reliability_stats_for_vars_r(
      step$df, fig2_fig3_vars,
      subset_fun = exclude_unmatched_females
    )
  ),
  step$config
)
