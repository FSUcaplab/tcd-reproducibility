# Step 03 - Figure 1 time-course summary, its two-way repeated-measures ANOVA,
# and the Bonferroni-corrected post-hoc comparisons.

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step(c("03_reliability_tables", "04_fig1_tables"))

write_outputs(
  list(
    Fig1_TimeCourse_Summary       = build_fig1_timecourse_summary(step$df),
    Fig1_ANOVA_Time_Visit_Results = build_fig1_anova(step$df),
    Fig1_Posthoc_Bonferroni       = build_fig1_posthoc(step$df)
  ),
  step$config
)
