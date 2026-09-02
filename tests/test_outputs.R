# Regression test: re-run the pipeline into a scratch directory and require
# every table to come back byte-for-byte identical to the committed CSVs in
# Analysis_R/outputs/.
#
# This is the test that protects the published numbers. Refactoring is free to
# change how a table is produced, but not what it contains -- and because
# write_csv_minimal() is byte-stable, "identical" can mean exactly that.
#
# Figures are not compared here: PNG output depends on the system's fonts and
# graphics devices, so it is not portable across machines. Run step 06 and
# inspect the figures by eye instead.
#
# Skipped when the private workbooks are absent, so a clone without data still
# runs the kernel tests.

section("Pipeline regression against committed outputs")

config <- analysis_config()

if (!inputs_available(config)) {
  check_skip("pipeline reproduces every committed CSV", "raw workbooks not present")
} else {
  scratch <- file.path(tempdir(), "tcd_regression_outputs")
  unlink(scratch, recursive = TRUE)
  dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  steps <- c(
    "run_step_01_table1.R",
    "run_step_02_reliability_tables.R",
    "run_step_03_fig1_tables.R",
    "run_step_04_sensitivity_tables.R",
    "run_step_05_loo_tables.R",
    "run_step_07_regression_ancillary.R"
  )

  # system2(env = ) is a no-op on Windows, so redirect the output directory
  # through this process's environment, which the child steps inherit.
  previous_output_dir <- Sys.getenv("TCD_R_OUTPUT_DIR", unset = NA)
  Sys.setenv(TCD_R_OUTPUT_DIR = scratch)
  on.exit({
    if (is.na(previous_output_dir)) {
      Sys.unsetenv("TCD_R_OUTPUT_DIR")
    } else {
      Sys.setenv(TCD_R_OUTPUT_DIR = previous_output_dir)
    }
  }, add = TRUE)

  for (step in steps) {
    status <- system2(
      rscript,
      args = shQuote(file.path(config$analysis_dir, step)),
      stdout = FALSE, stderr = FALSE
    )
    check(paste0(step, " runs cleanly"), identical(as.integer(status), 0L),
          paste("exit status", status))
  }

  committed <- sort(list.files(config$output_dir, pattern = "\\.csv$"))
  regenerated <- sort(list.files(scratch, pattern = "\\.csv$"))

  # Every committed CSV must be reproducible. The reverse is checked too, so a
  # newly added output cannot slip in uncommitted.
  check("no committed CSV is missing from a fresh run",
        length(setdiff(committed, regenerated)) == 0,
        paste("missing:", paste(setdiff(committed, regenerated), collapse = ", ")))
  check("a fresh run produces no uncommitted CSV",
        length(setdiff(regenerated, committed)) == 0,
        paste("unexpected:", paste(setdiff(regenerated, committed), collapse = ", ")))

  for (name in intersect(committed, regenerated)) {
    old <- readBin(file.path(config$output_dir, name), "raw",
                   file.info(file.path(config$output_dir, name))$size)
    new <- readBin(file.path(scratch, name), "raw",
                   file.info(file.path(scratch, name))$size)
    check(paste0(name, " is unchanged"), identical(old, new),
          paste0(length(old), " vs ", length(new), " bytes"))
  }
}
