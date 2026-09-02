# Shared entry-point plumbing for the run_step_*.R scripts.
#
# Each step is a thin script: resolve this file, call start_step() with the
# analysis modules it needs, then do its own work. start_step() sources the
# modules, builds the config, and returns the loaded data frame -- or exits
# cleanly if the private workbooks are not present, so the scripts remain
# runnable in a clone that has no data.

# Directory holding the run_step_*.R scripts, whether invoked by Rscript
# (--file=... is set) or sourced interactively from the repository root.
step_dir <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
  path <- if (is.na(arg)) file.path(getwd(), "Analysis_R", ".") else sub("^--file=", "", arg)
  dirname(normalizePath(path, winslash = "/", mustWork = FALSE))
}

# Modules always available to a step, in source order.
BASE_MODULES <- c("00_utils", "00_config", "01_load_data", "99_write_csv")

source_modules <- function(modules, dir = step_dir()) {
  for (module in unique(c(BASE_MODULES, modules))) {
    source(file.path(dir, "R", paste0(module, ".R")))
  }
  invisible(modules)
}

start_step <- function(modules = character(), require_data = TRUE) {
  source_modules(modules)

  config <- analysis_config()
  ensure_dir(config$output_dir)

  if (require_data && !inputs_available(config)) {
    cat("SKIP: raw workbook inputs are not available.\n")
    cat("Expected master workbook: ", config$master_xlsx, "\n", sep = "")
    cat("Expected ET-CO2 workbook: ", config$etco2_xlsx, "\n", sep = "")
    cat("Set TCD_MASTER_XLSX and TCD_ETCO2_XLSX, then rerun this step.\n")
    quit(status = 0)
  }

  df <- load_tcd_data(config)
  cat("Loaded rows: ", nrow(df), "\n", sep = "")
  cat("Loaded columns: ", ncol(df), "\n", sep = "")
  cat("Patched ET-CO2 cells: ", attr(df, "patched_n"), "\n", sep = "")

  list(config = config, df = df)
}
