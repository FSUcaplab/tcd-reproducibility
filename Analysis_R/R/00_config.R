# Paths. Every location is derived from the repository root so the pipeline
# runs unchanged from any working directory, and each input can be overridden
# with an environment variable.

find_repo_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    is_root <- file.exists(file.path(path, "Analysis_R", "R", "03_reliability_tables.R")) &&
      file.exists(file.path(path, "README.md"))
    if (is_root) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not find repository root from: ", start, call. = FALSE)
    }
    path <- parent
  }
}

analysis_config <- function(start = getwd()) {
  root <- find_repo_root(start)
  analysis_dir <- file.path(root, "Analysis_R")
  data_dir <- Sys.getenv("TCD_DATA_DIR", file.path(root, "data", "raw"))

  list(
    root         = root,
    analysis_dir = analysis_dir,
    output_dir   = Sys.getenv("TCD_R_OUTPUT_DIR", file.path(analysis_dir, "outputs")),
    data_dir     = data_dir,
    master_xlsx   = Sys.getenv("TCD_MASTER_XLSX",   file.path(data_dir, "CPT Data_visit split.xlsx")),
    etco2_xlsx    = Sys.getenv("TCD_ETCO2_XLSX",    file.path(data_dir, "CPT_ETCO2_Resp_Comparison.xlsx")),
    cpt_side_xlsx = Sys.getenv("TCD_CPT_SIDE_XLSX", file.path(data_dir, "CPT Data.xlsx"))
  )
}
