# Small helpers shared across every module.
# Sourced first; depends on nothing.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Numeric coercion that stays quiet on the many text cells in the workbooks.
to_num <- function(x) suppressWarnings(as.numeric(x))

fmt_0    <- function(x) sprintf("%.0f", as.numeric(x))
fmt_1    <- function(x) sprintf("%.1f", as.numeric(x))
fmt_sig3 <- function(x) if (is.na(x)) "" else sprintf("%.3g", x)

# type = 7 matches the default used throughout the analysis.
quantile_np <- function(x, prob) {
  as.numeric(stats::quantile(x, probs = prob, type = 7, names = FALSE))
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

# First name in `candidates` that exists in `df`. The workbooks have been
# relabelled over time, so most columns are looked up through a candidate list.
pick_col <- function(df, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  if (required) {
    stop("Could not find any expected column: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  NULL
}
