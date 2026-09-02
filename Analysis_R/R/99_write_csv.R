# CSV serialisation.
#
# The pipeline writes its own CSVs rather than using write.csv() so that output
# is byte-stable: no row names, no scientific-notation drift, logicals as
# True/False, and minimal quoting. Byte stability is what makes the regression
# test in tests/ a meaningful check.

quote_csv_field <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  needs_quote <- grepl('[,"\n\r]', x)
  x <- gsub('"', '""', x, fixed = TRUE)
  ifelse(needs_quote, paste0('"', x, '"'), x)
}

write_csv_minimal <- function(df, path) {
  header <- paste(quote_csv_field(names(df)), collapse = ",")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    vals <- vapply(df, function(col) {
      val <- col[i]
      if (is.logical(val) && !is.na(val)) {
        return(if (isTRUE(val)) "True" else "False")
      }
      as.character(val)
    }, character(1))
    paste(quote_csv_field(vals), collapse = ",")
  }, character(1))
  writeLines(c(header, rows), path, useBytes = TRUE)
  invisible(path)
}

# Writes a named list of data frames as <name>.csv into the output directory.
write_outputs <- function(outputs, config) {
  for (name in names(outputs)) {
    path <- file.path(config$output_dir, paste0(name, ".csv"))
    write_csv_minimal(outputs[[name]], path)
    cat("Wrote: ", basename(path), "\n", sep = "")
  }
  invisible(names(outputs))
}
