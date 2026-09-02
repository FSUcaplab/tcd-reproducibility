# A minimal test harness.
#
# Deliberately dependency-free: a reproducibility repository should be
# verifiable in a bare R installation, without first installing a test package.

TEST_STATE <- new.env(parent = emptyenv())
TEST_STATE$pass <- 0L
TEST_STATE$fail <- 0L
TEST_STATE$skip <- 0L
TEST_STATE$failures <- character(0)

section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

check <- function(desc, ok, detail = NULL) {
  if (isTRUE(ok)) {
    TEST_STATE$pass <- TEST_STATE$pass + 1L
    cat("  ok    ", desc, "\n", sep = "")
  } else {
    TEST_STATE$fail <- TEST_STATE$fail + 1L
    msg <- if (is.null(detail)) desc else paste0(desc, " -- ", detail)
    TEST_STATE$failures <- c(TEST_STATE$failures, msg)
    cat("  FAIL  ", msg, "\n", sep = "")
  }
  invisible(isTRUE(ok))
}

check_skip <- function(desc, reason) {
  TEST_STATE$skip <- TEST_STATE$skip + 1L
  cat("  skip  ", desc, " (", reason, ")\n", sep = "")
  invisible(NULL)
}

# Numeric comparison with a tolerance; NA equals NA.
check_near <- function(desc, actual, expected, tol = 1e-9) {
  ok <- isTRUE(all.equal(actual, expected, tolerance = tol, check.attributes = FALSE))
  check(desc, ok, if (ok) NULL else paste0("got ", format(actual), ", expected ", format(expected)))
}

check_equal <- function(desc, actual, expected) {
  ok <- identical(actual, expected)
  check(desc, ok, if (ok) NULL else paste0("got ", paste(format(actual), collapse = ", "),
                                           ", expected ", paste(format(expected), collapse = ", ")))
}

# Prints the tally and returns the exit status for the runner.
report <- function() {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat(sprintf("passed %d   failed %d   skipped %d\n",
              TEST_STATE$pass, TEST_STATE$fail, TEST_STATE$skip))

  if (TEST_STATE$fail > 0) {
    cat("\nFailures:\n")
    for (f in TEST_STATE$failures) {
      cat("  - ", f, "\n", sep = "")
    }
  }
  cat(strrep("=", 60), "\n", sep = "")

  if (TEST_STATE$fail > 0) 1L else 0L
}
