# Table 1 - participant characteristics.
# Age is reported as median [IQR] (range); the anthropometrics as
# mean +/- SD (range); sex, race and ethnicity as counts.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# "n Label, n Label, ..." ordered by `preferred_order`, then by descending count.
count_summary <- function(x, preferred_order = NULL) {
  vals <- x[!is.na(x) & x != ""]
  tab <- as.data.frame(table(vals), stringsAsFactors = FALSE)
  names(tab) <- c("value", "n")

  if (is.null(preferred_order)) {
    tab <- tab[order(-tab$n), ]
  } else {
    rank <- match(tab$value, preferred_order)
    tab <- tab[order(is.na(rank), rank, -tab$n, tab$value), ]
  }

  paste(paste(tab$n, tab$value), collapse = ", ")
}

median_iqr_summary <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  sprintf("%s [%s] (%s - %s)",
          fmt_1(stats::median(x)), fmt_1(stats::IQR(x)), fmt_1(min(x)), fmt_1(max(x)))
}

mean_sd_summary <- function(x, digits = 1) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  fmt <- if (digits == 0) fmt_0 else fmt_1
  sprintf("%s +/- %s (%s - %s)",
          fmt(mean(x)), fmt(stats::sd(x)), fmt(min(x)), fmt(max(x)))
}

RACE_LABELS <- c(
  W  = "White",
  A  = "Asian",
  BW = "Black or White (multi)",
  MR = "Multiracial",
  AI = "American Indian/Alaskan Native",
  B  = "Black or African American"
)

RACE_ORDER <- c(
  "White", "Asian", "Unknown", "Black or White (multi)", "Multiracial",
  "American Indian/Alaskan Native", "Black or African American"
)

ETHNICITY_LABELS <- c(
  NH = "Not Hispanic/Latine",
  H  = "Hispanic/Latine",
  "Not Hispanic or Latinx" = "Not Hispanic/Latine"
)

# Expand coded values to their display labels, leaving anything unrecognised as-is.
relabel <- function(x, labels) {
  x <- as.character(x)
  hit <- match(x, names(labels))
  ifelse(is.na(hit), x, unname(labels[hit]))
}

build_table1 <- function(df) {
  sex_col       <- pick_col(df, c("1 Sex", "Sex"))
  age_col       <- pick_col(df, c("1 Age", "Age"))
  height_col    <- pick_col(df, c("1 Screening Height (cm)", "1 Height", "Height", "Height, cm"))
  mass_col      <- pick_col(df, c("1 Screening Mass (kg)", "1 Weight", "1 Body mass", "Body mass", "Body mass, kg", "Weight"))
  bmi_col       <- pick_col(df, c("1 Screening BMI", "1 BMI", "BMI", "BMI, kg/m2"))
  race_col      <- pick_col(df, c("1 Race (AI/A/NHPI/BW/MR/Unknown)", "1 Race", "Race"))
  ethnicity_col <- pick_col(df, c("1 Ethnicity (H/NH/Unknown)", "1 Ethnicity", "Ethnicity"))

  analytic <- df %>% filter(!is.na(.data[[sex_col]]))

  race <- relabel(analytic[[race_col]], RACE_LABELS)
  race[is.na(race) | race == "NA"] <- "Unknown"
  ethnicity <- relabel(analytic[[ethnicity_col]], ETHNICITY_LABELS)

  tibble::tibble(
    Characteristic = c(
      "Sex", "Age, yrs", "Height, cm", "Body mass, kg", "BMI, kg/m2", "Race", "Ethnicity"
    ),
    Value = c(
      count_summary(as.character(analytic[[sex_col]]), preferred_order = c("Female", "Male")),
      median_iqr_summary(analytic[[age_col]]),
      mean_sd_summary(analytic[[height_col]], digits = 0),
      mean_sd_summary(analytic[[mass_col]]),
      mean_sd_summary(analytic[[bmi_col]]),
      count_summary(race, preferred_order = RACE_ORDER),
      count_summary(ethnicity)
    )
  )
}
