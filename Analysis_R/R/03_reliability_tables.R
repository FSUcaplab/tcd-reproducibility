# Between-visit reliability of the 15 cardiovascular / cerebrovascular
# variables: paired summaries, Bland-Altman statistics, ICC(3,k) and Lin's CCC.
#
# The numeric kernels (icc_ccc, lin_ccc, linregress, spearman_scipy) are
# deliberately written out rather than delegated to a package: they reproduce
# the scipy/pingouin results of the original Python pipeline exactly, and the
# published numbers depend on that parity. Do not swap them for lm()/cor.test().

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ============================================================
# Exclusions and variable registry
# ============================================================

# Participant 45 is excluded from every analysis.
GLOBAL_EXCL_IDS <- c(45L)

# Per-variable exclusions, by subject-visit ID (signal quality / artefact).
var_excl <- list(
  etco2      = c("F02V1", "M24V1", "K32V1/F10V1", "K05V1"),
  mcav_peak  = c("K10V1", "F02V1", "M24V1"),
  mcav_min   = c("K10V1", "F02V1", "M24V1"),
  mcav_mean  = c("K10V1", "F02V1", "M24V1"),
  mcav_pulse = c("K10V1", "F02V1", "M24V1"),
  mcav_gpi   = c("K10V1", "F02V1", "M24V1"),
  cvci       = c("K10V1", "F02V1", "M24V1"),
  cvri       = c("M26V1", "F17V1", "F15V1", "M02V1", "F27V1/M16", "K05V1", "F26V2", "M21V1"),
  smo2       = c("K05V1", "K19V1", "F11V1", "F33V1", "M24V1"),
  q          = character(),
  tpr        = c("F05V1"),
  sbp        = character(),
  dbp        = character(),
  mbp        = character(),
  hr         = character()
)

epochs <- c("Base", "1min", "2min")
epoch_label <- c(Base = "Baseline", `1min` = "Min 1", `2min` = "Min 2")

all15 <- tibble::tribble(
  ~var_key,     ~stem,           ~label,             ~units,
  "sbp",        "SBP",           "SBP",              "mmHg",
  "dbp",        "DBP",           "DBP",              "mmHg",
  "mbp",        "MBP",           "MBP",              "mmHg",
  "hr",         "HR",            "HR",               "bpm",
  "q",          "Q",             "Q (cardiac)",      "L/min",
  "tpr",        "TPR",           "TPR",              "dynes*s/cm5",
  "etco2",      "ET-CO2",        "ET-CO2",           "mmHg",
  "mcav_peak",  "MCAv peak",     "MCAv peak",        "cm/s",
  "mcav_min",   "MCAv minimum",  "MCAv min",         "cm/s",
  "mcav_mean",  "MCAv mean",     "MCAv mean",        "cm/s",
  "mcav_pulse", "MCAv pulse",    "MCAv pulse",       "cm/s",
  "mcav_gpi",   "MCAv GPI",      "MCAv pulsatility", "ratio",
  "cvci",       "MCAv CVCi",     "CVCi",             "cm/s/mmHg",
  "cvri",       "MCAv Resis",    "CVRi",             "mmHg*s/cm",
  "smo2",       "SmO2",          "SmO2",             "%"
)

# Columns that stay character when a results table is coerced to numeric.
TEXT_COLS <- c(
  "Variable", "Epoch", "Group", "LoA_type", "Comparison_Test", "Effect_type",
  "Visit_1", "Visit_1_summary", "Visit_2", "Visit_2_summary",
  "Fixed_Bias", "Fixed_Bias_summary"
)

# Coerce every non-text column of a results table to numeric.
numify <- function(df) {
  for (col in setdiff(names(df), TEXT_COLS)) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df
}

# ============================================================
# Building paired visit-1 / visit-2 vectors
# ============================================================

# Values for one visit/epoch across every row of df.
# Baseline is the absolute resting value; minutes 1 and 2 are deltas from it,
# so a "delta" baseline is zero by construction.
epoch_values <- function(df, visit, stem, epoch, delta_baseline = FALSE) {
  if (epoch == "Base") {
    if (isTRUE(delta_baseline)) {
      return(rep(0, nrow(df)))
    }
    return(to_num(df[[paste(visit, stem, "Base")]]))
  }
  to_num(df[[paste(visit, "Delta", stem, "CPT", epoch)]])
}

# Rows surviving the global and per-variable exclusions.
included_rows <- function(df, var_key) {
  pid <- as.integer(df[["1 Identifier"]])
  sid <- as.character(df[["1 Subject ID"]])
  !(pid %in% GLOBAL_EXCL_IDS) & !(sid %in% var_excl[[var_key]])
}

# Paired V1/V2 values for one variable and epoch, dropping any participant
# missing either visit. `subset_fun` is a predicate over the whole data frame
# returning one logical per row (see 05_sensitivity_tables.R).
build_paired <- function(df, var_key, stem, epoch, delta_baseline = FALSE, subset_fun = NULL) {
  keep <- included_rows(df, var_key)
  if (!is.null(subset_fun)) {
    keep <- keep & subset_fun(df)
  }

  v1 <- epoch_values(df, 1, stem, epoch, delta_baseline)
  v2 <- epoch_values(df, 2, stem, epoch, delta_baseline)
  keep <- keep & !is.na(v1) & !is.na(v2)

  data.frame(
    pid = as.integer(df[["1 Identifier"]])[keep],
    sid = as.character(df[["1 Subject ID"]])[keep],
    v1  = as.numeric(v1)[keep],
    v2  = as.numeric(v2)[keep],
    stringsAsFactors = FALSE
  )
}

# ============================================================
# Numeric kernels
# ============================================================

# Lin's concordance correlation coefficient (population form, as in pingouin).
lin_ccc <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- mean((x - mx)^2)
  vy <- mean((y - my)^2)
  cov_xy <- mean((x - mx) * (y - my))
  denom <- vx + vy + (mx - my)^2
  if (denom == 0) NA_real_ else (2 * cov_xy) / denom
}

# ICC(3,k) with its F-test and 95% CI, plus Lin's CCC and a Fisher-z p-value.
# Two-way mixed model, average of the k = 2 visits.
icc_ccc <- function(dp) {
  n <- nrow(dp)
  if (n < 4) {
    return(list(ICC3k = NA_real_, ICC3k_lo = NA_real_, ICC3k_hi = NA_real_,
                ICC3k_p = NA_real_, CCC = NA_real_, CCC_p = NA_real_))
  }

  k <- 2
  y <- as.matrix(dp[, c("v1", "v2")])
  grand <- mean(y)

  ss_targets <- k * sum((rowMeans(y) - grand)^2)
  ss_raters <- n * sum((colMeans(y) - grand)^2)
  ss_error <- sum((y - grand)^2) - ss_targets - ss_raters

  df_targets <- n - 1
  df_error <- (n - 1) * (k - 1)
  msb <- ss_targets / df_targets
  mse <- ss_error / df_error

  f3k <- msb / mse
  icc3k <- (msb - mse) / msb
  pval <- stats::pf(f3k, df_targets, df_error, lower.tail = FALSE)

  alpha <- 0.05
  f_lower <- f3k / stats::qf(1 - alpha / 2, df_targets, df_error)
  f_upper <- f3k * stats::qf(1 - alpha / 2, df_error, df_targets)

  ccc_val <- as.numeric(lin_ccc(dp$v1, dp$v2))
  ccc_p <- if (is.na(ccc_val)) {
    NA_real_
  } else {
    as.numeric(2 * stats::pnorm(abs(atanh(ccc_val) * sqrt(n - 3)), lower.tail = FALSE))
  }

  list(
    ICC3k    = as.numeric(icc3k),
    ICC3k_lo = round(1 - 1 / f_lower, 2),
    ICC3k_hi = round(1 - 1 / f_upper, 2),
    ICC3k_p  = as.numeric(pval),
    CCC      = ccc_val,
    CCC_p    = ccc_p
  )
}

# Ordinary least squares slope/intercept with a correlation t-test,
# matching scipy.stats.linregress.
linregress <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(slope = NA_real_, intercept = NA_real_, r = NA_real_, p = NA_real_))
  }

  slope <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  r <- stats::cor(x, y)
  t_val <- r * sqrt((n - 2) / (1 - r^2))

  list(
    slope     = as.numeric(slope),
    intercept = as.numeric(mean(y) - slope * mean(x)),
    r         = as.numeric(r),
    p         = as.numeric(2 * stats::pt(abs(t_val), df = n - 2, lower.tail = FALSE))
  )
}

# Spearman rho with the t-approximation p-value used by scipy.stats.spearmanr.
spearman_scipy <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(rho = NA_real_, p = NA_real_))
  }

  rho <- stats::cor(rank(x, ties.method = "average"), rank(y, ties.method = "average"))
  t_val <- rho * sqrt((n - 2) / ((1 + rho) * (1 - rho)))

  list(
    rho = as.numeric(rho),
    p   = as.numeric(2 * stats::pt(abs(t_val), df = n - 2, lower.tail = FALSE))
  )
}

# Multiplier turning a standard deviation into 95% limits of agreement.
LOA_MULT <- 1.96

# Bland-Altman statistics. The limits of agreement adapt to the data:
#   standard        - constant bias, constant spread
#   regression      - bias varies with the mean (proportional bias)
#   fan             - spread varies with the mean (heteroscedasticity)
#   regression+fan  - both
# Proportional bias and heteroscedasticity are each accepted at P < 0.05.
ba_stats <- function(dp) {
  n <- nrow(dp)
  if (n < 4) {
    return(list(
      n = n, Mean_diff = NA_real_, SD_diff = NA_real_, LoA_type = NA_character_,
      LoA_lo = NA_real_, LoA_hi = NA_real_, Prop_bias_r = NA_real_,
      Prop_bias_P = NA_real_, Hetero_rho = NA_real_, Hetero_P = NA_real_
    ))
  }

  mean_val <- (dp$v1 + dp$v2) / 2
  diff_val <- dp$v1 - dp$v2
  md <- mean(diff_val)
  sd_diff <- stats::sd(diff_val)

  pb <- linregress(mean_val, diff_val)
  use_pb <- !is.na(pb$p) && pb$p < 0.05
  fitted <- if (use_pb) pb$intercept + pb$slope * mean_val else rep(md, n)
  resid <- diff_val - fitted

  hs <- spearman_scipy(mean_val, abs(resid))
  use_hs <- !is.na(hs$p) && hs$p < 0.05

  x_mid <- mean(mean_val)
  mid <- if (use_pb) pb$intercept + pb$slope * x_mid else md

  if (use_hs) {
    # Spread modelled as a line through the absolute residuals, floored so a
    # steep negative slope cannot drive the limits to zero width.
    sd_fit <- linregress(mean_val, abs(resid))
    sd_mid <- max(sd_fit$intercept + sd_fit$slope * x_mid, max(mean(abs(resid)) * 0.25, 1e-6))
    loa_type <- if (use_pb) "regression+fan" else "fan"
  } else if (use_pb) {
    sd_mid <- stats::sd(resid) * sqrt((n - 1) / (n - 2))
    loa_type <- "regression"
  } else {
    sd_mid <- sd_diff
    loa_type <- "standard"
  }

  list(
    n           = n,
    Mean_diff   = as.numeric(md),
    SD_diff     = as.numeric(sd_diff),
    LoA_type    = loa_type,
    LoA_lo      = as.numeric(mid - LOA_MULT * sd_mid),
    LoA_hi      = as.numeric(mid + LOA_MULT * sd_mid),
    Prop_bias_r = as.numeric(pb$r),
    Prop_bias_P = as.numeric(pb$p),
    Hetero_rho  = as.numeric(hs$rho),
    Hetero_P    = as.numeric(hs$p)
  )
}

# ============================================================
# Summary formatting
# ============================================================

mean_sd_string <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 3) {
    return(list(value = "", kind = "mean_sd"))
  }
  list(value = paste0(fmt_sig3(mean(x)), " +/- ", fmt_sig3(stats::sd(x))), kind = "mean_sd")
}

median_iqr_string <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 3) {
    return(list(value = "", kind = "median_iqr"))
  }
  iqr <- quantile_np(x, 0.75) - quantile_np(x, 0.25)
  list(value = paste0(fmt_sig3(stats::median(x)), " [", fmt_sig3(iqr), "]"), kind = "median_iqr")
}

# Mean +/- SD if Shapiro-Wilk does not reject normality, else median [IQR].
summary_string <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 3) {
    return(list(value = "", kind = ""))
  }
  if (stats::sd(x) == 0) {
    return(mean_sd_string(x))
  }
  p_norm <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
  if (!is.na(p_norm) && p_norm > 0.05) mean_sd_string(x) else median_iqr_string(x)
}

# ============================================================
# Paired comparison
# ============================================================

paired_rank_biserial <- function(diff) {
  diff <- as.numeric(diff)
  diff <- diff[!is.na(diff) & diff != 0]
  if (length(diff) == 0) {
    return(NA_real_)
  }
  ranks <- rank(abs(diff), ties.method = "average")
  pos <- sum(ranks[diff > 0])
  neg <- sum(ranks[diff < 0])
  if (pos + neg == 0) NA_real_ else (pos - neg) / (pos + neg)
}

# Paired t-test when the differences pass Shapiro-Wilk, else Wilcoxon signed-rank.
paired_comparison <- function(dp) {
  if (nrow(dp) < 4) {
    return(list())
  }

  diff <- dp$v1 - dp$v2
  norm_p <- tryCatch(stats::shapiro.test(diff)$p.value, error = function(e) NA_real_)

  if (!is.na(norm_p) && norm_p > 0.05) {
    return(list(
      Test        = "paired t",
      P           = as.numeric(stats::t.test(dp$v1, dp$v2, paired = TRUE)$p.value),
      Effect      = if (stats::sd(diff) == 0) NA_real_ else as.numeric(mean(diff) / stats::sd(diff)),
      Effect_type = "paired d"
    ))
  }

  # Prefer the exact test; fall back to the normal approximation when it fails.
  p <- tryCatch(
    stats::wilcox.test(dp$v1, dp$v2, paired = TRUE, exact = TRUE, correct = FALSE)$p.value,
    error = function(e) tryCatch(
      stats::wilcox.test(dp$v1, dp$v2, paired = TRUE, exact = FALSE, correct = FALSE)$p.value,
      error = function(e2) NA_real_
    )
  )
  list(
    Test        = "Wilcoxon",
    P           = as.numeric(p),
    Effect      = as.numeric(paired_rank_biserial(diff)),
    Effect_type = "rank biserial"
  )
}

# ============================================================
# Table assembly
# ============================================================

# Visit-1, visit-2 and fixed-bias summary strings. Both visits are reported in
# the same style -- median [IQR] wins if either asks for it -- and the fixed
# bias follows the paired-difference distribution, matching the paired test.
paired_summaries <- function(dp, comp) {
  v1s <- summary_string(dp$v1)
  v2s <- summary_string(dp$v2)
  if (any(c(v1s$kind, v2s$kind) == "median_iqr")) {
    v1s <- median_iqr_string(dp$v1)
    v2s <- median_iqr_string(dp$v2)
  }
  diffs <- if (identical(comp$Test, "paired t")) {
    mean_sd_string(dp$v1 - dp$v2)
  } else {
    median_iqr_string(dp$v1 - dp$v2)
  }
  list(v1 = v1s, v2 = v2s, diff = diffs)
}

# Mean absolute error, mean absolute percentage error, and coefficient of
# variation between the two visits.
paired_error <- function(dp) {
  abs_diff <- abs(dp$v1 - dp$v2)
  pair_mean <- (dp$v1 + dp$v2) / 2
  denom <- abs(pair_mean)
  denom[denom == 0] <- NA_real_
  list(
    mae  = mean(abs_diff),
    mape = mean(abs_diff / denom, na.rm = TRUE) * 100,
    cv   = stats::sd(dp$v1 - dp$v2) / mean(pair_mean) * 100
  )
}

# One row of Table 2 / Table 3: visit summaries, fixed bias, error metrics,
# Bland-Altman statistics and agreement coefficients for one variable-epoch.
agreement_row <- function(df, var_key, stem, label, epoch) {
  dp <- build_paired(df, var_key, stem, epoch)
  ba <- ba_stats(dp)
  icc <- icc_ccc(dp)
  comp <- paired_comparison(dp)

  if (nrow(dp) > 0) {
    s <- paired_summaries(dp, comp)
    v1s <- s$v1
    v2s <- s$v2
    diffs <- s$diff
    err <- paired_error(dp)
    mae <- err$mae
    mape <- err$mape
    cv <- err$cv
  } else {
    v1s <- v2s <- diffs <- list(value = "", kind = "")
    mae <- mape <- cv <- NA_real_
  }

  c(
    list(
      Variable           = label,
      Epoch              = unname(epoch_label[[epoch]]),
      n                  = nrow(dp),
      Visit_1            = v1s$value,
      Visit_1_summary    = v1s$kind,
      Visit_2            = v2s$value,
      Visit_2_summary    = v2s$kind,
      Fixed_Bias         = diffs$value,
      Fixed_Bias_summary = diffs$kind,
      Comparison_Test    = comp$Test %||% "",
      Comparison_P       = comp$P %||% NA_real_,
      Effect             = comp$Effect %||% NA_real_,
      Effect_type        = comp$Effect_type %||% "",
      MAE                = as.numeric(mae),
      `MAPE_%`           = as.numeric(mape),
      `CV_%`             = as.numeric(cv)
    ),
    ba[setdiff(names(ba), "n")],
    icc
  )
}

# Columns of the statistics table that drives Figures 2-4.
STATS_COLS <- c(
  "Variable", "Epoch", "n", "Mean_diff", "SD_diff", "LoA_type", "LoA_lo", "LoA_hi",
  "Prop_bias_r", "Prop_bias_P", "Hetero_rho", "Hetero_P",
  "ICC3k", "ICC3k_lo", "ICC3k_hi", "ICC3k_p", "CCC", "CCC_p"
)

build_tables_and_stats_r <- function(df) {
  rows <- list()
  for (i in seq_len(nrow(all15))) {
    var <- all15[i, ]
    for (epoch in epochs) {
      row <- agreement_row(df, var$var_key, var$stem, var$label, epoch)
      rows[[length(rows) + 1]] <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
    }
  }

  rel <- numify(dplyr::bind_rows(rows))

  list(
    rel    = rel,
    table2 = rel[rel$Epoch == "Baseline", , drop = FALSE],
    table3 = rel[rel$Epoch %in% c("Min 1", "Min 2"), , drop = FALSE],
    stats  = rel[, STATS_COLS, drop = FALSE]
  )
}
