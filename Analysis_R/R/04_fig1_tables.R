# Figure 1 - CPT time course: per-visit summaries, a two-way repeated-measures
# ANOVA (visit x time), and Bonferroni-corrected post-hoc comparisons of time.
#
# Values here are deltas from each visit's own baseline, so baseline is 0.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

fig1_vars <- tibble::tribble(
  ~var_key,    ~stem,        ~ylabel,
  "sbp",       "SBP",        "Delta SBP (mmHg)",
  "dbp",       "DBP",        "Delta DBP (mmHg)",
  "mbp",       "MBP",        "Delta MBP (mmHg)",
  "hr",        "HR",         "Delta HR (bpm)",
  "mcav_mean", "MCAv mean",  "Delta MCAv mean (cm/s)",
  "cvci",      "MCAv CVCi",  "Delta CVCi (cm/s/mmHg)",
  "mcav_gpi",  "MCAv GPI",   "Delta MCAv pulsatility",
  "etco2",     "ET-CO2",     "Delta ET-CO2 (mmHg)"
)

TIME_LEVELS <- c("Baseline", "Min 1", "Min 2")

# "Delta SBP (mmHg)" -> "SBP"
fig1_var_label <- function(ylabel) {
  sub(" \\(.*$", "", sub("^Delta ", "", ylabel))
}

# One row per participant x visit x epoch, missing values dropped.
# Rows stay in workbook order, nested visit-then-epoch within participant;
# build_fig1_gg() jitters points from a fixed seed, so this order is load-bearing.
long_for_var <- function(df, var_key, stem, delta_baseline = TRUE, subset_fun = NULL) {
  keep <- included_rows(df, var_key)
  if (!is.null(subset_fun)) {
    keep <- keep & subset_fun(df)
  }
  idx <- which(keep)

  # Columns ordered epoch-within-visit, matching the layout expanded below.
  values <- do.call(cbind, lapply(c(1, 2), function(visit) {
    vapply(epochs, function(epoch) epoch_values(df, visit, stem, epoch, delta_baseline),
           numeric(nrow(df)))
  }))

  n_epochs <- length(epochs)
  out <- data.frame(
    pid   = rep(as.integer(df[["1 Identifier"]])[idx], each = 2 * n_epochs),
    visit = rep(rep(c("V1", "V2"), each = n_epochs), times = length(idx)),
    time  = rep(rep(unname(epoch_label[epochs]), 2), times = length(idx)),
    epoch = rep(rep(epochs, 2), times = length(idx)),
    value = as.vector(t(values[idx, , drop = FALSE])),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$value), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Centre and error bars per variable x visit x epoch, using mean +/- SD or
# median [IQR] according to the same normality rule as the tables.
build_fig1_timecourse_summary <- function(df) {
  rows <- list()

  for (i in seq_len(nrow(fig1_vars))) {
    var <- fig1_vars[i, ]
    long <- long_for_var(df, var$var_key, var$stem)
    variable <- fig1_var_label(var$ylabel)

    for (visit in c("V1", "V2")) {
      for (epoch in epochs) {
        y <- long$value[long$visit == visit & long$epoch == epoch]

        if (length(y) == 0) {
          center <- NA_real_
          err_lo <- err_hi <- 0
          kind <- ""
        } else {
          kind <- summary_string(y)$kind
          if (kind == "mean_sd") {
            center <- mean(y)
            err_lo <- err_hi <- stats::sd(y)
          } else {
            center <- stats::median(y)
            err_lo <- center - quantile_np(y, 0.25)
            err_hi <- quantile_np(y, 0.75) - center
          }
        }

        rows[[length(rows) + 1]] <- data.frame(
          Variable = variable,
          Visit    = visit,
          Epoch    = unname(epoch_label[[epoch]]),
          Center   = center,
          Err_lo   = err_lo,
          Err_hi   = err_hi,
          Summary  = kind,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  dplyr::bind_rows(rows)
}

# Two-way (2 visits x 3 times) repeated-measures ANOVA on complete cases.
# Written out rather than delegated to aov() so the sums of squares -- and the
# floating-point order they accumulate in -- match the original Python results.
rm_anova_two_by_three <- function(long) {
  visit_levels <- c("V1", "V2")
  pids <- sort(unique(long$pid))

  y <- array(NA_real_, dim = c(length(pids), length(visit_levels), length(TIME_LEVELS)))
  for (s in seq_along(pids)) {
    for (ia in seq_along(visit_levels)) {
      for (ib in seq_along(TIME_LEVELS)) {
        vals <- long$value[long$pid == pids[s] & long$visit == visit_levels[ia] & long$time == TIME_LEVELS[ib]]
        if (length(vals) > 0) {
          y[s, ia, ib] <- mean(vals)
        }
      }
    }
  }

  y <- y[apply(y, 1, function(x) all(!is.na(x))), , , drop = FALSE]
  n <- dim(y)[1]
  a <- length(visit_levels)
  b <- length(TIME_LEVELS)

  grand <- mean(y)
  mean_s <- apply(y, 1, mean)
  mean_a <- apply(y, 2, mean)
  mean_b <- apply(y, 3, mean)
  mean_ab <- apply(y, c(2, 3), mean)
  mean_sa <- apply(y, c(1, 2), mean)
  mean_sb <- apply(y, c(1, 3), mean)

  ss_a <- n * b * sum((mean_a - grand)^2)
  ss_b <- n * a * sum((mean_b - grand)^2)
  ss_ab <- n * sum((mean_ab - outer(mean_a, mean_b, "+") + grand)^2)

  ss_sa <- 0
  for (s in seq_len(n)) {
    for (ia in seq_len(a)) {
      ss_sa <- ss_sa + b * (mean_sa[s, ia] - mean_s[s] - mean_a[ia] + grand)^2
    }
  }

  ss_sb <- 0
  for (s in seq_len(n)) {
    for (ib in seq_len(b)) {
      ss_sb <- ss_sb + a * (mean_sb[s, ib] - mean_s[s] - mean_b[ib] + grand)^2
    }
  }

  ss_sab <- 0
  for (s in seq_len(n)) {
    for (ia in seq_len(a)) {
      for (ib in seq_len(b)) {
        resid <- y[s, ia, ib] - mean_sa[s, ia] - mean_sb[s, ib] - mean_ab[ia, ib] +
          mean_s[s] + mean_a[ia] + mean_b[ib] - grand
        ss_sab <- ss_sab + resid^2
      }
    }
  }

  effects <- data.frame(
    Effect   = c("visit", "time", "visit:time"),
    ss       = c(ss_a, ss_b, ss_ab),
    ss_error = c(ss_sa, ss_sb, ss_sab),
    df       = c(a - 1, b - 1, (a - 1) * (b - 1)),
    df_error = c((n - 1) * (a - 1), (n - 1) * (b - 1), (n - 1) * (a - 1) * (b - 1)),
    stringsAsFactors = FALSE
  )
  effects$F <- (effects$ss / effects$df) / (effects$ss_error / effects$df_error)
  effects$P <- stats::pf(effects$F, effects$df, effects$df_error, lower.tail = FALSE)
  effects$partial_eta_sq <- (effects$F * effects$df) / ((effects$F * effects$df) + effects$df_error)
  effects$n_participants <- n
  effects$n_observations <- n * a * b
  effects
}

# Pairwise comparisons of the three time points, pooled over visit.
build_fig1_posthoc <- function(df, bonferroni_m = 3L) {
  comparisons <- list(
    c("Baseline", "Min 1"),
    c("Baseline", "Min 2"),
    c("Min 1",    "Min 2")
  )
  rows <- list()

  for (i in seq_len(nrow(fig1_vars))) {
    var <- fig1_vars[i, ]
    long <- long_for_var(df, var$var_key, var$stem)
    variable <- fig1_var_label(var$ylabel)

    # Main effect of time: average each participant over their two visits.
    pooled <- aggregate(value ~ pid + time, data = long, FUN = mean)

    for (comp in comparisons) {
      a_rows <- pooled[pooled$time == comp[1], ]
      b_rows <- pooled[pooled$time == comp[2], ]
      common <- intersect(a_rows$pid, b_rows$pid)
      ta <- a_rows$value[match(common, a_rows$pid)]
      tb <- b_rows$value[match(common, b_rows$pid)]
      diff_ab <- ta - tb

      norm_p <- if (length(common) >= 3) {
        tryCatch(stats::shapiro.test(diff_ab)$p.value, error = function(e) NA_real_)
      } else {
        NA_real_
      }

      if (!is.na(norm_p) && norm_p > 0.05) {
        p_raw <- as.numeric(stats::t.test(ta, tb, paired = TRUE)$p.value)
        effect <- if (stats::sd(diff_ab) > 0) mean(diff_ab) / stats::sd(diff_ab) else NA_real_
        test_name <- "paired t"
        effect_type <- "Cohen d"
      } else {
        p_raw <- tryCatch(
          stats::wilcox.test(ta, tb, paired = TRUE, exact = FALSE, correct = FALSE)$p.value,
          error = function(e) NA_real_
        )
        effect <- as.numeric(paired_rank_biserial(diff_ab))
        test_name <- "Wilcoxon"
        effect_type <- "rank biserial"
      }

      rows[[length(rows) + 1]] <- data.frame(
        Variable     = variable,
        Comparison   = paste0(comp[1], " vs ", comp[2]),
        Test         = test_name,
        n            = length(common),
        P_raw        = p_raw,
        P_bonferroni = min(p_raw * bonferroni_m, 1.0),
        Effect       = effect,
        Effect_type  = effect_type,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )
    }
  }

  dplyr::bind_rows(rows)
}

build_fig1_anova <- function(df) {
  rows <- list()

  for (i in seq_len(nrow(fig1_vars))) {
    var <- fig1_vars[i, ]
    long <- long_for_var(df, var$var_key, var$stem)
    variable <- fig1_var_label(var$ylabel)
    n_available <- length(unique(long$pid))

    # The ANOVA needs all six visit x time cells present for a participant.
    cell_counts <- long %>% distinct(pid, visit, time) %>% count(pid, name = "n_cells")
    complete <- long[long$pid %in% cell_counts$pid[cell_counts$n_cells == 6], , drop = FALSE]

    tab <- rm_anova_two_by_three(complete)
    for (r in seq_len(nrow(tab))) {
      rows[[length(rows) + 1]] <- data.frame(
        Variable                 = variable,
        Test                     = "two-way repeated-measures ANOVA",
        Effect                   = tab$Effect[r],
        F                        = tab$F[r],
        `Num DF`                 = tab$df[r],
        `Den DF`                 = tab$df_error[r],
        P                        = tab$P[r],
        partial_eta_sq           = tab$partial_eta_sq[r],
        n_participants           = tab$n_participants[r],
        n_participants_available = n_available,
        n_observations           = tab$n_observations[r],
        check.names              = FALSE,
        stringsAsFactors         = FALSE
      )
    }
  }

  dplyr::bind_rows(rows)
}
