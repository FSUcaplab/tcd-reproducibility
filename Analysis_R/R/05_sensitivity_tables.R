# Sensitivity analyses on the reliability estimates:
#   - split by sex and menstrual-cycle matching (Figure 5)
#   - recomputed with unmatched females excluded (Figure 6)
#   - leave-one-participant-out influence on ICC and CCC (supplementary)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# The eight variables carried into Figures 2, 3, 5 and 6.
fig2_fig3_vars <- tibble::tribble(
  ~var_key,    ~stem,        ~label,             ~units,
  "sbp",       "SBP",        "SBP",              "mmHg",
  "dbp",       "DBP",        "DBP",              "mmHg",
  "mbp",       "MBP",        "MBP",              "mmHg",
  "hr",        "HR",         "HR",               "bpm",
  "mcav_mean", "MCAv mean",  "MCAv mean",        "cm/s",
  "cvci",      "MCAv CVCi",  "CVCi",             "cm/s/mmHg",
  "mcav_gpi",  "MCAv GPI",   "MCAv pulsatility", "ratio",
  "etco2",     "ET-CO2",     "ET-CO2",           "mmHg"
)

# ============================================================
# Row predicates
#
# Each returns one logical per row of df, for build_paired(subset_fun = ).
# A participant counts as phase-matched only if both visits fell in a named
# cycle phase; anything else (IUD, hysterectomy, COVID gap, irregular cycle)
# is unmatched. This is the definition the manuscript Methods describe.
# ============================================================

MATCHED_PHASES <- c("Luteal", "Follicular")
SEX_GROUPS <- c("Male", "Female matched", "Female unmatched")

is_sex <- function(df, sex) as.character(df[["1 Sex"]]) %in% sex

is_phase_matched <- function(df) {
  as.character(df[["Menstrual phase matched?"]]) %in% MATCHED_PHASES
}

group_subset <- function(group_name) {
  force(group_name)
  function(df) {
    switch(
      group_name,
      "Male"             = is_sex(df, "Male"),
      "Female matched"   = is_sex(df, "Female") & is_phase_matched(df),
      "Female unmatched" = is_sex(df, "Female") & !is_phase_matched(df),
      rep(FALSE, nrow(df))
    )
  }
}

exclude_unmatched_females <- function(df) {
  !(is_sex(df, "Female") & !is_phase_matched(df))
}

# ============================================================
# Reliability tables
# ============================================================

# Bland-Altman + ICC/CCC for each variable x epoch, over an optional subset.
build_reliability_stats_for_vars_r <- function(df, vars, subset_fun = NULL) {
  rows <- list()

  for (i in seq_len(nrow(vars))) {
    var <- vars[i, ]
    for (epoch in epochs) {
      dp <- build_paired(df, var$var_key, var$stem, epoch, subset_fun = subset_fun)
      vals <- c(
        ba_stats(dp),
        icc_ccc(dp),
        list(Variable = var$label, Epoch = unname(epoch_label[[epoch]]))
      )
      rows[[length(rows) + 1]] <- as.data.frame(vals, check.names = FALSE, stringsAsFactors = FALSE)
    }
  }

  numify(dplyr::bind_rows(rows))
}

# ICC/CCC for each variable x epoch within each sex / cycle-matching group.
build_sex_reliability_r <- function(df, vars = fig2_fig3_vars) {
  rows <- list()

  for (group in SEX_GROUPS) {
    subset_fun <- group_subset(group)
    for (i in seq_len(nrow(vars))) {
      var <- vars[i, ]
      for (epoch in epochs) {
        dp <- build_paired(df, var$var_key, var$stem, epoch, subset_fun = subset_fun)
        vals <- c(
          icc_ccc(dp),
          list(Group = group, Variable = var$label, Epoch = unname(epoch_label[[epoch]]), n = nrow(dp))
        )
        rows[[length(rows) + 1]] <- as.data.frame(vals, check.names = FALSE, stringsAsFactors = FALSE)
      }
    }
  }

  numify(dplyr::bind_rows(rows))
}

# ============================================================
# Leave-one-out influence
# ============================================================

LOO_GROUPS <- c("All", "Female unmatched", "Exclude unmatched females")

loo_subset_for <- function(group) {
  switch(
    group,
    "All"                       = NULL,
    "Exclude unmatched females" = exclude_unmatched_females,
    group_subset(group)
  )
}

# Recomputes ICC and CCC with each participant dropped in turn, so a single
# influential participant behind a reliability estimate becomes visible.
# Groups with fewer than 5 paired participants are skipped.
build_loo_influence_tables_r <- function(df, vars = fig2_fig3_vars) {
  summary_rows <- list()
  loo_rows <- list()

  for (group in LOO_GROUPS) {
    subset_fun <- loo_subset_for(group)

    for (i in seq_len(nrow(vars))) {
      var <- vars[i, ]
      for (epoch in epochs) {
        dp <- build_paired(df, var$var_key, var$stem, epoch, subset_fun = subset_fun)
        if (nrow(dp) < 5) {
          next
        }

        full <- icc_ccc(dp)
        epoch_name <- unname(epoch_label[[epoch]])
        icc_vals <- numeric(0)
        ccc_vals <- numeric(0)

        for (pid in sort(unique(dp$pid))) {
          without <- icc_ccc(dp[dp$pid != pid, , drop = FALSE])
          icc_vals <- c(icc_vals, without$ICC3k)
          ccc_vals <- c(ccc_vals, without$CCC)

          loo_rows[[length(loo_rows) + 1]] <- data.frame(
            Group                    = group,
            Variable                 = var$label,
            Epoch                    = epoch_name,
            pid_left_out             = pid,
            n_full                   = nrow(dp),
            ICC_full                 = full$ICC3k,
            CCC_full                 = full$CCC,
            ICC_leave_one_out        = without$ICC3k,
            CCC_leave_one_out        = without$CCC,
            delta_ICC_full_minus_LOO = full$ICC3k - without$ICC3k,
            delta_CCC_full_minus_LOO = full$CCC - without$CCC,
            abs_delta_ICC            = abs(full$ICC3k - without$ICC3k),
            abs_delta_CCC            = abs(full$CCC - without$CCC),
            stringsAsFactors = FALSE
          )
        }

        summary_rows[[length(summary_rows) + 1]] <- data.frame(
          Group             = group,
          Variable          = var$label,
          Epoch             = epoch_name,
          n                 = nrow(dp),
          ICC_full          = full$ICC3k,
          CCC_full          = full$CCC,
          ICC_LOO_min       = min(icc_vals, na.rm = TRUE),
          ICC_LOO_max       = max(icc_vals, na.rm = TRUE),
          CCC_LOO_min       = min(ccc_vals, na.rm = TRUE),
          CCC_LOO_max       = max(ccc_vals, na.rm = TRUE),
          max_abs_delta_ICC = max(abs(icc_vals - full$ICC3k), na.rm = TRUE),
          max_abs_delta_CCC = max(abs(ccc_vals - full$CCC), na.rm = TRUE),
          stringsAsFactors  = FALSE
        )
      }
    }
  }

  loo <- dplyr::bind_rows(loo_rows)

  # The single most influential participant per group x variable x epoch,
  # for `stat` ("ICC" or "CCC"), renamed to its reporting column names.
  most_influential <- function(stat) {
    cols <- c("pid_left_out",
              paste0("delta_", stat, "_full_minus_LOO"),
              paste0("abs_delta_", stat),
              paste0(stat, "_leave_one_out"))

    ranked <- loo %>%
      group_by(Group, Variable, Epoch) %>%
      arrange(desc(.data[[paste0("abs_delta_", stat)]]), .by_group = TRUE) %>%
      slice_head(n = 1) %>%
      ungroup()

    out <- as.data.frame(ranked[, c("Group", "Variable", "Epoch", cols)])
    names(out)[-(1:3)] <- c(
      paste0("Most_influential_ID_", stat),
      paste0(stat, "_change_full_minus_leave_one_out"),
      paste0("Max_abs_change_", stat),
      paste0(stat, "_after_removing_most_influential_ID")
    )
    out
  }

  out <- dplyr::bind_rows(summary_rows) %>%
    left_join(most_influential("ICC"), by = c("Group", "Variable", "Epoch")) %>%
    left_join(most_influential("CCC"), by = c("Group", "Variable", "Epoch")) %>%
    mutate(
      ICC_sign_changes_in_LOO = ICC_LOO_min < 0 & ICC_LOO_max > 0,
      CCC_sign_changes_in_LOO = CCC_LOO_min < 0 & CCC_LOO_max > 0,
      High_influence_flag     = Max_abs_change_ICC >= 0.30 | Max_abs_change_CCC >= 0.20
    )

  # Round the statistics but leave counts and participant IDs intact.
  round_cols <- setdiff(
    names(out)[vapply(out, is.numeric, logical(1))],
    c("n", "Most_influential_ID_ICC", "Most_influential_ID_CCC")
  )
  out[round_cols] <- lapply(out[round_cols], round, digits = 3)
  out <- out %>% arrange(Group, Variable, Epoch)

  list(out = out, flagged = out[out$High_influence_flag, , drop = FALSE])
}
