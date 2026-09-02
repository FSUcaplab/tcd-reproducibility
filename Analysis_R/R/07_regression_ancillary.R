# Table 4 and the ancillary analyses:
#   - mixed-model regression of the CPT cerebrovascular response
#   - between-visit reliability of pain, pressure-pain tolerance, water temperature
#   - sensitivity of the model to which hand was immersed

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(car)
  library(MuMIn)
  library(readxl)
})

# Participant 13's visit 1 carries the K32V1 ET-CO2 artefact.
VISIT_EXCL <- list(c(pid = 13L, visit = 1L))

LMM_CONTROL <- lme4::lmerControl(optimizer = "bobyqa")

# ============================================================
# Long (participant x visit) frame for the mixed models
# ============================================================

# Two rows per participant, ordered visit-within-participant. Rows with missing
# values are kept here and dropped by the callers via complete.cases(), so each
# model can define completeness over its own set of predictors.
build_lmm_data <- function(df) {
  n <- nrow(df)
  row_of <- rep(seq_len(n), each = 2)
  visit <- rep(c(1L, 2L), times = n)

  # Pick visit 1's or visit 2's column according to `visit`.
  by_visit <- function(suffix) {
    v1 <- to_num(df[[paste(1, suffix)]])
    v2 <- to_num(df[[paste(2, suffix)]])
    ifelse(visit == 1L, v1[row_of], v2[row_of])
  }

  pid <- as.integer(df[["1 Identifier"]])[row_of]
  excluded_visit <- Reduce(`|`, lapply(VISIT_EXCL, function(v) pid == v[["pid"]] & visit == v[["visit"]]))

  out <- data.frame(
    pid         = pid,
    visit       = visit,
    delta_cvci  = by_visit("Delta MCAv CVCi CPT 2min"),
    pain        = by_visit("rating of discomfort"),
    delta_etco2 = by_visit("Delta ET-CO2 CPT 2min"),
    stringsAsFactors = FALSE
  )

  out[!(pid %in% GLOBAL_EXCL_IDS) & !excluded_visit, , drop = FALSE]
}

# Fixed-effect coefficients with SEs, t and p on residual df.
fixef_table <- function(fit, n_obs) {
  cf <- lme4::fixef(fit)
  se <- sqrt(diag(as.matrix(lme4::vcov.merMod(fit))))
  t_val <- cf / se
  list(
    coef = cf,
    se   = se,
    t    = t_val,
    p    = 2 * stats::pt(abs(t_val), df = n_obs - length(cf) - 1L, lower.tail = FALSE)
  )
}

# ============================================================
# Table 4 - delta_CVCi ~ pain + delta_ET-CO2 + visit + (1 | participant)
# ============================================================

PREDICTOR_LABELS <- c(
  "(Intercept)" = "Intercept",
  pain          = "Perceived Pain (VAS)",
  delta_etco2   = "Delta ET-CO2 (mmHg)",
  visit         = "Visit Number"
)

build_regression_table <- function(df) {
  lmm <- build_lmm_data(df)
  cc <- lmm[complete.cases(lmm), ]

  fit <- lme4::lmer(delta_cvci ~ pain + delta_etco2 + visit + (1 | pid),
                    data = cc, REML = TRUE, control = LMM_CONTROL)

  r2 <- MuMIn::r.squaredGLMM(fit)
  var_re <- as.numeric(lme4::VarCorr(fit)$pid)
  var_res <- attr(lme4::VarCorr(fit), "sc")^2

  ft <- fixef_table(fit, nrow(cc))
  cf <- ft$coef

  # Standardised betas for the two continuous predictors only; visit is an
  # index, and an intercept has no standardised form.
  sd_y <- stats::sd(cc$delta_cvci)
  std_betas <- c(
    "(Intercept)" = NA_real_,
    pain          = as.numeric(cf["pain"]) * stats::sd(cc$pain) / sd_y,
    delta_etco2   = as.numeric(cf["delta_etco2"]) * stats::sd(cc$delta_etco2) / sd_y,
    visit         = NA_real_
  )

  # Model-level statistics belong to the model, not to any one coefficient,
  # so they are reported on the first row only.
  first_row_only <- function(value, n) c(value, rep(NA, n - 1))

  coef_df <- data.frame(
    Predictor      = PREDICTOR_LABELS[names(cf)],
    b              = as.numeric(cf),
    beta           = std_betas[names(cf)],
    SE             = as.numeric(ft$se),
    t              = as.numeric(ft$t),
    P              = as.numeric(ft$p),
    Marginal_R2    = first_row_only(as.numeric(r2[1, "R2m"]), length(cf)),
    Conditional_R2 = first_row_only(as.numeric(r2[1, "R2c"]), length(cf)),
    ICC_model      = first_row_only(var_re / (var_re + var_res), length(cf)),
    n_participants = first_row_only(length(unique(cc$pid)), length(cf)),
    n_observations = first_row_only(nrow(cc), length(cf)),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  # Collinearity is assessed on an auxiliary fixed-effects-only fit.
  vif_vals <- tryCatch(
    car::vif(stats::lm(delta_cvci ~ pain + delta_etco2 + visit, data = cc)),
    error = function(e) rep(NA_real_, 3)
  )

  list(
    coef_df = coef_df,
    vif_df  = data.frame(
      Predictor = unname(PREDICTOR_LABELS[c("pain", "delta_etco2", "visit")]),
      VIF       = as.numeric(vif_vals),
      stringsAsFactors = FALSE
    ),
    fit  = fit,
    data = cc
  )
}

# ============================================================
# Ancillary measures - between-visit reliability
# ============================================================

# Paired V1/V2 values for a measure held in one column per visit.
paired_measure <- function(df, v1_col, v2_col) {
  pid <- as.integer(df[["1 Identifier"]])
  v1 <- to_num(df[[v1_col]])
  v2 <- to_num(df[[v2_col]])
  keep <- !(pid %in% GLOBAL_EXCL_IDS) & !is.na(v1) & !is.na(v2)

  data.frame(pid = pid[keep], v1 = v1[keep], v2 = v2[keep])
}

ANCILLARY_MEASURES <- list(
  ppt        = list(v1 = "1 Baseline Algometer",   v2 = "2 Baseline Algometer",
                    label = "Pressure pain tolerance (algometer)", units = "kg/cm²"),
  water_temp = list(v1 = "1 water temperature",    v2 = "2 water temperature",
                    label = "Water temperature", units = "°C"),
  pain       = list(v1 = "1 rating of discomfort", v2 = "2 rating of discomfort",
                    label = "Perceived pain (VAS)", units = "0–100")
)

# Same statistics as Tables 2 and 3, for a single non-epoched measure.
ancillary_row <- function(dp, label, units) {
  ba <- ba_stats(dp)
  icc <- icc_ccc(dp)
  comp <- paired_comparison(dp)
  s <- paired_summaries(dp, comp)
  err <- paired_error(dp)

  data.frame(
    Variable           = label,
    Units              = units,
    n                  = nrow(dp),
    Visit_1            = s$v1$value,
    Visit_1_summary    = s$v1$kind,
    Visit_2            = s$v2$value,
    Visit_2_summary    = s$v2$kind,
    Fixed_Bias         = s$diff$value,
    Fixed_Bias_summary = s$diff$kind,
    Comparison_Test    = comp$Test %||% "",
    Comparison_P       = comp$P %||% NA_real_,
    Effect             = comp$Effect %||% NA_real_,
    Effect_type        = comp$Effect_type %||% "",
    MAE                = err$mae,
    `MAPE_%`           = err$mape,
    Mean_diff          = ba$Mean_diff,
    SD_diff            = ba$SD_diff,
    ICC3k              = icc$ICC3k,
    ICC3k_lo           = icc$ICC3k_lo,
    ICC3k_hi           = icc$ICC3k_hi,
    ICC3k_p            = icc$ICC3k_p,
    CCC                = icc$CCC,
    CCC_p              = icc$CCC_p,
    stringsAsFactors   = FALSE,
    check.names        = FALSE
  )
}

build_ppt_water_reliability <- function(df) {
  lapply(ANCILLARY_MEASURES, function(m) {
    ancillary_row(paired_measure(df, m$v1, m$v2), m$label, m$units)
  })
}

# ============================================================
# CPT hand sensitivity
# ============================================================

# Does the immersed hand (left vs right) change the Table 4 model?
# Compares the base model against one with a CPT-side term by likelihood ratio,
# both refitted with ML on the same complete cases. Needs the CPT side
# workbook, which is not distributed with this repository.
build_cpt_hand_sensitivity <- function(df, cpt_side_xlsx) {
  if (!file.exists(cpt_side_xlsx)) {
    warning("CPT side workbook not found: ", cpt_side_xlsx)
    return(NULL)
  }

  # Participant, visit and immersed side sit in unlabelled columns 8-10.
  raw <- readxl::read_excel(cpt_side_xlsx, sheet = "CPT side", col_names = FALSE)
  side_df <- raw[-1, c(8, 9, 10)]
  colnames(side_df) <- c("pid", "visit", "cpt_side")
  side_df <- side_df %>%
    dplyr::mutate(
      pid      = suppressWarnings(as.integer(pid)),
      visit    = suppressWarnings(as.integer(visit)),
      cpt_side = as.character(cpt_side)
    ) %>%
    dplyr::filter(!is.na(pid), !is.na(visit), cpt_side %in% c("Left", "Right"))

  lmm <- build_lmm_data(df) %>%
    dplyr::left_join(side_df, by = c("pid", "visit")) %>%
    dplyr::mutate(cpt_right = as.integer(cpt_side == "Right"))

  model_vars <- c("delta_cvci", "pain", "delta_etco2", "visit", "cpt_right")
  cc <- lmm[complete.cases(lmm[, model_vars]), ]

  fit_base <- lme4::lmer(delta_cvci ~ pain + delta_etco2 + visit + (1 | pid),
                         data = cc, REML = FALSE, control = LMM_CONTROL)
  fit_hand <- lme4::lmer(delta_cvci ~ pain + delta_etco2 + visit + cpt_right + (1 | pid),
                         data = cc, REML = FALSE, control = LMM_CONTROL)

  lrt <- anova(fit_base, fit_hand)
  ft <- fixef_table(fit_hand, nrow(cc))
  r2_base <- MuMIn::r.squaredGLMM(fit_base)
  r2_hand <- MuMIn::r.squaredGLMM(fit_hand)

  data.frame(
    n_obs               = nrow(cc),
    n_participants      = length(unique(cc$pid)),
    cpt_right_b         = as.numeric(ft$coef["cpt_right"]),
    cpt_right_SE        = as.numeric(ft$se["cpt_right"]),
    cpt_right_t         = as.numeric(ft$t["cpt_right"]),
    cpt_right_P         = as.numeric(ft$p["cpt_right"]),
    LRT_Chisq           = as.numeric(lrt[["Chisq"]][2]),
    LRT_df              = as.integer(lrt[["Df"]][2]),
    LRT_P               = as.numeric(lrt[["Pr(>Chisq)"]][2]),
    Base_Marginal_R2    = as.numeric(r2_base[1, "R2m"]),
    Base_Conditional_R2 = as.numeric(r2_base[1, "R2c"]),
    Hand_Marginal_R2    = as.numeric(r2_hand[1, "R2m"]),
    Hand_Conditional_R2 = as.numeric(r2_hand[1, "R2c"]),
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
}
