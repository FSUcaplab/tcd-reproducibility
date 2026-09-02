# Step 07 - Mixed-model regression of delta-CVCi (Table 4), reliability of the
# ancillary CPT measures (pain, pressure-pain tolerance, water temperature),
# and the CPT-hand sensitivity check.

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step(c("03_reliability_tables", "07_regression_ancillary"))

# ---- Mixed-model regression (Table 4) ----
cat("\n=== Mixed-model regression: delta_CVCi ~ pain + delta_ET-CO2 + visit ===\n")
reg <- build_regression_table(step$df)
write_outputs(
  list(
    Table4_Regression_LMM = reg$coef_df,
    Table4_Regression_VIF = reg$vif_df
  ),
  step$config
)

cat("\nCoefficients:\n")
print(reg$coef_df[, c("Predictor", "b", "beta", "SE", "t", "P",
                      "Marginal_R2", "Conditional_R2", "ICC_model")],
      row.names = FALSE, digits = 4)
cat("\nVIF:\n")
print(reg$vif_df, row.names = FALSE, digits = 4)

# ---- Ancillary measures: V1 vs V2 reliability ----
cat("\n=== PPT, water temperature and perceived pain: V1 vs V2 reliability ===\n")
rel <- build_ppt_water_reliability(step$df)
write_outputs(
  list(
    Table_PPT_WaterTemp_Reliability      = dplyr::bind_rows(rel$ppt, rel$water_temp),
    Table_PPT_WaterTemp_Pain_Reliability = dplyr::bind_rows(rel$ppt, rel$water_temp, rel$pain)
  ),
  step$config
)

show_cols <- c("Variable", "n", "Visit_1", "Visit_2", "Fixed_Bias",
               "Comparison_P", "Effect", "Effect_type", "MAE", "MAPE_%",
               "ICC3k", "ICC3k_lo", "ICC3k_hi", "ICC3k_p", "CCC", "CCC_p")
for (name in names(rel)) {
  cat("\n", name, ":\n", sep = "")
  print(rel[[name]][, show_cols], row.names = FALSE, digits = 4)
}

# ---- CPT hand sensitivity ----
# Needs the CPT side workbook, which is not distributed with the repository.
cat("\n=== CPT hand sensitivity: does CPT side (L vs R) change the model? ===\n")
hand <- build_cpt_hand_sensitivity(step$df, step$config$cpt_side_xlsx)
if (is.null(hand)) {
  cat("SKIP: CPT side workbook not available.\n")
} else {
  write_outputs(list(Table4_CPThand_Sensitivity = hand), step$config)
  cat(sprintf("n=%d obs / %d participants with CPT side data\n", hand$n_obs, hand$n_participants))
  cat(sprintf("CPT hand (right=1): b=%.5f, SE=%.5f, t=%.3f, p=%.4f\n",
              hand$cpt_right_b, hand$cpt_right_SE, hand$cpt_right_t, hand$cpt_right_P))
  cat(sprintf("LRT vs. base model: chi2(%d)=%.3f, p=%.4f\n",
              hand$LRT_df, hand$LRT_Chisq, hand$LRT_P))
  cat(sprintf("Base model:  Marginal R2=%.4f, Conditional R2=%.4f\n",
              hand$Base_Marginal_R2, hand$Base_Conditional_R2))
  cat(sprintf("+hand model: Marginal R2=%.4f, Conditional R2=%.4f\n",
              hand$Hand_Marginal_R2, hand$Hand_Conditional_R2))
}
