# Revision Statistics Summary

_All analyses performed in R v4.6.0. Statistical code and outputs archived in the project repository._

---

## Item 1 / 2 / 4 / 5 — Fig1 post-hoc testing, effect sizes, ANOVA stats in panels

**Their comment:**
> "figure 1: adding post hoc testing. When completed, would be helpful to have some marking on the figure (showing asterics for significance)"
> "Want to report significant and effect size for all points"
> "There was a significant effect of time for all variables (Ps<0.03; F=3.5–104.9; ηp2=0.07–0.68). Notably, ηp2 for Δmean MCAv was only medium (ηp2=0.07) while all others were large (ηp2=0.26–0.68). There were no significant main effects of visit (Ps>0.14; F=0.18–2.52; ηp2=0.03–0.05) nor were there any significant time-by-visit interactions (Ps>0.06; F=0.06–0.65; ηp2=0.01–0.06)."

**Our stats — two-way repeated-measures ANOVA (time × visit):**

| Variable | Time F | Time df | Time p | Time ηp² | Visit p | Interaction p |
|---|---|---|---|---|---|---|
| Systolic BP | 59.6 | 2, 96 | <0.001 | 0.554 | 0.353 | 0.246 |
| Diastolic BP | 104.9 | 2, 96 | <0.001 | 0.686 | 0.119 | 0.084 |
| Mean BP | 104.1 | 2, 96 | <0.001 | 0.684 | 0.198 | 0.123 |
| HR | 49.2 | 2, 96 | <0.001 | 0.506 | 0.137 | 0.247 |
| MCAv mean | 3.52 | 2, 90 | 0.034 | 0.073 | 0.677 | 0.649 |
| CVCi | 46.0 | 2, 90 | <0.001 | 0.505 | 0.305 | 0.060 |
| MCAv pulsatility | 15.9 | 2, 90 | <0.001 | 0.261 | 0.312 | 0.523 |
| ET-CO2 | 41.2 | 2, 84 | <0.001 | 0.495 | 0.283 | 0.242 |

**Post-hoc pairwise (Bonferroni-corrected, m = 3 comparisons per variable):**

| Variable | Base vs Min1 p | Base vs Min1 ES | Base vs Min2 p | Base vs Min2 ES | Min1 vs Min2 p | Min1 vs Min2 ES |
|---|---|---|---|---|---|---|
| Systolic BP | <0.001 | d = 0.67 | <0.001 | d = 1.25 | <0.001 | rbc = 0.96 |
| Diastolic BP | <0.001 | d = 0.99 | <0.001 | d = 1.75 | <0.001 | rbc = 0.98 |
| Mean BP | <0.001 | d = 1.04 | <0.001 | d = 1.69 | <0.001 | rbc = 0.97 |
| HR | <0.001 | rbc = 0.96 | <0.001 | rbc = 0.94 | <0.001 | rbc = 0.63 |
| MCAv mean | 0.174 | d = 0.29 | 1.000 | d = 0.11 | 0.030 | rbc = 0.44 |
| CVCi | 0.010 | d = 0.46 | <0.001 | d = 1.18 | <0.001 | rbc = 0.97 |
| MCAv pulsatility | <0.001 | d = 0.92 | <0.001 | rbc = 0.67 | 1.000 | rbc = 0.08 |
| ET-CO2 | <0.001 | d = 1.04 | <0.001 | rbc = 0.92 | <0.001 | rbc = 0.74 |

Asterisks (* p < .05, ** p < .01, *** p < .001) and ANOVA panel annotations added to Figure 1.

---

## Item 7 — IQR consistency

**Their comment:**
> "Table 2: if one is IQR, then both should be. This is global rule."

**Action:** Enforced globally across all reliability tables — if any of Visit 1 / Visit 2 / Difference is non-normal, all three columns for that variable report median [IQR].

---

## Item 8 — CCC p-values

**Their comment:**
> "Add p values for CCC"

**Action:** CCC p-values added throughout (Fisher Z-transform: z = atanh(CCC) × √(n − 3), two-tailed).

---

## Item 9 — VIF

**Their comment:**
> "Run variance inflation factor for all variables and have outputs for manuscript"

**Our stats — VIF for mixed model predictors:**

| Predictor | VIF |
|---|---|
| Perceived Pain (VAS) | 1.03 |
| Delta ET-CO2 (mmHg) | 1.04 |
| Visit Number | 1.01 |

All VIF < 1.5; no multicollinearity concern.

---

## Item 10 / 12 — Marginal R², Conditional R², model ICC

**Their comment:**
> "The model fixed effects (perceived pain, end-tidal CO2, and visit number) explained 35.9% of the variance in ΔCVCi (Marginal R2 = 0.359). The full model—including the random effect of participant ID—explained 56.0% of the variance (Conditional R2 = 0.560). The intraclass correlation for the model was 0.314."

**Our stats — mixed model: ΔCVCi ~ pain + ΔET-CO2 + visit + (1 | participant):**

| Predictor | b | β (std) | SE | t | p |
|---|---|---|---|---|---|
| Intercept | −0.0020 | — | 0.0313 | −0.065 | 0.949 |
| Perceived Pain (VAS) | −0.00144 | −0.296 | 0.000445 | −3.23 | 0.0017 |
| Delta ET-CO2 (mmHg) | 0.01105 | 0.467 | 0.00197 | 5.60 | <0.001 |
| Visit Number | 0.01304 | — | 0.01203 | 1.08 | 0.281 |

- **Marginal R² = 0.367** (fixed effects only)
- **Conditional R² = 0.645** (full model including random intercept)
- **Model ICC = 0.439** (variance attributable to between-participant differences)
- n = 49 participants, 93 observations

_Note: Values differ slightly from the placeholder text above — these are the updated statistics._

---

## Item 14 — R vs L hand effect on model

**Their comment:**
> "Need to add stats for testing if R vs. L hand changed the model at all. Have outputs for manuscript"

**Our stats — CPT hand sensitivity analysis (n = 37 participants with CPT side data; 74 observations):**

| | b | SE | t | p |
|---|---|---|---|---|
| CPT hand (right = 1) | 0.004 | 0.013 | 0.32 | 0.747 |

- **Likelihood ratio test vs. base model:** χ²(1) = 0.105, p = 0.746
- Marginal R²: 0.380 (base) → 0.382 (+ hand); no meaningful change
- Conditional R²: 0.571 (base) → 0.570 (+ hand)

CPT hand did not improve model fit. The remaining 13 participants had no CPT side recorded.

---

## Item 16 — Bonferroni correction

**Their comment:**
> "Add Bonferroni correction in the ANOVA."

**Action:** Bonferroni correction applied to all pairwise post-hoc comparisons (m = 3: Base vs Min1, Base vs Min2, Min1 vs Min2). Corrected p-values reported throughout.

---

## Joe Note 1 — PPT ICC with p-value

**Their comment:**
> "Pressure pain tolerance was not different (P = 0.15, d = 0.22) between visit 1 (1.24 ± 0.4) and visit 2 (1.09 [0.5]). The mean absolute difference between visits was 0.25, and the mean absolute percent error was not good (i.e., poor, 20.0%). Pressure pain tolerance had moderate relative agreement between the two visits (CCC = 0.679). — We will need ICCs (with p-values) here"

**Our stats (n = 41):**

| | Visit 1 | Visit 2 | Difference |
|---|---|---|---|
| PPT (kg/cm²) | 1.15 [0.547] | 1.09 [0.54] | 0.05 [0.363] |

- Comparison: paired t, p = 0.236, d = 0.188
- MAE = 0.245 kg/cm²; MAPE = 20.2%
- **ICC(3,k) = 0.819 [95% CI: 0.66, 0.90], p < 0.001**
- **CCC = 0.686, p < 0.001**

---

## Joe Note 2 — Fig3 panel J outlier

**Their comment:**
> "Figure 3 panel J includes an outlier shown as the person who had a between-visit difference of over 20. Maybe we forgot to remove this person from baseline?"

**Action:** Identified as participant K05 Visit 1 — baseline ET-CO2 = 18.67 mmHg (artifactual; normal range ~40–50 mmHg). Added to ET-CO2 exclusion list alongside existing exclusions. Figure 3 panel J now shows n = 44 (previously n = 46 at baseline).

---

## Joe Note 3 — Water temperature and perceived pain ICC with p-value

**Their comment:**
> "Water temperature was not different between visits (visit 1: 0.36 [0.05] vs. visit 2: 0.34 [0.08] °C; P = 0.18, rbc = 0.23). Perceived pain during the CPT was significantly greater (P = 0.03, rbc = 0.36) during visit 2 (65 [24]) compared with visit 1 (60 [32])... We will need relative agreement based on ICC (with p-value) here."

**Our stats — Water temperature (n = 49):**

| | Visit 1 | Visit 2 | Difference |
|---|---|---|---|
| Water temp (°C) | 0.35 [0.05] | 0.34 [0.08] | 0.01 [0.08] |

- Comparison: Wilcoxon, p = 0.109, rbc = 0.277
- MAE = 0.052°C; MAPE = 19.3%
- **ICC(3,k) = 0.574 [95% CI: 0.25, 0.76], p = 0.002**
- **CCC = 0.380, p = 0.007**

**Our stats — Perceived pain / VAS (n = 49):**

| | Visit 1 | Visit 2 | Difference |
|---|---|---|---|
| VAS (0–100) | 59 [32] | 65 [23] | −4 [13] |

- Comparison: paired t, p = 0.041, d = −0.300 (V2 > V1)
- MAE = 9.2; MAPE = 19.2%
- **ICC(3,k) = 0.915 [95% CI: 0.85, 0.95], p < 0.001**
- **CCC = 0.831, p < 0.001**

---

## New Supplemental Figures

Per manuscript text: *"Bland-Altman plots for peak MCAv, minimum MCAv, CVRi, and SmO2 baseline values and Δ values during minutes 1 and 2 of the CPT are shown in supplemental figure X."*

- **Supplemental Figure 1** — ICC and CCC for all 15 variables
- **Supplemental Figure 2** — Bland-Altman plots: MCAv peak, MCAv min, CVRi, SmO2 (baseline, Min 1, Min 2)
