# Unit tests for the statistical kernels and the selection logic around them.
# These need no data: every case is constructed so its answer is known by hand
# or independently derivable from base R.

section("Agreement coefficients")

# v2 = v1 + 1: perfect consistency but a constant bias.
# ICC(3,k) removes the rater (visit) effect, so it stays at 1; Lin's CCC
# penalises the shift. By hand: CCC = 2*2 / (2 + 2 + 1^2) = 0.8.
shifted <- data.frame(pid = 1:5, v1 = 1:5, v2 = 2:6)
shifted_res <- icc_ccc(shifted)
check_near("ICC(3,k) ignores a constant between-visit shift", shifted_res$ICC3k, 1)
check_near("CCC penalises a constant between-visit shift", shifted_res$CCC, 0.8)

identical_visits <- data.frame(pid = 1:6, v1 = c(2, 4, 6, 8, 10, 12), v2 = c(2, 4, 6, 8, 10, 12))
check_near("CCC is 1 for identical visits", icc_ccc(identical_visits)$CCC, 1)
check_near("lin_ccc is symmetric",
           lin_ccc(c(1, 2, 3, 5), c(2, 2, 4, 4)), lin_ccc(c(2, 2, 4, 4), c(1, 2, 3, 5)))

check("icc_ccc returns all fields when n is too small",
      identical(names(icc_ccc(data.frame(v1 = 1:3, v2 = 1:3))),
                c("ICC3k", "ICC3k_lo", "ICC3k_hi", "ICC3k_p", "CCC", "CCC_p")))

section("Regression and correlation kernels")

set.seed(11)
x <- c(2.1, 3.4, 4.8, 5.2, 7.9, 8.1, 9.6, 11.2)
y <- c(1.9, 3.9, 4.1, 6.0, 7.2, 8.8, 9.1, 12.0)

fit <- stats::lm(y ~ x)
lr <- linregress(x, y)
check_near("linregress slope matches lm()", lr$slope, unname(coef(fit)[2]))
check_near("linregress intercept matches lm()", lr$intercept, unname(coef(fit)[1]))
check_near("linregress p matches cor.test()", lr$p, stats::cor.test(x, y)$p.value)

sp <- spearman_scipy(x, y)
check_near("spearman rho matches cor(method='spearman')", sp$rho, stats::cor(x, y, method = "spearman"))

check("linregress returns NA below three points", is.na(linregress(c(1, 2), c(1, 2))$slope))
check("spearman returns NA on a constant vector", is.na(spearman_scipy(x, rep(1, length(x)))$rho))

section("Bland-Altman limits of agreement")

# Symmetric, homoscedastic differences with no trend against the mean:
# neither proportional bias nor a fan, so the standard limits apply.
plain <- data.frame(v1 = c(10, 12, 14, 16, 18, 20, 22, 24),
                    v2 = c(11, 11, 15, 15, 19, 19, 23, 23))
plain_ba <- ba_stats(plain)
check_equal("constant spread gives standard limits", plain_ba$LoA_type, "standard")
check_near("standard limits are mean diff +/- 1.96 SD",
           plain_ba$LoA_hi, plain_ba$Mean_diff + 1.96 * plain_ba$SD_diff)
check_near("limits are symmetric about the mean difference",
           (plain_ba$LoA_lo + plain_ba$LoA_hi) / 2, plain_ba$Mean_diff)

# Difference grows steadily with the mean -> proportional bias detected.
sloped <- data.frame(v1 = seq(10, 40, by = 2),
                     v2 = seq(10, 40, by = 2) - seq(0, 15, by = 1))
check("a mean-dependent difference is flagged as proportional bias",
      grepl("regression", ba_stats(sloped)$LoA_type))

check("ba_stats returns all fields when n is too small",
      identical(names(ba_stats(data.frame(v1 = 1:3, v2 = 1:3))),
                c("n", "Mean_diff", "SD_diff", "LoA_type", "LoA_lo", "LoA_hi",
                  "Prop_bias_r", "Prop_bias_P", "Hetero_rho", "Hetero_P")))

section("Effect sizes and summaries")

check_near("rank biserial is 1 when every difference is positive",
           paired_rank_biserial(c(1, 2, 3, 4)), 1)
check_near("rank biserial is -1 when every difference is negative",
           paired_rank_biserial(c(-1, -2, -3, -4)), -1)
check("rank biserial ignores zero differences", is.na(paired_rank_biserial(c(0, 0, 0))))

check_equal("a normal sample is summarised as mean +/- SD",
            summary_string(c(9.8, 10.1, 9.9, 10.3, 10.0, 9.7, 10.2, 10.0))$kind, "mean_sd")
check_equal("a skewed sample is summarised as median [IQR]",
            summary_string(c(1, 1, 1, 1, 1, 1, 2, 3, 40, 100))$kind, "median_iqr")
check_equal("median_iqr_string formats as median [IQR]",
            median_iqr_string(c(1, 2, 3, 4, 5))$value, "3 [2]")

section("Axis break helpers")

nb <- nice_breaks(0, 97)
check("nice_breaks limits contain the data", nb$limits[1] <= 0 && nb$limits[2] >= 97)
check("nice_breaks caps the tick count at six", length(nb$breaks) <= 6)
check("nice_breaks handles a degenerate range", length(nice_breaks(5, 5)$breaks) >= 2)
check("nice_breaks handles non-finite input", identical(nice_breaks(NA, NA)$limits, c(0, 1)))

yb <- reliability_y_breaks(-0.10)
check("reliability breaks start at the floor", isTRUE(all.equal(min(yb), -0.10)))
check("reliability breaks end at 1.0", isTRUE(all.equal(max(yb), 1.0)))
check("reliability breaks stay legible for a deep floor", length(reliability_y_breaks(-1.40)) <= 7)

section("Exclusions and grouping")

mini <- data.frame(
  check.names = FALSE,
  "1 Identifier" = c(1L, 2L, 45L, 4L),
  "1 Subject ID" = c("F01V1", "K10V1", "M03V1", "M04V1"),
  "1 Sex" = c("Female", "Female", "Male", "Male"),
  "Menstrual phase matched?" = c("Luteal", "IUD", NA, NA),
  "1 SBP Base" = c(110, 120, 130, 140),
  "2 SBP Base" = c(112, 118, 128, 145),
  "1 MCAv mean Base" = c(60, 62, 64, 66),
  "2 MCAv mean Base" = c(61, 63, 65, 67),
  stringsAsFactors = FALSE
)

check_equal("participant 45 is globally excluded",
            included_rows(mini, "sbp"), c(TRUE, TRUE, FALSE, TRUE))
check_equal("K10V1 is excluded from MCAv but not from SBP",
            included_rows(mini, "mcav_mean"), c(TRUE, FALSE, FALSE, TRUE))

paired_sbp <- build_paired(mini, "sbp", "SBP", "Base")
check_equal("build_paired drops excluded participants", paired_sbp$pid, c(1L, 2L, 4L))
check_equal("build_paired keeps visit 1 values", paired_sbp$v1, c(110, 120, 140))

check_equal("phase matching follows the named cycle phases",
            is_phase_matched(mini), c(TRUE, FALSE, FALSE, FALSE))
check_equal("the male group selects males",
            group_subset("Male")(mini), c(FALSE, FALSE, TRUE, TRUE))
check_equal("the matched-female group needs a named phase",
            group_subset("Female matched")(mini), c(TRUE, FALSE, FALSE, FALSE))
check_equal("the unmatched-female group takes the rest",
            group_subset("Female unmatched")(mini), c(FALSE, TRUE, FALSE, FALSE))
check_equal("excluding unmatched females keeps everyone else",
            exclude_unmatched_females(mini), c(TRUE, FALSE, TRUE, TRUE))

# A subset predicate composes with the per-variable exclusions.
check_equal("subset predicates compose with exclusions",
            build_paired(mini, "sbp", "SBP", "Base", subset_fun = group_subset("Male"))$pid, 4L)

section("Deltas from baseline")

check("a delta baseline is zero by construction",
      all(epoch_values(mini, 1, "SBP", "Base", delta_baseline = TRUE) == 0))
check_equal("an absolute baseline reads the visit column",
            epoch_values(mini, 2, "SBP", "Base"), c(112, 118, 128, 145))

section("CSV serialisation")

tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp), add = TRUE)
write_csv_minimal(
  data.frame(
    plain = c("a", "b"),
    comma = c("x,y", "z"),
    quoted = c('say "hi"', "plain"),
    flag = c(TRUE, FALSE),
    num = c(1.5, NA),
    stringsAsFactors = FALSE
  ),
  tmp
)
lines <- readLines(tmp)

check_equal("header is written unquoted when it can be",
            lines[1], "plain,comma,quoted,flag,num")
check_equal("fields containing a comma are quoted",
            lines[2], 'a,"x,y","say ""hi""",True,1.5')
check_equal("logicals write as True/False and NA writes empty",
            lines[3], "b,z,plain,False,")
