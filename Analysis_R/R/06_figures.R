suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(ggtext)
})

# ============================================================
# FSU brand palette (rules_fsu_branding.md)
# ============================================================
FSU_GARNET     <- "#782F40"
FSU_GOLD       <- "#CEB888"
LEGACY_BLUE    <- "#425563"
VAULT_GARNET   <- "#A6192E"
WESTCOTT_WATER <- "#5CB8B2"
VAULT_GOLD     <- "#FFC72C"
PLAZA_BRICK    <- "#572932"
STADIUM_NIGHT  <- "#101820"

COL_V1   <- FSU_GARNET   # Visit 1
COL_V2   <- LEGACY_BLUE  # Visit 2
COL_TEXT <- STADIUM_NIGHT
COL_CI   <- "#D9D9D9"
COL_DIAG <- "grey60"     # zero / reference lines

# mm-to-pt conversion used by ggplot2 for geom_text()/annotate() `size` (text size is in mm)
PT <- 72.27 / 25.4

# ============================================================
# Figure-level variable ordering & display labels
# Cosmetic only: reorders/relabels BP variables as SBP -> MBP -> DBP with
# full names for display. Does NOT change all15 / fig2_fig3_vars order used
# by steps 01-05 (validated CSV outputs are unaffected).
# ============================================================
fig_label_map <- c(
  SBP = "Systolic BP",
  MBP = "Mean BP",
  DBP = "Diastolic BP",
  HR  = "Heart rate",
  "MCAv mean" = "mean MCAv",
  "MCAv peak" = "Peak MCAv",
  "MCAv min"  = "Min MCAv",
  "ET-CO2"    = "ET-CO<sub>2</sub>"
)

fig_display_label <- function(x) {
  out <- unname(fig_label_map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

bp_first_order <- function(labels) {
  bp <- c("SBP", "MBP", "DBP", "HR")
  c(intersect(bp, labels), setdiff(labels, bp))
}

# ============================================================
# Categorical styles for reliability figures (Fig4/5/6/SupFig)
# Reordered to SBP, MBP, DBP, HR, ... (color/shape/linetype per variable
# preserved from the original palette so variable identity stays consistent).
# ============================================================
rel_styles <- data.frame(
  Variable = c("SBP", "MBP", "DBP", "HR", "MCAv mean", "CVCi", "MCAv pulsatility", "ET-CO2"),
  color = c("#7BAFD4", LEGACY_BLUE, VAULT_GARNET, PLAZA_BRICK, FSU_GARNET, FSU_GOLD, WESTCOTT_WATER, VAULT_GOLD),
  shape = c(15, 15, 15, 18, 16, 16, 16, 18),
  linetype = c(2, 1, 3, 1, 1, 1, 2, 2),
  stringsAsFactors = FALSE
)

extra_rel_styles <- data.frame(
  Variable = c("Q (cardiac)", "TPR", "MCAv peak", "MCAv min", "MCAv pulse", "CVRi", "SmO2"),
  color = c("#00843D", "#6A1B9A", "#D55E00", "#0072B2", "#009E73", "#CC79A7", "#999999"),
  shape = c(17, 17, 8, 8, 8, 4, 4),
  linetype = c(1, 2, 1, 2, 3, 1, 2),
  stringsAsFactors = FALSE
)

all_rel_styles <- rbind(rel_styles, extra_rel_styles)

# ============================================================
# Sizing constants (rules_figures.md: panel vs single figure text sizing)
# ============================================================

# Fig4 / Fig6 - 2 stacked panels, single-column figure
FIG4_AXIS_TEXT_SIZE  <- 13
FIG4_AXIS_TITLE_SIZE <- 14
FIG4_TITLE_SIZE      <- 14
FIG4_SUPTITLE_SIZE   <- 16
FIG4_ANNOT_SIZE      <- 10 / PT

# SupFig - 2 stacked panels, wide single-column figure, 15 variables
FIGS_AXIS_TEXT_SIZE  <- 12
FIGS_AXIS_TITLE_SIZE <- 13
FIGS_TITLE_SIZE      <- 13
FIGS_SUPTITLE_SIZE   <- 15
FIGS_ANNOT_SIZE      <- 9 / PT
FIGS_CAPTION_SIZE    <- 11

# Fig5 - 2 x 3 panel grid
FIG5_AXIS_TEXT_SIZE  <- 11 * 1.25
FIG5_AXIS_TITLE_SIZE <- 12 * 1.25
FIG5_TITLE_SIZE      <- 12 * 1.25
FIG5_SUPTITLE_SIZE   <- 15 * 1.25
FIG5_ANNOT_SIZE      <- (8.5 * 1.25) / PT
FIG5_TAG_SIZE        <- 12 * 1.25
FIG5_CAPTION_SIZE    <- 10.5 * 1.25

# Fig1 - 4 x 2 panel grid (CPT time-course)
FIG1_AXIS_TEXT_SIZE  <- 11
FIG1_AXIS_TITLE_SIZE <- 12
FIG1_TITLE_SIZE      <- 13
FIG1_SUPTITLE_SIZE   <- 16
# In-panel stat annotations (n=, significance stars, ANOVA text) sized to
# match the axis text so they read consistently with the tick labels.
FIG1_ANNOT_SIZE      <- FIG1_AXIS_TEXT_SIZE / PT

# Fig2 / Fig3 / SupFig2 - 4 x 3 panel grid (Bland-Altman). Axis text/suptitle
# at 125% of the original sizing; panel letters, axis titles, and per-panel
# titles ("Baseline"/"Minute 1"/"Minute 2") at a further +25% on top of that.
FIG23_AXIS_TEXT_SIZE  <- 8 * 1.25
FIG23_AXIS_TITLE_SIZE <- 9 * 1.25 * 1.25
FIG23_TITLE_SIZE      <- 9 * 1.25 * 1.25
FIG23_SUPTITLE_SIZE   <- 15 * 1.25
# n= annotation sized to match the axis text.
FIG23_ANNOT_SIZE      <- FIG23_AXIS_TEXT_SIZE / PT

# ============================================================
# Shared helpers
# ============================================================

# "Nice" axis breaks whose first/last values become scale limits, capped at
# 4-6 ticks by trying increasingly coarse human-friendly step sizes
# (1, 2, 2.5, 5, 10 x 10^k) until the resulting break count is <= 6.
nice_breaks <- function(lo, hi, n = 5) {
  if (!is.finite(lo) || !is.finite(hi)) {
    return(list(limits = c(0, 1), breaks = c(0, 1)))
  }
  if (lo == hi) {
    lo <- lo - 1
    hi <- hi + 1
  }
  span <- hi - lo
  raw_step <- span / n
  mag <- 10 ^ floor(log10(raw_step))
  candidates <- c(1, 2, 2.5, 5, 10, 20, 25, 50, 100) * mag
  for (step in candidates) {
    lo_b <- step * floor(lo / step)
    hi_b <- step * ceiling(hi / step)
    n_breaks <- round((hi_b - lo_b) / step) + 1
    if (n_breaks <= 6) break
  }
  breaks <- seq(lo_b, hi_b, by = step)
  list(limits = c(lo_b, hi_b), breaks = breaks)
}

# Axis breaks for ICC/CCC panels: y_min is always the first break (lower limit),
# 1.0 is always the last break (upper limit), with a step chosen so the
# resulting break count stays in the 4-6 range regardless of how low y_min is.
reliability_y_breaks <- function(y_min, hi = 1.0) {
  span <- hi - y_min
  step <- if (span <= 1.3) 0.25 else 0.5
  lo_break <- step * ceiling(y_min / step)
  breaks <- seq(lo_break, hi, by = step)
  breaks <- sort(unique(c(y_min, breaks)))
  if (length(breaks) > 1 && (breaks[2] - breaks[1]) < step / 2) {
    breaks <- breaks[-2]
  }
  breaks
}

# "n=NN" if n is constant across epochs for this variable, else "n=lo-hi"
n_label <- function(stats_df, variable) {
  ns <- stats_df$n[stats_df$Variable == variable]
  ns <- ns[is.finite(ns)]
  if (length(ns) == 0) return("")
  if (length(unique(ns)) == 1) sprintf("n=%d", ns[1]) else sprintf("n=%d-%d", min(ns), max(ns))
}

save_fig <- function(plot, name, width, height, config, dpi = 500) {
  ensure_dir(config$output_dir)
  png_path <- file.path(config$output_dir, paste0(name, ".png"))
  pdf_path <- file.path(config$output_dir, paste0(name, ".pdf"))
  ggsave(png_path, plot, width = width, height = height, units = "in", dpi = dpi, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, units = "in", dpi = dpi, bg = "white", device = grDevices::cairo_pdf)
  invisible(c(png = png_path, pdf = pdf_path))
}

figure_dims <- list(
  Fig1_CPT_TimeCourse = c(width = 8.6, height = 12.3),
  # Fig2/Fig3/SupFig2 rows taller than before so the larger rotated row
  # labels have room to center without overlapping the row above.
  Fig2_BA_Cardiovascular = c(width = 8.4, height = 13.0),
  Fig3_BA_Cerebrovascular = c(width = 8.4, height = 13.0),
  Fig4_ICC_CCC_Summary = c(width = 7.7, height = 10.4),
  Fig5_ICC_CCC_Sex_Menstrual = c(width = 12.8, height = 9.0),
  Fig6_ICC_CCC_Exclude_Unmatched_Females = c(width = 7.7, height = 10.4),
  SupFig1_ICC_CCC_AllVars        = c(width = 10.5, height = 9.5),
  SupFig2_BA_MCAv_CVRi_SmO2     = c(width = 8.4,  height = 13.0)
)

# ============================================================
# Fig4 / Fig6 - ICC / CCC reliability summary (2 stacked panels)
# ============================================================

fig4_theme <- theme_classic(base_size = FIG4_AXIS_TEXT_SIZE) +
  theme(
    text = element_text(color = COL_TEXT),
    axis.title.x = element_markdown(size = FIG4_AXIS_TITLE_SIZE, color = COL_TEXT),
    axis.title.y = element_markdown(size = FIG4_AXIS_TITLE_SIZE, color = COL_TEXT, angle = 90),
    axis.text = element_text(size = FIG4_AXIS_TEXT_SIZE, color = COL_TEXT),
    axis.line = element_line(linewidth = 0.6, color = COL_TEXT),
    axis.ticks = element_line(linewidth = 0.5, color = COL_TEXT),
    plot.title = element_text(size = FIG4_TITLE_SIZE, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.text = element_markdown(size = FIG4_AXIS_TEXT_SIZE),
    legend.title = element_blank(),
    legend.key.width = unit(1.6, "lines"),
    plot.margin = margin(t = 6, r = 14, b = 4, l = 6)
  )

figs_theme <- theme_classic(base_size = FIGS_AXIS_TEXT_SIZE) +
  theme(
    text = element_text(color = COL_TEXT),
    axis.title.x = element_markdown(size = FIGS_AXIS_TITLE_SIZE, color = COL_TEXT),
    axis.title.y = element_markdown(size = FIGS_AXIS_TITLE_SIZE, color = COL_TEXT, angle = 90),
    axis.text = element_text(size = FIGS_AXIS_TEXT_SIZE, color = COL_TEXT),
    axis.line = element_line(linewidth = 0.6, color = COL_TEXT),
    axis.ticks = element_line(linewidth = 0.5, color = COL_TEXT),
    plot.title = element_text(size = FIGS_TITLE_SIZE, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.text = element_markdown(size = FIGS_AXIS_TEXT_SIZE),
    legend.title = element_blank(),
    legend.key.width = unit(1.4, "lines"),
    plot.margin = margin(t = 6, r = 18, b = 4, l = 6)
  )

# Reliability interpretation bands (Poor/Moderate/.../Excellent for ICC,
# Poor/Fair/.../Very Strong for CCC) as background rects + right-edge labels,
# plus dotted reference lines at each cut point.
reliability_bands <- function(stat, y_min, x_min, x_max, label_x, annot_size = FIG4_ANNOT_SIZE) {
  if (stat == "ICC3k") {
    bands <- data.frame(
      lo = c(y_min, 0.50, 0.75, 0.90),
      hi = c(0.50, 0.75, 0.90, 1.00),
      fill = c("#FFCCCC", "#FFE0B2", "#FFFDE7", "#DCEDC8"),
      label = c("Poor", "Moderate", "Good", "Excellent"),
      stringsAsFactors = FALSE
    )
    cuts <- c(0.50, 0.75, 0.90)
  } else {
    bands <- data.frame(
      lo = c(y_min, 0.20, 0.60, 0.70, 0.90),
      hi = c(0.20, 0.60, 0.70, 0.90, 1.00),
      fill = c("#FFCCCC", "#FFE0B2", "#FFFDE7", "#DCEDC8", "#C8E6C9"),
      label = c("Poor", "Fair", "Moderate", "Strong", "Very Strong"),
      stringsAsFactors = FALSE
    )
    cuts <- c(0.20, 0.60, 0.70, 0.90)
  }
  bands$lo <- pmax(bands$lo, y_min)
  bands <- bands[bands$hi > bands$lo, , drop = FALSE]
  bands$mid <- (bands$lo + bands$hi) / 2
  bands$xmin <- x_min
  bands$xmax <- x_max
  bands$label_x <- label_x

  list(
    geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = lo, ymax = hi, fill = fill),
              inherit.aes = FALSE, alpha = 0.25),
    scale_fill_identity(),
    geom_hline(yintercept = cuts, color = "grey55", linetype = "dotted", linewidth = 0.4),
    geom_text(data = bands, aes(x = label_x, y = mid, label = label),
              inherit.aes = FALSE, hjust = 1, vjust = 0.5, size = annot_size,
              fontface = "italic", color = "#555555")
  )
}

# Builds the 2-panel (ICC top, CCC bottom) patchwork used by Fig4, Fig6, and SupFig.
build_reliability_summary_gg <- function(stats_df, vars, title, y_min = -0.10,
                                          theme_spec = fig4_theme,
                                          annot_size = FIG4_ANNOT_SIZE,
                                          suptitle_size = FIG4_SUPTITLE_SIZE,
                                          legend_ncol = 2,
                                          offset_step = 0.07,
                                          point_size = 2.2,
                                          line_width = 0.8,
                                          show_n_in_legend = TRUE,
                                          capitalize_mean_mcav = FALSE,
                                          caption = NULL,
                                          caption_size = 10) {
  var_order <- bp_first_order(vars$label)
  epoch_levels <- c("Baseline", "Min 1", "Min 2")

  pd <- stats_df %>%
    dplyr::filter(Variable %in% var_order) %>%
    dplyr::mutate(
      Variable = factor(Variable, levels = var_order),
      Epoch = factor(Epoch, levels = epoch_levels),
      epoch_idx = as.numeric(Epoch)
    )

  n_vars <- length(var_order)
  offsets <- (seq_len(n_vars) - (n_vars + 1) / 2) * offset_step
  pd$x_pos <- pd$epoch_idx + offsets[as.integer(pd$Variable)]
  pd$ICC3k_lo_clip <- pmax(pd$ICC3k_lo, y_min)
  pd$ICC3k_hi_clip <- pmin(pd$ICC3k_hi, 1.0)

  style <- all_rel_styles[match(var_order, all_rel_styles$Variable), ]
  color_values <- setNames(style$color, var_order)
  shape_values <- setNames(style$shape, var_order)
  lty_values   <- setNames(style$linetype, var_order)
  display_labels <- fig_display_label(var_order)
  if (capitalize_mean_mcav) {
    display_labels[display_labels == "mean MCAv"] <- "Mean MCAv"
  }
  legend_labels <- if (show_n_in_legend) {
    setNames(
      paste0(display_labels, "  (", vapply(var_order, function(v) n_label(stats_df, v), character(1)), ")"),
      var_order
    )
  } else {
    setNames(display_labels, var_order)
  }

  x_lim <- c(0.5, 3.5)
  label_x <- 3.45
  y_breaks <- reliability_y_breaks(y_min)

  build_panel <- function(stat, panel_title, show_x_axis) {
    p <- ggplot(pd, aes(x = x_pos, y = .data[[stat]], color = Variable, shape = Variable,
                        linetype = Variable, group = Variable))

    for (layer in reliability_bands(stat, y_min, x_lim[1], x_lim[2], label_x, annot_size)) {
      p <- p + layer
    }

    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = COL_DIAG, linewidth = 0.5)

    if (stat == "ICC3k") {
      p <- p + geom_errorbar(aes(ymin = ICC3k_lo_clip, ymax = ICC3k_hi_clip),
                              width = 0.05, linetype = 1, alpha = 0.45, show.legend = FALSE)
    }

    p <- p +
      geom_line(linewidth = line_width) +
      geom_point(size = point_size, stroke = 0.8) +
      scale_color_manual(values = color_values, labels = legend_labels, breaks = var_order, name = NULL) +
      scale_shape_manual(values = shape_values, labels = legend_labels, breaks = var_order, name = NULL) +
      scale_linetype_manual(values = lty_values, labels = legend_labels, breaks = var_order, name = NULL) +
      scale_x_continuous(breaks = 1:3, labels = epoch_levels, limits = x_lim, expand = expansion(add = c(0, 0))) +
      scale_y_continuous(breaks = y_breaks, limits = c(y_min, 1.0), expand = expansion(add = c(0, 0))) +
      coord_cartesian(clip = "off") +
      labs(y = if (stat == "ICC3k") "ICC(3,k)" else "CCC", title = panel_title) +
      guides(color = guide_legend(ncol = legend_ncol), shape = guide_legend(ncol = legend_ncol),
             linetype = guide_legend(ncol = legend_ncol)) +
      theme_spec

    if (!show_x_axis) {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    } else {
      p <- p + theme(axis.title.x = element_blank())
    }
    p
  }

  p_icc <- build_panel("ICC3k", "A. ICC(3,k) [95% CI]", show_x_axis = FALSE)
  p_ccc <- build_panel("CCC", "B. Lin's CCC", show_x_axis = TRUE)

  combined <- (p_icc / p_ccc) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title,
      caption = caption,
      theme = theme(
        plot.title = element_text(size = suptitle_size, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = caption_size, color = "#555555", hjust = 0)
      )
    )
  combined & theme(legend.position = "bottom")
}

# ============================================================
# Fig5 - reliability by sex / menstrual-cycle matching (2 x 3 panel grid)
# ============================================================

fig5_theme <- theme_classic(base_size = FIG5_AXIS_TEXT_SIZE) +
  theme(
    text = element_text(color = COL_TEXT),
    axis.title.x = element_markdown(size = FIG5_AXIS_TITLE_SIZE, color = COL_TEXT),
    axis.title.y = element_markdown(size = FIG5_AXIS_TITLE_SIZE, color = COL_TEXT, angle = 90),
    axis.text = element_text(size = FIG5_AXIS_TEXT_SIZE, color = COL_TEXT),
    axis.line = element_line(linewidth = 0.5, color = COL_TEXT),
    axis.ticks = element_line(linewidth = 0.4, color = COL_TEXT),
    plot.title = element_text(size = FIG5_TITLE_SIZE, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.text = element_markdown(size = FIG5_AXIS_TEXT_SIZE),
    legend.title = element_blank(),
    legend.key.width = unit(1.3, "lines"),
    plot.margin = margin(t = 4, r = 10, b = 2, l = 4)
  )

build_fig5_gg <- function(sex_df, vars,
                           theme_spec = fig5_theme,
                           annot_size = FIG5_ANNOT_SIZE,
                           suptitle_size = FIG5_SUPTITLE_SIZE,
                           legend_ncol = 4,
                           offset_step = 0.06,
                           point_size = 1.6,
                           line_width = 0.6,
                           caption_size = FIG5_CAPTION_SIZE) {
  var_order <- bp_first_order(vars$label)
  epoch_levels <- c("Baseline", "Min 1", "Min 2")
  groups <- c("Male", "Female matched", "Female unmatched")

  point_min <- min(sex_df$ICC3k, sex_df$CCC, -0.10, na.rm = TRUE)
  y_min <- max(-1.40, floor((point_min - 0.05) * 10) / 10)

  pd <- sex_df %>%
    dplyr::filter(Variable %in% var_order) %>%
    dplyr::mutate(
      Variable = factor(Variable, levels = var_order),
      Epoch = factor(Epoch, levels = epoch_levels),
      Group = factor(Group, levels = groups),
      epoch_idx = as.numeric(Epoch)
    )

  n_vars <- length(var_order)
  offsets <- (seq_len(n_vars) - (n_vars + 1) / 2) * offset_step
  pd$x_pos <- pd$epoch_idx + offsets[as.integer(pd$Variable)]
  pd$ICC3k_lo_clip <- pmax(pd$ICC3k_lo, y_min)
  pd$ICC3k_hi_clip <- pmin(pd$ICC3k_hi, 1.0)

  style <- all_rel_styles[match(var_order, all_rel_styles$Variable), ]
  color_values <- setNames(style$color, var_order)
  shape_values <- setNames(style$shape, var_order)
  lty_values   <- setNames(style$linetype, var_order)
  display_labels <- fig_display_label(var_order)
  display_labels[display_labels == "mean MCAv"] <- "Mean MCAv"
  legend_labels <- setNames(display_labels, var_order)

  x_lim <- c(0.5, 3.5)
  label_x <- 3.45
  y_breaks <- reliability_y_breaks(y_min)

  group_n <- function(g) {
    d <- sex_df[sex_df$Group == g & sex_df$Variable == "SBP", , drop = FALSE]
    n_label(d, "SBP")
  }

  build_panel <- function(stat, group, show_x_axis, show_y_title, show_band_labels, panel_title) {
    df_panel <- pd[pd$Group == group, , drop = FALSE]
    p <- ggplot(df_panel, aes(x = x_pos, y = .data[[stat]], color = Variable, shape = Variable,
                              linetype = Variable, group = Variable))

    bands <- reliability_bands(stat, y_min, x_lim[1], x_lim[2], label_x, annot_size)
    if (!show_band_labels) bands <- bands[1:3]
    for (layer in bands) p <- p + layer

    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = COL_DIAG, linewidth = 0.5)

    if (stat == "ICC3k") {
      p <- p + geom_errorbar(aes(ymin = ICC3k_lo_clip, ymax = ICC3k_hi_clip),
                              width = 0.05, linetype = 1, alpha = 0.45, show.legend = FALSE)
    }

    p <- p +
      geom_line(linewidth = line_width) +
      geom_point(size = point_size, stroke = 0.7) +
      scale_color_manual(values = color_values, labels = legend_labels, breaks = var_order, name = NULL) +
      scale_shape_manual(values = shape_values, labels = legend_labels, breaks = var_order, name = NULL) +
      scale_linetype_manual(values = lty_values, labels = legend_labels, breaks = var_order, name = NULL) +
      scale_x_continuous(breaks = 1:3, labels = epoch_levels, limits = x_lim, expand = expansion(add = c(0, 0))) +
      scale_y_continuous(breaks = y_breaks, limits = c(y_min, 1.0), expand = expansion(add = c(0, 0))) +
      coord_cartesian(clip = "off") +
      labs(
        y = if (show_y_title) (if (stat == "ICC3k") "ICC(3,k)" else "CCC") else NULL,
        title = panel_title
      ) +
      guides(color = guide_legend(ncol = legend_ncol), shape = guide_legend(ncol = legend_ncol),
             linetype = guide_legend(ncol = legend_ncol)) +
      theme_spec

    if (!show_x_axis) {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    } else {
      p <- p + theme(axis.title.x = element_blank())
    }
    if (!show_y_title) {
      p <- p + theme(axis.title.y = element_blank())
    }
    p
  }

  panels <- list()
  for (stat in c("ICC3k", "CCC")) {
    for (g in groups) {
      panel_title <- if (stat == "ICC3k") paste0(g, " (", group_n(g), ")") else NULL
      panels[[length(panels) + 1]] <- build_panel(
        stat, g,
        show_x_axis = (stat == "CCC"),
        show_y_title = (g == groups[1]),
        show_band_labels = (g == groups[length(groups)]),
        panel_title = panel_title
      )
    }
  }

  combined <- patchwork::wrap_plots(panels, ncol = 3) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = "Reliability by sex and menstrual-cycle matching",
      tag_levels = "A",
      caption = "Negative reliability point estimates are shown; extremely wide ICC confidence intervals are clipped at the axis limit.",
      theme = theme(
        plot.title = element_text(size = suptitle_size, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = caption_size, color = "#555555", hjust = 0),
        plot.tag = element_text(size = FIG5_TAG_SIZE, face = "bold")
      )
    )
  combined & theme(legend.position = "bottom")
}

# ============================================================
# Fig1 - CPT time-course (4 x 2 panel grid)
# ============================================================

fig1_theme <- theme_classic(base_size = 10) +
  theme(
    text = element_text(color = COL_TEXT),
    axis.title.x = element_markdown(size = FIG1_AXIS_TITLE_SIZE, color = COL_TEXT),
    axis.title.y = element_markdown(size = FIG1_AXIS_TITLE_SIZE, color = COL_TEXT, angle = 90),
    axis.text = element_text(size = FIG1_AXIS_TEXT_SIZE, color = COL_TEXT),
    axis.line = element_line(linewidth = 0.5, color = COL_TEXT),
    axis.ticks = element_line(linewidth = 0.4, color = COL_TEXT),
    plot.title = element_markdown(size = FIG1_TITLE_SIZE, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = FIG1_AXIS_TEXT_SIZE + 1),
    legend.title = element_blank(),
    plot.margin = margin(t = 4, r = 10, b = 4, l = 4)
  )

build_fig1_gg <- function(df, anova_df = NULL, posthoc_df = NULL,
                           theme_spec = fig1_theme, annot_size = FIG1_ANNOT_SIZE,
                           suptitle_size = FIG1_SUPTITLE_SIZE) {
  epoch_levels <- c("Baseline", "Min 1", "Min 2")
  labels <- fig1_var_label(fig1_vars$ylabel)
  ordered_vars <- fig1_vars[match(bp_first_order(labels), labels), ]
  summary_all <- build_fig1_timecourse_summary(df)
  set.seed(42)

  sig_star <- function(p) {
    if (is.na(p) || p >= 0.05) return(NULL)
    if (p < 0.001) "***" else if (p < 0.01) "**" else "*"
  }
  fmt_p_short <- function(p) {
    if (is.na(p) || p >= 0.05) return("ns")
    if (p < 0.001) return("<.001")
    sprintf("=%.3f", p)
  }

  panels <- list()
  for (i in seq_len(nrow(ordered_vars))) {
    var <- ordered_vars[i, ]
    label <- fig1_var_label(var$ylabel)
    long <- long_for_var(df, var$var_key, var$stem, delta_baseline = TRUE)
    long$epoch_idx <- match(long$time, epoch_levels)

    summary_var <- summary_all[summary_all$Variable == label, , drop = FALSE]
    summary_var$epoch_idx <- match(summary_var$Epoch, epoch_levels)
    summary_var$offset <- ifelse(summary_var$Visit == "V1", -0.07, 0.07)
    summary_var$x_pos <- summary_var$epoch_idx + summary_var$offset

    jitter_df <- long[long$time != "Baseline", , drop = FALSE]
    jitter_df$offset <- ifelse(jitter_df$visit == "V1", -0.07, 0.07)
    jitter_df$x_jit <- jitter_df$epoch_idx + jitter_df$offset + rnorm(nrow(jitter_df), 0, 0.025)

    y_range <- range(jitter_df$value, summary_var$Center - summary_var$Err_lo,
                      summary_var$Center + summary_var$Err_hi, na.rm = TRUE)
    yb <- nice_breaks(y_range[1], y_range[2], n = 5)

    # Post-hoc significant comparisons (Bonferroni-corrected)
    ph_var <- if (!is.null(posthoc_df)) posthoc_df[posthoc_df$Variable == label, , drop = FALSE] else NULL
    get_ph_p <- function(comp) {
      if (is.null(ph_var)) return(NA_real_)
      r <- ph_var[ph_var$Comparison == comp, ]
      if (nrow(r) == 0) NA_real_ else r$P_bonferroni[1]
    }
    s_b1 <- sig_star(get_ph_p("Baseline vs Min 1"))
    s_b2 <- sig_star(get_ph_p("Baseline vs Min 2"))
    s_12 <- sig_star(get_ph_p("Min 1 vs Min 2"))

    # Extend y upper limit to give room for significance brackets if needed
    n_brackets <- sum(!vapply(list(s_12), is.null, logical(1)))
    bracket_pad <- diff(yb$limits) * 0.16 * max(1, n_brackets)
    y_top <- yb$limits[2] + bracket_pad
    y_n <- yb$limits[2] - 0.04 * diff(yb$limits)

    ylabel_full <- var$ylabel
    for (k in names(fig_label_map)) ylabel_full <- gsub(k, fig_label_map[[k]], ylabel_full, fixed = TRUE)
    ylabel_full <- sub("^Delta ", "Δ ", ylabel_full)

    n_subj <- length(unique(long$pid))

    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = COL_DIAG, linewidth = 0.5) +
      geom_point(data = jitter_df, aes(x = x_jit, y = value, color = visit),
                 alpha = 0.14, size = 0.9, shape = 16, show.legend = FALSE) +
      geom_errorbar(data = summary_var, aes(x = x_pos, ymin = Center - Err_lo, ymax = Center + Err_hi, color = Visit),
                    width = 0.07, linewidth = 0.7, show.legend = FALSE) +
      geom_line(data = summary_var, aes(x = x_pos, y = Center, color = Visit, linetype = Visit, group = Visit),
                linewidth = 0.9) +
      geom_point(data = summary_var, aes(x = x_pos, y = Center, color = Visit, shape = Visit), size = 2.2) +
      scale_color_manual(values = c(V1 = COL_V1, V2 = COL_V2), name = NULL) +
      scale_shape_manual(values = c(V1 = 16, V2 = 15), name = NULL) +
      scale_linetype_manual(values = c(V1 = "solid", V2 = "dashed"), name = NULL) +
      scale_x_continuous(breaks = 1:3, labels = epoch_levels, limits = c(0.6, 3.4), expand = expansion(add = c(0, 0))) +
      scale_y_continuous(breaks = yb$breaks, limits = yb$limits, expand = expansion(add = c(0, 0))) +
      coord_cartesian(clip = "off", ylim = c(yb$limits[1], y_top)) +
      annotate("text", x = 3.35, y = y_n, label = paste0("n=", n_subj), hjust = 1, vjust = 1,
               size = annot_size, color = "grey40") +
      labs(x = NULL, y = ylabel_full, title = fig_display_label(label)) +
      theme_spec

    # Simple significance markers: star above each epoch vs Baseline,
    # plus a bracket between Min 1 and Min 2 if those differ.
    star_y <- yb$limits[2] + diff(yb$limits) * 0.06
    bar_h  <- diff(yb$limits) * 0.03

    if (!is.null(s_b1)) {
      p <- p + annotate("text", x = 2, y = star_y, label = s_b1, vjust = 0,
                        size = annot_size * 1.2, color = COL_TEXT)
    }
    if (!is.null(s_b2)) {
      p <- p + annotate("text", x = 3, y = star_y, label = s_b2, vjust = 0,
                        size = annot_size * 1.2, color = COL_TEXT)
    }
    if (!is.null(s_12)) {
      bk_y <- yb$limits[2] + diff(yb$limits) * 0.10
      p <- p +
        annotate("segment", x = 2, xend = 3, y = bk_y, yend = bk_y, linewidth = 0.4, color = COL_TEXT) +
        annotate("segment", x = 2, xend = 2, y = bk_y - bar_h, yend = bk_y, linewidth = 0.4, color = COL_TEXT) +
        annotate("segment", x = 3, xend = 3, y = bk_y - bar_h, yend = bk_y, linewidth = 0.4, color = COL_TEXT) +
        annotate("text", x = 2.5, y = bk_y + bar_h * 0.5, label = s_12, vjust = 0,
                 size = annot_size * 1.2, color = COL_TEXT)
    }

    # ANOVA stats annotation (bottom-left corner)
    if (!is.null(anova_df)) {
      av <- anova_df[anova_df$Variable == label, , drop = FALSE]
      get_av <- function(eff) av[av$Effect == eff, ]
      tr <- get_av("time"); vr <- get_av("visit"); ir <- get_av("visit:time")
      if (nrow(tr) > 0) {
        atext <- sprintf(
          "Time: F(%d,%d)=%.1f, p%s, ηp²=%.2f\nVisit: p%s; Time×Visit: p%s",
          tr$`Num DF`, tr$`Den DF`, tr$F, fmt_p_short(tr$P), tr$partial_eta_sq,
          fmt_p_short(vr$P[1]), fmt_p_short(ir$P[1])
        )
        p <- p + annotate("text", x = 0.65, y = yb$limits[1] + diff(yb$limits) * 0.02,
                          label = atext, hjust = 0, vjust = 0,
                          size = annot_size * 0.82, color = "grey35",
                          lineheight = 0.9)
      }
    }

    panels[[i]] <- p
  }

  combined <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = "CPT response: baseline reference with minute 1 and minute 2 Δ values",
      tag_levels = "A",
      theme = theme(plot.title = element_text(size = suptitle_size, face = "bold", hjust = 0.5))
    )
  combined & theme(legend.position = "bottom")
}

# ============================================================
# Fig2 / Fig3 - Bland-Altman grids (4 rows x 3 epoch columns)
# Requires build_paired(), ba_stats(), linregress(), epochs, epoch_label
# from 03_reliability_tables.R (sourced before this file).
# ============================================================

fig23_theme <- theme_classic(base_size = FIG23_AXIS_TEXT_SIZE) +
  theme(
    text = element_text(color = COL_TEXT),
    axis.title.x = element_markdown(size = FIG23_AXIS_TITLE_SIZE, color = COL_TEXT),
    axis.title.y = element_markdown(size = FIG23_AXIS_TITLE_SIZE, color = COL_TEXT, angle = 90),
    axis.text = element_text(size = FIG23_AXIS_TEXT_SIZE, color = COL_TEXT),
    axis.line = element_line(linewidth = 0.4, color = COL_TEXT),
    axis.ticks = element_line(linewidth = 0.35, color = COL_TEXT),
    plot.title = element_text(size = FIG23_TITLE_SIZE, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 8, r = 6, b = 4, l = 4)
  )

# Mirrors Python's ba_line_values(): bias and limits-of-agreement lines
# evaluated at x, using the same bias/heteroscedasticity model ba_stats() picked.
ba_line_values_r <- function(dp, x) {
  b <- ba_stats(dp)
  mean_val <- (dp$v1 + dp$v2) / 2
  diff_val <- dp$v1 - dp$v2
  pb <- linregress(mean_val, diff_val)

  if (b$LoA_type == "standard") {
    return(list(
      bias = rep(b$Mean_diff, length(x)),
      loa_lo = rep(b$LoA_lo, length(x)),
      loa_hi = rep(b$LoA_hi, length(x))
    ))
  }

  use_regression <- grepl("regression", b$LoA_type)
  bias <- if (use_regression) pb$intercept + pb$slope * x else rep(b$Mean_diff, length(x))
  fitted <- if (use_regression) pb$intercept + pb$slope * mean_val else rep(b$Mean_diff, length(diff_val))
  resid <- diff_val - fitted

  if (grepl("fan", b$LoA_type)) {
    sd_fit <- linregress(mean_val, abs(resid))
    floor_val <- max(mean(abs(resid)) * 0.25, 1e-6)
    sd_line <- pmax(sd_fit$intercept + sd_fit$slope * x, floor_val)
  } else {
    n <- length(resid)
    sd_line <- rep(stats::sd(resid) * sqrt((n - 1) / (n - 2)), length(x))
  }

  list(bias = bias, loa_lo = bias - 1.96 * sd_line, loa_hi = bias + 1.96 * sd_line)
}

# One Bland-Altman panel: scatter of (mean, V1-V2), bias line (garnet) and
# LoA lines (gold dashed), with a zero-reference line and a corner n= label.
build_ba_panel <- function(dp, panel_title, y_breaks, row_label = NULL, show_x_title = FALSE,
                            theme_spec = fig23_theme, annot_size = FIG23_ANNOT_SIZE) {
  n <- nrow(dp)

  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = COL_DIAG, linewidth = 0.4)

  if (n >= 4) {
    mean_val <- (dp$v1 + dp$v2) / 2
    diff_val <- dp$v1 - dp$v2
    x_range <- range(mean_val)
    xb <- nice_breaks(x_range[1], x_range[2], n = 5)
    x_seq <- seq(xb$limits[1], xb$limits[2], length.out = 100)
    lv <- ba_line_values_r(dp, x_seq)
    line_df <- data.frame(x = x_seq, bias = lv$bias, loa_lo = lv$loa_lo, loa_hi = lv$loa_hi)
    point_df <- data.frame(x = mean_val, y = diff_val)

    p <- p +
      geom_line(data = line_df, aes(x = x, y = loa_hi), color = FSU_GOLD, linewidth = 0.6, linetype = "dashed") +
      geom_line(data = line_df, aes(x = x, y = loa_lo), color = FSU_GOLD, linewidth = 0.6, linetype = "dashed") +
      geom_line(data = line_df, aes(x = x, y = bias), color = FSU_GARNET, linewidth = 0.8) +
      geom_point(data = point_df, aes(x = x, y = y), color = FSU_GARNET, alpha = 0.7, size = 1.4, shape = 16) +
      scale_x_continuous(breaks = xb$breaks, limits = xb$limits, expand = expansion(add = c(0, 0)))
  } else {
    xb <- nice_breaks(0, 1, n = 5)
    p <- p + scale_x_continuous(breaks = xb$breaks, limits = xb$limits, expand = expansion(add = c(0, 0)))
  }

  x_n <- xb$limits[2] - 0.02 * diff(xb$limits)
  y_n <- y_breaks$limits[2] - 0.04 * diff(y_breaks$limits)

  p +
    scale_y_continuous(breaks = y_breaks$breaks, limits = y_breaks$limits, expand = expansion(add = c(0, 0))) +
    coord_cartesian(clip = "off") +
    annotate("text", x = x_n, y = y_n, label = paste0("n=", n), hjust = 1, vjust = 1,
             size = annot_size, color = "grey40") +
    labs(title = panel_title, x = if (show_x_title) "Mean (V1, V2)" else NULL, y = row_label) +
    theme_spec +
    (if (!show_x_title) theme(axis.title.x = element_blank()) else NULL)
}

# Builds a 4-var x 3-epoch Bland-Altman grid. Row y-limits are shared (pooled
# across that row's 3 epochs, including bias/LoA lines) via nice_breaks(); the
# row label (variable + units + "V1 - V2") becomes the y-axis title of column 1
# only, avoiding the old mtext-based row-label clipping.
build_ba_grid_gg <- function(df, vars_df, title,
                              theme_spec = fig23_theme,
                              annot_size = FIG23_ANNOT_SIZE,
                              suptitle_size = FIG23_SUPTITLE_SIZE,
                              tag_size = FIG23_TITLE_SIZE,
                              capitalize_mean_mcav = FALSE) {
  ba_epoch_label <- c(Base = "Baseline", `1min` = "Minute 1", `2min` = "Minute 2")
  var_order <- bp_first_order(vars_df$label)
  ordered_vars <- vars_df[match(var_order, vars_df$label), ]

  panels <- list()
  for (i in seq_len(nrow(ordered_vars))) {
    var <- ordered_vars[i, ]
    row_dps <- lapply(epochs, function(ep) build_paired(df, var$var_key, var$stem, ep, delta_baseline = FALSE))

    y_vals <- numeric(0)
    for (dp in row_dps) {
      if (nrow(dp) >= 4) {
        mean_val <- (dp$v1 + dp$v2) / 2
        diff_val <- dp$v1 - dp$v2
        x_seq <- seq(min(mean_val), max(mean_val), length.out = 100)
        lv <- ba_line_values_r(dp, x_seq)
        y_vals <- c(y_vals, diff_val, lv$bias, lv$loa_lo, lv$loa_hi)
      }
    }
    yb <- nice_breaks(min(y_vals), max(y_vals), n = 5)

    units_line <- if (nchar(var$units) > 0 && var$units != "ratio") paste0("(", var$units, ")") else NULL
    display_label <- fig_display_label(var$label)
    if (capitalize_mean_mcav && identical(display_label, "mean MCAv")) {
      display_label <- "Mean MCAv"
    }
    # Units get their own line so the rotated row label (which grows with the
    # 25% panel-title/axis-title bump) doesn't overflow into the row above.
    row_label <- paste(c(display_label, units_line, "V1 - V2"), collapse = "\n")

    for (j in seq_along(epochs)) {
      panel_title <- unname(ba_epoch_label[epochs[j]])
      p <- build_ba_panel(
        row_dps[[j]], panel_title, yb,
        row_label = if (j == 1) row_label else NULL,
        show_x_title = (i == nrow(ordered_vars)),
        theme_spec = theme_spec, annot_size = annot_size
      )
      panels[[length(panels) + 1]] <- p
    }
  }

  patchwork::wrap_plots(panels, ncol = 3) +
    patchwork::plot_annotation(
      title = title, tag_levels = "A",
      theme = theme(
        plot.title = element_text(size = suptitle_size, face = "bold", hjust = 0.5),
        plot.tag = element_text(size = tag_size, face = "bold")
      )
    )
}

# ============================================================
# Orchestrator - builds and saves all 7 figures
# ============================================================
render_all_figures_r <- function(df, config) {
  stats_df <- read.csv(file.path(config$output_dir, "BA_ICC_CCC_Statistics.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  fig6_df  <- read.csv(file.path(config$output_dir, "Fig6_ICC_CCC_Exclude_Unmatched_Females_Stats.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  sex_df   <- read.csv(file.path(config$output_dir, "Fig5_ICC_CCC_Sex_Menstrual_Stats.csv"), check.names = FALSE, stringsAsFactors = FALSE)

  anova_df   <- tryCatch(read.csv(file.path(config$output_dir, "Fig1_ANOVA_Time_Visit_Results.csv"), check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
  posthoc_df <- tryCatch(read.csv(file.path(config$output_dir, "Fig1_Posthoc_Bonferroni.csv"),       check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)

  d <- figure_dims
  save1 <- function(plot, name) save_fig(plot, name, d[[name]][["width"]], d[[name]][["height"]], config)

  save1(build_fig1_gg(df, anova_df = anova_df, posthoc_df = posthoc_df), "Fig1_CPT_TimeCourse")

  save1(build_ba_grid_gg(df, fig2_fig3_vars[1:4, ], "Bland-Altman plots: cardiovascular variables"),
        "Fig2_BA_Cardiovascular")

  save1(build_ba_grid_gg(
          df, fig2_fig3_vars[5:8, ], "Bland-Altman plots: cerebrovascular variables",
          capitalize_mean_mcav = TRUE
        ),
        "Fig3_BA_Cerebrovascular")

  save1(build_reliability_summary_gg(stats_df, fig2_fig3_vars, "ICC and CCC by epoch", y_min = -0.10,
                                      capitalize_mean_mcav = TRUE,
                                      caption = "ICC and CCC represent agreement between Visit 1 and Visit 2."),
        "Fig4_ICC_CCC_Summary")

  save1(build_fig5_gg(sex_df, fig2_fig3_vars), "Fig5_ICC_CCC_Sex_Menstrual")

  save1(build_reliability_summary_gg(fig6_df, fig2_fig3_vars, "ICC and CCC by epoch: unmatched females excluded", y_min = -0.10,
                                      capitalize_mean_mcav = TRUE,
                                      caption = "ICC and CCC represent agreement between Visit 1 and Visit 2."),
        "Fig6_ICC_CCC_Exclude_Unmatched_Females")

  save1(
    build_reliability_summary_gg(
      stats_df, all15, "ICC and CCC by epoch: all variables", y_min = -0.10,
      theme_spec = figs_theme, annot_size = FIGS_ANNOT_SIZE, suptitle_size = FIGS_SUPTITLE_SIZE,
      legend_ncol = 5, offset_step = 0.045, point_size = 1.8, line_width = 0.6,
      show_n_in_legend = FALSE,
      capitalize_mean_mcav = TRUE,
      caption = "n = 41–49 depending on variable and epoch; see Table 2 and Table 3 for per-variable n.",
      caption_size = FIGS_CAPTION_SIZE
    ),
    "SupFig1_ICC_CCC_AllVars"
  )

  sup2_vars <- tibble::tribble(
    ~var_key,    ~stem,            ~label,       ~units,
    "mcav_peak", "MCAv peak",      "MCAv peak",  "cm/s",
    "mcav_min",  "MCAv minimum",   "MCAv min",   "cm/s",
    "cvri",      "MCAv Resis",     "CVRi",       "mmHg·s/cm",
    "smo2",      "SmO2",           "SmO2",       "%"
  )
  save1(build_ba_grid_gg(df, sup2_vars,
                         "Bland-Altman plots: MCAv peak, MCAv min, CVRi, and SmO2"),
        "SupFig2_BA_MCAv_CVRi_SmO2")

  invisible(NULL)
}
