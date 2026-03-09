#' Create a forest plot of book-level polarization reduction effects
#'
#' Generates a forest plot displaying mean reduction in affective
#' polarization across books, including 95% confidence intervals.
#'
#' @param forest_df Either:
#'   - A data frame produced by `prepare_forest_books()` containing
#'     `book`, `mean`, `lower`, and `upper`, or
#'   - A summary data frame (for example from
#'     `summarize_chapter_scores(..., aggregate_level = "book")`) with
#'     `mean_{dv}` and `sd_{dv}` columns.
#' @param dv Character. Variable prefix used when `forest_df` is not already
#'   prepared. Defaults to `"delta_gap"`.
#' @param add_ci_label Logical. Passed to `prepare_forest_books()` when
#'   internal preparation is needed. Defaults to `TRUE`.
#' @param digits Integer. Passed to `prepare_forest_books()` when internal
#'   preparation is needed. Defaults to 2.
#' @param label_cols The names of the columns to be included on the side
#' of the plot.
#' @param header Labels of the columns to be displayed and as specified in
#' `label_cols`.
#' @param title Plot title
#' @param xlab X-axis label
#' @param zero Where should the "zero" axis (graph) start.
#' @param show_overall Logical. If TRUE (default), a vertical dashed
#' line indicating the overall mean effect is added.
#' @param ci.vertices Logical. Whether to draw CI vertices in the forest plot.
#'
#' @return A `forestplot` grob object.
#'
#' @details
#' Books are ordered from strongest to weakest mean effect.
#'
#' The plot uses circular markers for point estimates and displays
#' 95% confidence intervals.
#'
#' The vertical dashed line (if enabled) represents the average
#' effect across books.
#'
#' The temporary `mean/lower/upper` columns required by `forestplot` are
#' generated internally when needed.
#'
#' @export
plot_forest_books <- function(
  forest_df,
  dv = "delta_gap",
  add_ci_label = TRUE,
  digits = 2,
  label_cols = c("book", "ci"),
  header = NULL,
  title = "",
  xlab = "",
  zero = 0,
  show_overall = TRUE,
  ci.vertices = FALSE
) {
  required_cols <- c("mean", "lower", "upper")
  if (!all(required_cols %in% names(forest_df))) {
    forest_df <- prepare_forest_books(
      summary_books = forest_df,
      dv = dv,
      add_ci_label = add_ci_label,
      digits = digits
    )
  }

  missing_label_cols <- setdiff(label_cols, names(forest_df))
  if (length(missing_label_cols) > 0) {
    stop(
      "`label_cols` not found in data: ",
      paste(missing_label_cols, collapse = ", ")
    )
  }

  forest_df <- forest_df |>
    dplyr::arrange(
      dplyr::across(dplyr::any_of("party")),
      dplyr::desc(mean)
    )

  if (show_overall) {
    line_pos <- mean(forest_df$mean)
    attr(line_pos, "gp") <- grid::gpar(col = "darkgray", lwd = 2, lty = 2)
  }

  # Build label matrix from column names (forestplot needs a plain matrix)
  label_mat <- as.matrix(forest_df[, label_cols, drop = FALSE])

  if (dplyr::is_grouped_df(forest_df)) {
    fn.ci_norm <- c(
      forestplot::fpDrawCircleCI, # e.g. Democrat
      forestplot::fpDrawDiamondCI # e.g. Republican
    )
  } else {
    fn.ci_norm <- forestplot::fpDrawCircleCI
  }

  # Convert to plain data.frame so forestplot doesn't choke on tibble
  forest_df <- as.data.frame(forest_df)

  p <- forestplot::forestplot(
    forest_df,
    labeltext = label_mat,
    mean = "mean",
    lower = "lower",
    upper = "upper",
    title = title,
    xlab = xlab,
    zero = zero,
    ci.vertices = ci.vertices,
    legend_args = forestplot::fpLegend(
      pos = list(x = 0.85, y = 0.15),
      gp = grid::gpar(
        col = "#CCCCCC",
        fill = "#F9F9F9"
      )
    ),
    fn.ci_norm = fn.ci_norm,
    txt_gp = forestplot::fpTxtGp(
      label = grid::gpar(fontsize = 10),
      ticks = grid::gpar(fontsize = 18),
      xlab = grid::gpar(fontsize = 18),
      legend = grid::gpar(fontsize = 12)
    ),
  ) |>
    forestplot::fp_set_style(
      box = "black",
      line = grid::gpar(col = "black", lwd = 2)
    ) |>
    forestplot::fp_set_zebra_style("#EFEFEF") |>
    forestplot::fp_decorate_graph(graph.pos = 2)

  if ("party" %in% names(forest_df)) {
    group_levels <- tolower(levels(as.factor(forest_df$party)))
    if (all(group_levels %in% c("democrat", "republican"))) {
      p <- p |>
        forestplot::fp_set_style(box = c("#00AEF3", "#E81B23"))
    }
  }

  if (!is.null(header)) {
    p <- do.call(forestplot::fp_add_header, c(list(p), as.list(header)))
  }

  if (show_overall) {
    p <- p |>
      forestplot::fp_decorate_graph(
        grid = line_pos,
        left_top_txt = forestplot::fp_txt_gp(
          "Vertical line = overall mean",
          gp = grid::gpar(col = "#757575", cex = 0.8)
        ),
        right_bottom_txt = forestplot::fp_txt_gp(
          "",
          gp = ""
        )
      )
  }

  return(p)
}

#' Prepare book-level data for forest plotting
#'
#' Computes standard errors and 95% confidence intervals for book-level
#' estimates. This function assumes the input is already aggregated at the
#' book level (e.g., using
#' `summarize_chapter_scores(..., aggregate_level = "book")`).
#'
#' @param summary_books A data frame containing at least `book`, `sim`, and
#'   mean/sd columns matching the `dv` prefix pattern.
#' @param dv Character. The variable prefix to use for the forest plot.
#'   Defaults to `"delta_gap"`. The function looks for
#'   `mean_{dv}` and `sd_{dv}` columns.
#' @param add_ci_label Logical. Default is TRUE. If TRUE, a formatted
#'   character column `ci` is added containing the estimate and its
#'   95% confidence interval in the format:
#'   `mean [lower, upper]`.
#'   If FALSE, only numeric columns (`mean`, `se`, `lower`, `upper`)
#'   are returned.
#' @param digits Integer. Default is 2. Number of decimal places used
#'   when formatting the `ci` column. Ignored if `add_ci_label = FALSE`.
#'
#' @return A tibble with added columns:
#' \describe{
#'   \item{mean}{Mean effect.}
#'   \item{se}{Standard error of the mean.}
#'   \item{lower}{Lower bound of the 95% CI.}
#'   \item{upper}{Upper bound of the 95% CI.}
#' }
#'
#' @details
#' Standard errors are computed as `sd / sqrt(sim)`. Confidence intervals
#' are calculated using a normal approximation (`mean +/- 1.96 * SE`).
#'
#' @export
prepare_forest_books <- function(
  summary_books,
  dv = "delta_gap",
  add_ci_label = TRUE,
  digits = 2
) {
  mean_col <- paste0("mean_", dv)
  sd_col <- paste0("sd_", dv)

  if (!mean_col %in% names(summary_books)) {
    stop(
      "Column '",
      mean_col,
      "' not found in summary_books. ",
      "Available columns: ",
      paste(names(summary_books), collapse = ", ")
    )
  }

  if (dplyr::is_grouped_df(summary_books)) {
    n_groups <- dplyr::n_groups(summary_books)
    summary_books$sim <- summary_books$sim * n_groups
  }

  out <- dplyr::mutate(
    summary_books,
    book = paste0(.data$book, " (n = ", .data$sim, ")"),
    mean = .data[[mean_col]],
    se = .data[[sd_col]] / sqrt(.data$sim),
    lower = .data$mean - 1.96 * .data$se,
    upper = .data$mean + 1.96 * .data$se
  )

  if (add_ci_label) {
    out <- dplyr::mutate(
      out,
      ci = paste0(
        round(.data$mean, digits),
        " [",
        round(.data$lower, digits),
        ", ",
        round(.data$upper, digits),
        "]"
      )
    )
  }

  return(out)
}

#' Save a forest plot to PNG and PDF formats
#'
#' Exports a forestplot object to both PNG and PDF files using
#' grid graphics devices.
#'
#' @param plot_object A forestplot grob object.
#' @param filename Character string specifying the file path
#' without extension.
#' @param width Width of the output figure in inches.
#' @param height Height of the output figure in inches.
#' @param res Resolution in DPI for the PNG output (default = 300).
#'
#' @details
#' Because `forestplot` uses grid graphics (not ggplot2),
#' `ggsave()` is not compatible. This function opens graphics
#' devices manually and prints the plot object.
#'
#' Two files are created:
#' \describe{
#'   \item{`filename.png`}{High-resolution raster image}
#'   \item{`filename.pdf`}{Vector-based PDF}
#' }
#'
#' @export
save_forest_plot <- function(
  plot_object,
  filename,
  width = 16 / 1.8,
  height = 9 / 1.8,
  res = 300
) {
  grDevices::png(
    paste0(filename, ".png"),
    width = width,
    height = height,
    units = "in",
    res = res
  )
  print(plot_object)
  grDevices::dev.off()

  grDevices::pdf(paste0(filename, ".pdf"), width = width, height = height)
  print(plot_object)
  grDevices::dev.off()
}
