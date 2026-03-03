#' Create a forest plot of book-level polarization reduction effects
#'
#' Generates a forest plot displaying mean reduction in affective
#' polarization across books, including 95% confidence intervals.
#'
#' @param forest_df A data frame produced by `prepare_forest_books()`,
#' containing `book`, `mean`, `lower`, and `upper` columns.
#' @param label_cols The names of the columns to be included on the side
#' of the plot.
#' @param header Labels of the columns to be displayed and as specified in
#' `label_cols`.
#' @param title Plot title
#' @param xlab X-axis label
#' @param zero Where should the "zero" axis (graph) start.
#' @param show_overall Logical. If TRUE (default), a vertical dashed
#' line indicating the overall mean effect is added.
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
#' @export
plot_forest_books <- function(
  forest_df,
  label_cols = c("book", "ci"),
  header = NULL,
  title = "",
  xlab = "",
  zero = 0,
  show_overall = TRUE,
  ci.vertices = FALSE
) {
  forest_df <- forest_df |>
    dplyr::arrange(
      dplyr::across(dplyr::any_of("party")),
      dplyr::desc(mean)
    )

  if (show_overall) {
    line_pos <- mean(forest_df$mean)
    attr(line_pos, "gp") <- grid::gpar(col = "darkgray", lwd = 2, lty = 2)
  }

  # Build label matrix from column names
  label_mat <- as.matrix(forest_df[, label_cols, drop = FALSE])

  forest_df$labeltext <- forest_df$book

  if (dplyr::is_grouped_df(forest_df)) {
    fn.ci_norm <- c(
      forestplot::fpDrawCircleCI, # Democrat
      forestplot::fpDrawDiamondCI # Republican
    )
  } else {
    fn.ci_norm <- forestplot::fpDrawCircleCI
  }

  p <- forestplot::forestplot(
    forest_df,
    labeltext = eval(label_cols),
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
    fn.ci_norm = c(
      forestplot::fpDrawCircleCI, # Democrat
      forestplot::fpDrawDiamondCI # Republican
    ),
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
#' estimates of affective polarization reduction. This function assumes
#' the input is already aggregated at the book level (e.g., using
#' `summarize_chapter_scores(..., aggregate_level = "book")`).
#'
#' @param summary_books A data frame containing at least the following columns:
#' @param add_ci_label Logical. Default is TRUE. If TRUE, a formatted
#'   character column `ci` is added containing the estimate and its
#'   95% confidence interval in the format:
#'   `mean [lower, upper]`.
#'   If FALSE, only numeric columns (`mean`, `se`, `lower`, `upper`)
#'   are returned.
#' @param digits Integer. Default is 2. Number of decimal places used
#'   when formatting the `ci` column. Ignored if `add_ci_label = FALSE`.
#' \describe{
#'   \item{book}{Character string identifying the book.}
#'   \item{sim}{Number of simulations.}
#'   \item{mean_diff}{Mean reduction score.}
#'   \item{sd_diff}{Standard deviation of the reduction score.}
#' }
#'
#' @return A tibble with added columns:
#' \describe{
#'   \item{mean}{Mean effect (copied from `mean_diff`).}
#'   \item{se}{Standard error of the mean.}
#'   \item{lower}{Lower bound of the 95% CI.}
#'   \item{upper}{Upper bound of the 95% CI.}
#' }
#'
#' @details
#' Standard errors are computed as `sd_diff / sqrt(sim)`. Confidence
#' intervals are calculated using a normal approximation
#' (`mean +/- 1.96 * SE`).
#'
#' @export
prepare_forest_books <- function(
  summary_books,
  add_ci_label = TRUE,
  digits = 2
) {
  if (dplyr::is_grouped_df(summary_books)) {
    ob.length <- length(unique(unlist(attributes(
      grouped_summary_books
    )$groups[1])))
    summary_books$sim <- summary_books$sim * ob.length
  }

  out <- dplyr::mutate(
    summary_books,
    book = paste0(.data$book, " (n = ", .data$sim, ")"),
    mean = .data$mean_diff,
    se = .data$sd_diff / sqrt(.data$sim),
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
# prepare_forest_books <- function(summary_books) {
#   summary_books |>
#     dplyr::mutate(
#       book = paste0(book, " (n = ", sim, ")"),
#       mean = .data[["mean_diff"]],
#       se = .data[["sd_diff"]] / sqrt(sim),
#       lower = mean - 1.96 * .data[["se"]],
#       upper = mean + 1.96 * .data[["se"]]
#     )
# }

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
