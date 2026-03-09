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
#' @param label_cols Character vector of left-side label columns.
#'   Defaults to `"book"`.
#' @param show_ci_label Logical. If TRUE, appends an internally generated `ci`
#'   label column to the right-side text.
#' @param ci_multiline Logical. For grouped party output, print each party CI on
#'   its own line (using `\n`) when TRUE.
#' @param ci_show_party Logical. Include party names in CI labels.
#' @param show_legend Logical. Show party legend when grouped data are present.
#' @param ci_label_fontsize Optional numeric size for the CI label column.
#'   Useful when grouped party CIs are shown on multiple lines.
#' @param ci_label_lineheight Numeric line height for the CI label column when
#'   `ci_label_fontsize` is set.
#' @param header Labels of the columns to be displayed and as specified in
#'   `label_cols`.
#' @param title Plot title
#' @param xlab X-axis label
#' @param zero Numeric scalar, NA, or NULL. Reference line position for
#'   forestplot. Defaults to NA (no zero/reference line). NULL is
#'   treated as NA for convenience.
#' @param show_overall Logical. If TRUE (default), a vertical dashed
#'   line indicating the overall mean effect is added.
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
#' If `party` is present, estimates are drawn as multiple CIs per book row
#' (one row per book; one estimate per party).
#'
#' @export
plot_forest_books <- function(
  forest_df,
  dv = "delta_gap",
  add_ci_label = TRUE,
  digits = 2,
  label_cols = c("book"),
  show_ci_label = TRUE,
  ci_multiline = TRUE,
  ci_show_party = FALSE,
  show_legend = TRUE,
  ci_label_fontsize = NULL,
  ci_label_lineheight = 0.85,
  header = NULL,
  title = "",
  xlab = "",
  zero = NA,
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

  input <- .build_forestplot_inputs(
    forest_df = forest_df,
    label_cols = label_cols,
    digits = digits,
    show_ci_label = show_ci_label,
    ci_multiline = ci_multiline,
    ci_show_party = ci_show_party
  )

  if (show_overall) {
    line_pos <- mean(as.numeric(input$mean), na.rm = TRUE)
    attr(line_pos, "gp") <- grid::gpar(col = "darkgray", lwd = 2, lty = 2)
  }

  n_estimates <- if (is.matrix(input$mean)) ncol(input$mean) else 1
  fn_cis <- if (n_estimates == 1) {
    forestplot::fpDrawCircleCI
  } else {
    out <- rep(list(forestplot::fpDrawCircleCI), n_estimates)
    if (n_estimates >= 2) {
      out[[2]] <- forestplot::fpDrawDiamondCI
    }
    out
  }

  label_gp <- grid::gpar(fontsize = 10)
  if ("ci" %in% colnames(input$label_mat) && !is.null(ci_label_fontsize)) {
    ci_idx <- match("ci", colnames(input$label_mat))
    label_gp <- rep(list(grid::gpar(fontsize = 10)), ncol(input$label_mat))
    label_gp[[ci_idx]] <- grid::gpar(
      fontsize = ci_label_fontsize,
      lineheight = ci_label_lineheight
    )
  }
  fp_args <- list(
    labeltext = input$label_mat,
    mean = input$mean,
    lower = input$lower,
    upper = input$upper,
    title = title,
    xlab = xlab,
    ci.vertices = ci.vertices,
    legend_args = forestplot::fpLegend(
      pos = list(x = 0.85, y = 0.15),
      gp = grid::gpar(col = "#CCCCCC", fill = "#F9F9F9")
    ),
    fn.ci_norm = fn_cis,
    txt_gp = forestplot::fpTxtGp(
      label = label_gp,
      ticks = grid::gpar(fontsize = 18),
      xlab = grid::gpar(fontsize = 18),
      legend = grid::gpar(fontsize = 12)
    )
  )

  if (!is.null(input$party_levels) && show_legend) {
    fp_args$legend <- input$party_levels
  }

  if (is.null(zero)) {
    zero <- NA_real_
  }
  fp_args$zero <- zero

  p <- do.call(forestplot::forestplot, fp_args) |>
    forestplot::fp_set_style(
      box = "black",
      line = grid::gpar(col = "black", lwd = 2)
    ) |>
    forestplot::fp_set_zebra_style("#EFEFEF") |>
    forestplot::fp_decorate_graph(graph.pos = 2)

  if (!is.null(input$party_levels)) {
    party_levels <- tolower(input$party_levels)
    if (all(party_levels %in% c("democrat", "republican"))) {
      party_colors <- c("democrat" = "#00AEF3", "republican" = "#E81B23")
      p <- p |>
        forestplot::fp_set_style(box = unname(party_colors[party_levels]))
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
        right_bottom_txt = forestplot::fp_txt_gp("", gp = "")
      )
  }

  p
}

.build_forestplot_inputs <- function(
  forest_df,
  label_cols,
  digits = 2,
  show_ci_label = TRUE,
  ci_multiline = TRUE,
  ci_show_party = FALSE
) {
  forest_df <- tibble::as_tibble(forest_df)
  has_party <- "party" %in% names(forest_df) &&
    dplyr::n_distinct(forest_df$party, na.rm = TRUE) > 1

  final_label_cols <- label_cols
  if (show_ci_label && !"ci" %in% final_label_cols) {
    final_label_cols <- c(final_label_cols, "ci")
  }

  if (!has_party) {
    out_df <- forest_df |>
      dplyr::arrange(dplyr::desc(.data$mean))

    if (show_ci_label && !"ci" %in% names(out_df)) {
      out_df <- out_df |>
        dplyr::mutate(
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

    missing_label_cols <- setdiff(final_label_cols, names(out_df))
    if (length(missing_label_cols) > 0) {
      stop("`label_cols` not found in data: ", paste(missing_label_cols, collapse = ", "))
    }

    return(list(
      label_mat = as.matrix(out_df[, final_label_cols, drop = FALSE]),
      mean = out_df$mean,
      lower = out_df$lower,
      upper = out_df$upper,
      party_levels = NULL
    ))
  }

  party_levels <- unique(as.character(stats::na.omit(forest_df$party)))

  core_df <- forest_df |>
    dplyr::group_by(.data$book, .data$party) |>
    dplyr::summarise(
      mean = dplyr::first(.data$mean),
      lower = dplyr::first(.data$lower),
      upper = dplyr::first(.data$upper),
      .groups = "drop"
    )

  mean_wide <- tidyr::pivot_wider(
    dplyr::select(core_df, dplyr::all_of(c("book", "party", "mean"))),
    names_from = "party",
    values_from = "mean"
  )
  lower_wide <- tidyr::pivot_wider(
    dplyr::select(core_df, dplyr::all_of(c("book", "party", "lower"))),
    names_from = "party",
    values_from = "lower"
  )
  upper_wide <- tidyr::pivot_wider(
    dplyr::select(core_df, dplyr::all_of(c("book", "party", "upper"))),
    names_from = "party",
    values_from = "upper"
  )

  mean_mat <- as.matrix(mean_wide[, party_levels, drop = FALSE])
  order_idx <- order(rowMeans(mean_mat, na.rm = TRUE), decreasing = TRUE)
  mean_wide <- mean_wide[order_idx, , drop = FALSE]
  lower_wide <- lower_wide[order_idx, , drop = FALSE]
  upper_wide <- upper_wide[order_idx, , drop = FALSE]

  label_df <- tibble::tibble(book = mean_wide$book)

  if (show_ci_label) {
    ci_source <- if ("ci" %in% names(forest_df)) {
      forest_df |>
        dplyr::group_by(.data$book, .data$party) |>
        dplyr::summarise(ci = dplyr::first(.data$ci), .groups = "drop")
    } else {
      core_df |>
        dplyr::mutate(
          ci = paste0(
            round(.data$mean, digits),
            " [",
            round(.data$lower, digits),
            ", ",
            round(.data$upper, digits),
            "]"
          )
        ) |>
        dplyr::select(dplyr::all_of(c("book", "party", "ci")))
    }

    ci_wide <- tidyr::pivot_wider(
      ci_source,
      names_from = "party",
      values_from = "ci"
    )
    ci_wide <- ci_wide[match(mean_wide$book, ci_wide$book), , drop = FALSE]

    sep <- if (ci_multiline) "\n" else " | "
    ci_compact <- apply(as.matrix(ci_wide[, party_levels, drop = FALSE]), 1, function(x) {
      vals <- as.character(x)
      if (ci_show_party) {
        vals <- paste0(party_levels, ": ", vals)
      }
      paste(vals, collapse = sep)
    })
    label_df$ci <- ci_compact
  }

  extra_label_cols <- setdiff(final_label_cols, c("book", "ci"))
  if (length(extra_label_cols) > 0) {
    extras <- forest_df |>
      dplyr::group_by(.data$book) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(extra_label_cols), dplyr::first), .groups = "drop")
    extras <- extras[match(mean_wide$book, extras$book), , drop = FALSE]
    label_df <- dplyr::left_join(label_df, extras, by = "book")
  }

  missing_label_cols <- setdiff(final_label_cols, names(label_df))
  if (length(missing_label_cols) > 0) {
    stop(
      "`label_cols` not found in data after party consolidation: ",
      paste(missing_label_cols, collapse = ", ")
    )
  }

  list(
    label_mat = as.matrix(label_df[, final_label_cols, drop = FALSE]),
    mean = as.matrix(mean_wide[, party_levels, drop = FALSE]),
    lower = as.matrix(lower_wide[, party_levels, drop = FALSE]),
    upper = as.matrix(upper_wide[, party_levels, drop = FALSE]),
    party_levels = party_levels
  )
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





