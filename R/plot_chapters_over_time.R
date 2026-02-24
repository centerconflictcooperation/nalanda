#' Plot chapters over time (multi-timepoint means)
#'
#' Create a plot showing means over chapter timepoints using rempsyc::plot_means_over_time
#' for the wide-format response variables.
#'
#' @param chapters A data frame or list of simulation rows containing columns `book`, `chapter`, and the desired `dv`.
#' @param dv Character. Name of the column to plot as the dependent variable (default: "score").
#' @param group The group by which to plot the variable
#' @param xtitle Character. X-axis label.
#' @param ytitle Character. Y-axis label.
#' @param plot_title Logical. Whether to include a title.
#' @param plot_subtitle Optional plot subtitle.
#' @param ci_type Character. Type of confidence interval to pass to `rempsyc::plot_means_over_time`.
#' @param legend.position Position for legend.
#' @param groups.order Specifies the desired display order of the groups
#' on the legend. Either provide the levels directly, or a string: "increasing"
#' or "decreasing", to order based on the average value of the variable on the
#' y axis, or "string.length", to order from the shortest to the longest
#' string (useful when working with long string names). "Defaults to "decreasing".
#' @param text_size Numeric. Base text size for axis/title text.
#' @param line_width Numeric. Line thickness used in `geom_line()`.
#'   Defaults to 3. Can be reduced for publication figures or increased
#'   for presentation slides.
#' @param point_size Numeric. Point size used in `geom_point()`.
#'   Defaults to 4. Adjust to improve readability depending on output format.
#' @param reverse_score Logical. Whether to reverse score scale using rempsyc::nice_reverse.
#' @param error_bars Logical. Show error bars.
#' @param neutrality_line Logical. Add a horizontal neutrality line at 50.
#' @param facet The variable by which to facet grid.
#' @param facets.order Specifies the desired display order of facet panels.
#'   Either provide the levels directly, or a string: "increasing" or
#'   "decreasing", to order panels based on the average value of the
#'   y variable, or "string.length" to order panels by facet label length.
#'   Defaults to "increasing".
#' @return A ggplot2 object.
#' @export
plot_chapters_over_time <- function(
  chapters,
  dv = "mean_diff",
  group = "book",
  xtitle = "Chapter",
  ytitle = "Simulated scores",
  plot_title = TRUE,
  plot_subtitle = "",
  ci_type = "between",
  legend.position = "bottom",
  groups.order = "decreasing",
  text_size = 20,
  line_width = 3,
  point_size = 4,
  reverse_score = FALSE,
  error_bars = TRUE,
  neutrality_line = TRUE,
  facet = NULL,
  facets.order = "increasing"
) {
  df <- bind_simulation_results(chapters)
  dv_column <- dv
  if (!dv_column %in% names(df)) {
    candidate <- intersect(c("score", "mean_score", "mean_baseline"), names(df))
    if (length(candidate) == 0) {
      stop(
        "Column '",
        dv,
        "' not found and no fallback mean score columns detected.\n",
        "Available columns: ",
        paste(names(df), collapse = ", ")
      )
    }
    dv_column <- candidate[1]
  }
  df <- df |>
    dplyr::mutate(score = .data[[dv_column]])
  if (reverse_score) {
    df <- df |>
      dplyr::mutate(difference_score = difference_score * -1)
    # df <- df |>
    #   dplyr::mutate(score = rempsyc::nice_reverse(difference_score, 100, 1))
  }
  df <- df |>
    dplyr::mutate(
      chapter_num = suppressWarnings(as.integer(stringr::str_extract(
        chapter,
        "\\d+"
      )))
    ) |>
    dplyr::arrange(book, chapter_num, sim) |>
    dplyr::group_by(book) |>
    dplyr::mutate(chapter_index = dplyr::dense_rank(chapter_num)) |>
    dplyr::ungroup()

  # Create a unique simulation ID to handle cases with multiple contexts/parties per sim
  # This ensures pivot_wider has a unique key for each row
  grouping_cols <- intersect(c("book", "sim", "context", "party"), names(df))
  if (length(grouping_cols) > 0) {
    df <- df |>
      dplyr::group_by(dplyr::pick(dplyr::all_of(grouping_cols))) |>
      dplyr::mutate(sim_unique_id = dplyr::cur_group_id()) |>
      dplyr::ungroup()
  } else {
    df$sim_unique_id <- df$sim
  }

  df_wide <- df |>
    dplyr::mutate(time_var = paste0("T", chapter_index)) |>
    dplyr::select(
      book,
      sim_unique_id,
      time_var,
      difference_score,
      dplyr::any_of("party")
    ) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = time_var, values_from = difference_score)

  response_cols <- grep("^T[0-9]+$", names(df_wide), value = TRUE)

  # Auto-detect grouping variable
  group <- group

  # Update x-axis label if grouping by party
  if (group == "party" && is.null(facet)) {
    book_name <- unique(df_wide$book)[1]
    xtitle <- paste0(xtitle, " (", book_name, ")")
  }

  p <- rempsyc::plot_means_over_time(
    data = df_wide,
    response = response_cols,
    group = group,
    ytitle = ytitle,
    ci_type = ci_type,
    legend.position = legend.position,
    error_bars = error_bars,
    line_width = line_width,
    point_size = point_size,
    groups.order = groups.order,
    facet = facet,
    facets.order = facets.order
  ) +
    ggplot2::labs(x = xtitle) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(size = text_size)
    )
  if (isTRUE(plot_title)) {
    p <- p +
      ggplot2::labs(
        title = paste0(
          "Results of ",
          max(chapters[[1]]$sim) * 2,
          " simulations per book per chapter",
          " (model = '",
          attr(chapters[[1]], "model"),
          "')"
        )
      )
  }
  if (
    isTRUE(neutrality_line) && !grepl("difference|diff", dv, ignore.case = TRUE)
  ) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 50,
        linetype = "dashed",
        linewidth = 0.6,
        color = "grey40"
      ) +
      ggplot2::annotate(
        "text",
        x = 1,
        y = 52,
        label = "Neutral (50)",
        color = "grey30",
        hjust = 0,
        size = 3
      )
  }

  # Add line at 0 for difference scores
  if (
    isTRUE(neutrality_line) && grepl("difference|diff", dv, ignore.case = TRUE)
  ) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.6,
        color = "grey40"
      ) +
      ggplot2::annotate(
        "text",
        x = 1,
        y = 2, # Slightly above 0
        label = "No Change (0)",
        color = "grey30",
        hjust = 0,
        size = 3
      )
  }

  if (!missing(plot_subtitle)) {
    p <- p +
      ggplot2::labs(subtitle = plot_subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 16))
  }

  if (group %in% names(df_wide)) {
    group_levels <- levels(as.factor(df_wide[[group]]))
    if (all(group_levels %in% c("Democrat", "Republican"))) {
      # dem_blue <- "#2E74C0" #2E6FBA #0015BC
      # rep_red <- "#CB454A" #C63D3D #E81B23
      # https://www.flagcolorcodes.com/republican-party-elephant
      p <- p +
        ggplot2::scale_colour_manual(
          values = c("Democrat" = "#00AEF3", "Republican" = "#E81B23")
        ) +
        ggplot2::scale_shape_manual(
          values = c("Democrat" = 21, "Republican" = 24)
        ) +
        ggplot2::theme(
          panel.spacing = ggplot2::unit(1.2, "lines"),
          axis.line = ggplot2::element_line(linewidth = 0.6),
          axis.ticks = ggplot2::element_line(linewidth = 0.6),
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(face = "plain")
        ) +
        ggplot2::labs(colour = NULL, shape = NULL, linetype = NULL)
    }
  }
  # Optional faceting
  if (!is.null(facet) && facet %in% names(df)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  }

  p
}

bind_simulation_results <- function(chapters) {
  if (is.list(chapters) && !inherits(chapters, "data.frame")) {
    # Unclass to ensure bind_rows treats it as a plain list of data frames
    # and not a custom object that might be misinterpreted
    dplyr::bind_rows(unclass(chapters))
  } else {
    chapters
  }
}
