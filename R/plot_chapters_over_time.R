#' Plot chapters over time (multi-timepoint means)
#'
#' Create a plot showing means over chapter timepoints using rempsyc::plot_means_over_time
#' for the wide-format response variables.
#'
#' @param chapters A data frame or list of simulation rows containing columns `book`, `chapter`, and the desired `dv`.
#' @param dv Character. Name of the column to plot as the dependent variable (default: "score").
#' @param xtitle Character. X-axis label.
#' @param ytitle Character. Y-axis label.
#' @param plot_title Logical. Whether to include a title.
#' @param ci_type Character. Type of confidence interval to pass to `rempsyc::plot_means_over_time`.
#' @param legend.position Position for legend.
#' @param text_size Numeric. Base text size for axis/title text.
#' @param reverse_score Logical. Whether to reverse score scale using rempsyc::nice_reverse.
#' @param error_bars Logical. Show error bars.
#' @param neutrality_line Logical. Add a horizontal neutrality line at 50.
#' @return A ggplot2 object.
#' @export
plot_chapters_over_time <- function(
  chapters,
  dv = "score",
  xtitle = "Chapter",
  ytitle = "Simulated scores",
  plot_title = TRUE,
  ci_type = "between",
  legend.position = "bottom",
  text_size = 20,
  reverse_score = FALSE,
  error_bars = TRUE,
  neutrality_line = TRUE
) {
  bind_simulation_results <- function(chapters) {
    if (is.list(chapters) && !inherits(chapters, "data.frame")) {
      dplyr::bind_rows(chapters)
    } else {
      chapters
    }
  }
  df <- bind_simulation_results(chapters)
  dv_column <- dv
  if (!dv_column %in% names(df)) {
    candidate <- intersect(c("score", "mean_score", "mean_baseline"), names(df))
    if (length(candidate) == 0) {
      stop("Column '", dv, "' not found and no fallback mean score columns detected.")
    }
    dv_column <- candidate[1]
  }
  df <- df |>
    dplyr::mutate(score = .data[[dv_column]])
  if (reverse_score) {
    df <- df |>
      dplyr::mutate(score = rempsyc::nice_reverse(score, 100, 1))
  }
  df <- df |>
    dplyr::arrange(book, chapter, sim) |>
    dplyr::group_by(book) |>
    dplyr::mutate(chapter_index = dplyr::dense_rank(chapter)) |>
    dplyr::ungroup()
  df_wide <- df |>
    dplyr::mutate(time_var = paste0("T", chapter_index)) |>
    dplyr::select(book, sim, time_var, score) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = time_var, values_from = score)
  response_cols <- grep("^T[0-9]+$", names(df_wide), value = TRUE)
  p <- rempsyc::plot_means_over_time(
    data = df_wide,
    response = response_cols,
    group = "book",
    ytitle = ytitle,
    ci_type = ci_type,
    legend.position = legend.position,
    error_bars = error_bars
  ) +
    ggplot2::labs(x = xtitle) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(size = text_size)
    )
  if (!is.null(title)) {
    p <- p +
      ggplot2::labs(
        title = paste0(
          "Results of ",
          df_wide$sim[1],
          " simulations",
          " (model = '",
          attr(chapters, "model"),
          "')"
        )
      )
  }
  if (isTRUE(neutrality_line)) {
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
  p
}
