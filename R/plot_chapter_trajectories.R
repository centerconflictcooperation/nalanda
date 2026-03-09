#' Plot chapter trajectories by book
#'
#' Simple line plot of mean simulated outgroup rating across chapter order
#' for each book.
#'
#' @param summary_df A data frame produced by `summarize_chapter_scores()` with
#'   columns `chapter_index`, `mean_post_outgroup`, and `book`.
#' @param dv Character. Column name to plot on the y-axis. Defaults to
#'   `"mean_post_outgroup"`.
#' @param ytitle Character. Y-axis label.
#' @return A ggplot2 object.
#' @export
plot_chapter_trajectories <- function(
  summary_df,
  dv = "mean_post_outgroup",
  ytitle = "Simulated scores"
) {
  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = .data$chapter_index, y = .data[[dv]], group = .data$book)
  ) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$book)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$book)) +
    ggplot2::labs(
      x = "Chapter (order in book)",
      y = ytitle,
      linetype = "Book",
      shape = "Book"
    ) +
    ggplot2::theme_classic()
}

