#' Plot chapter trajectories by book
#'
#' Simple line plot of mean simulated score across chapter order for each book.
#'
#' @param summary_df A data frame produced by `summarize_chapter_scores()` with
#'   columns `chapter_index`, `mean_score`, and `book`.
#' @return A ggplot2 object.
#' @export
plot_chapter_trajectories <- function(summary_df) {
  ggplot2::ggplot(summary_df, ggplot2::aes(x = chapter_index, y = mean_score, group = book)) +
    ggplot2::geom_line(ggplot2::aes(linetype = book)) +
    ggplot2::geom_point(ggplot2::aes(shape = book)) +
    ggplot2::labs(
      x = "Chapter (order in book)",
      y = "Simulated polarization-reduction score (0-100)",
      linetype = "Book",
      shape = "Book"
    ) +
    ggplot2::theme_classic()
}
