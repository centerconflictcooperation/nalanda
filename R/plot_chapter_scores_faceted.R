#' Faceted plot of chapter scores
#'
#' Create a faceted plot (one facet per book) showing mean scores and error bars.
#'
#' @param summary_df Data frame produced by `summarize_chapter_scores()`.
#' @param ytitle Character string for y-axis label.
#' @return A ggplot2 object.
#' @export
plot_chapter_scores_faceted <- function(
  summary_df,
  ytitle = "Simulated scores"
) {
  ggplot2::ggplot(summary_df, ggplot2::aes(x = chapter_index, y = mean_score)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_score - sd_score, ymax = mean_score + sd_score),
      width = 0.15
    ) +
    ggplot2::facet_wrap(~book, scales = "free_x") +
    ggplot2::labs(
      x = "Chapter (order in book)",
      y = ytitle
    ) +
    ggplot2::theme_classic()
}
