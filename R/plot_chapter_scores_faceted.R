#' Faceted plot of chapter scores
#'
#' Create a faceted plot (one facet per book) showing mean scores and error bars.
#'
#' @param summary_df Data frame produced by `summarize_chapter_scores()`.
#' @param dv Character. Column name prefix for mean and sd. For example,
#'   `"post_outgroup"` will plot `mean_post_outgroup` ± `sd_post_outgroup`.
#' @param ytitle Character string for y-axis label.
#' @return A ggplot2 object.
#' @export
plot_chapter_scores_faceted <- function(
  summary_df,
  dv = "post_outgroup",
  ytitle = "Simulated scores"
) {
  mean_col <- paste0("mean_", dv)
  sd_col <- paste0("sd_", dv)

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = .data$chapter_index, y = .data[[mean_col]])
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data[[mean_col]] - .data[[sd_col]],
        ymax = .data[[mean_col]] + .data[[sd_col]]
      ),
      width = 0.15
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$book), scales = "free_x") +
    ggplot2::labs(
      x = "Chapter (order in book)",
      y = ytitle
    ) +
    ggplot2::theme_classic()
}

