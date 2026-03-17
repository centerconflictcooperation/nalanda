# Faceted plot of chapter scores

Create a faceted plot (one facet per book) showing mean scores and error
bars.

## Usage

``` r
plot_chapter_scores_faceted(
  summary_df,
  dv = "post_outgroup",
  y_label = "Simulated scores"
)
```

## Arguments

- summary_df:

  Data frame produced by
  [`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md).

- dv:

  Character. Column name prefix for mean and sd. For example,
  `"post_outgroup"` will plot `mean_post_outgroup` ± `sd_post_outgroup`.

- y_label:

  Character string for y-axis label.

## Value

A ggplot2 object.

## Examples

``` r
chapter_summary <- summarize_chapter_scores(toy_sim_results)
plot_chapter_scores_faceted(
  chapter_summary,
  dv = "delta_outgroup",
  y_label = "Mean outgroup change"
)
```
