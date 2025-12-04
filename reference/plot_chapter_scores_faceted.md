# Faceted plot of chapter scores

Create a faceted plot (one facet per book) showing mean scores and error
bars.

## Usage

``` r
plot_chapter_scores_faceted(summary_df, ytitle = "Simulated scores")
```

## Arguments

- summary_df:

  Data frame produced by
  [`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md).

- ytitle:

  Character string for y-axis label.

## Value

A ggplot2 object.
