# Plot chapter trajectories by book

Simple line plot of mean simulated outgroup rating across chapter order
for each book.

## Usage

``` r
plot_chapter_trajectories(
  summary_df,
  dv = "mean_post_outgroup",
  y_label = "Simulated scores"
)
```

## Arguments

- summary_df:

  A data frame produced by
  [`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md)
  with columns `chapter_index`, `mean_post_outgroup`, and `book`.

- dv:

  Character. Column name to plot on the y-axis. Defaults to
  `"mean_post_outgroup"`.

- y_label:

  Character. Y-axis label.

## Value

A ggplot2 object.
