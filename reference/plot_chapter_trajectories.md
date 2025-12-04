# Plot chapter trajectories by book

Simple line plot of mean simulated score across chapter order for each
book.

## Usage

``` r
plot_chapter_trajectories(summary_df)
```

## Arguments

- summary_df:

  A data frame produced by
  [`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md)
  with columns `chapter_index`, `mean_score`, and `book`.

## Value

A ggplot2 object.
