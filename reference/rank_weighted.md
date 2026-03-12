# Rank rows using a weighted rubric

Compute a weighted score across selected numeric columns and return the
input data with a final `weighted_score`, sorted by score.

## Usage

``` r
rank_weighted(data, weights, normalize = TRUE, decreasing = TRUE)
```

## Arguments

- data:

  A data frame.

- weights:

  Named numeric vector of weights. Names must match columns in `data`,
  and weights must sum to 1.

- normalize:

  Logical. If `TRUE` (default), selected variables are scaled to
  `[0, 1]` using min-max normalization before weighting.

- decreasing:

  Logical. If `TRUE` (default), rows are sorted from highest to lowest
  `weighted_score`.

## Value

A tibble containing all original columns plus `weighted_score`, sorted
by score.
