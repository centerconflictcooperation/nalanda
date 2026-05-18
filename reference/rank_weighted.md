# Rank rows using a weighted rubric

Compute a weighted score across selected numeric columns and return the
input data with a final `weighted_score`, sorted by score.

## Usage

``` r
rank_weighted(
  data,
  weights,
  normalize = TRUE,
  decreasing = TRUE,
  na_rm = FALSE
)
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

- na_rm:

  Logical. If `TRUE`, missing values in weighted columns are ignored and
  remaining weights are rescaled within each row. Rows where all
  weighted columns are missing receive `NA_real_`. Defaults to `FALSE`,
  which preserves missing values in `weighted_score`.

## Value

A tibble containing all original columns plus `weighted_score`, sorted
by score.

## Examples

``` r
book_summary <- summarize_chapter_scores(
  toy_sim_results,
  aggregate_level = "book"
)
rank_weighted(
  book_summary,
  weights = c(mean_delta_outgroup = 0.6, mean_delta_gap = 0.4)
)
#> # A tibble: 2 × 21
#>   book     sim mean_pre_ingroup sd_pre_ingroup mean_post_ingroup sd_post_ingroup
#>   <chr>  <int>            <dbl>          <dbl>             <dbl>           <dbl>
#> 1 Bridg…    16             80.5           2.13                82            2.19
#> 2 Commo…    16             79.5           2.13                81            2.19
#> # ℹ 15 more variables: mean_pre_outgroup <dbl>, sd_pre_outgroup <dbl>,
#> #   mean_post_outgroup <dbl>, sd_post_outgroup <dbl>, mean_pre_gap <dbl>,
#> #   sd_pre_gap <dbl>, mean_post_gap <dbl>, sd_post_gap <dbl>,
#> #   mean_delta_outgroup <dbl>, sd_delta_outgroup <dbl>,
#> #   mean_delta_ingroup <dbl>, sd_delta_ingroup <dbl>, mean_delta_gap <dbl>,
#> #   sd_delta_gap <dbl>, weighted_score <dbl>
```
