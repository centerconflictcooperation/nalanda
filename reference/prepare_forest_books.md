# Prepare book-level data for forest plotting

Computes standard errors and 95% confidence intervals for book-level
estimates. This function assumes the input is already aggregated at the
book level (e.g., using
`summarize_chapter_scores(..., aggregate_level = "book")`).

## Usage

``` r
prepare_forest_books(
  summary_books,
  dv = "delta_gap",
  add_ci_label = TRUE,
  digits = 2
)
```

## Arguments

- summary_books:

  A data frame containing at least `book`, `sim`, and mean/sd columns
  matching the `dv` prefix pattern.

- dv:

  Character. The variable prefix to use for the forest plot. Defaults to
  `"delta_gap"`. The function looks for `mean_{dv}` and `sd_{dv}`
  columns.

- add_ci_label:

  Logical. Default is TRUE. If TRUE, a formatted character column `ci`
  is added containing the estimate and its 95% confidence interval in
  the format: `mean [lower, upper]`. If FALSE, only numeric columns
  (`mean`, `se`, `lower`, `upper`) are returned.

- digits:

  Integer. Default is 2. Number of decimal places used when formatting
  the `ci` column. Ignored if `add_ci_label = FALSE`.

## Value

A tibble with added columns:

- mean:

  Mean effect.

- se:

  Standard error of the mean.

- lower:

  Lower bound of the 95% CI.

- upper:

  Upper bound of the 95% CI.

## Details

Standard errors are computed as `sd / sqrt(sim)`. Confidence intervals
are calculated using a normal approximation (`mean +/- 1.96 * SE`).

## Examples

``` r
book_summary <- summarize_chapter_scores(
  toy_sim_results,
  aggregate_level = "book"
)
prepare_forest_books(book_summary, dv = "delta_gap")
#> # A tibble: 2 × 25
#>   book     sim mean_pre_ingroup sd_pre_ingroup mean_post_ingroup sd_post_ingroup
#>   <chr>  <int>            <dbl>          <dbl>             <dbl>           <dbl>
#> 1 Bridg…    16             80.5           2.13                82            2.19
#> 2 Commo…    16             79.5           2.13                81            2.19
#> # ℹ 19 more variables: mean_pre_outgroup <dbl>, sd_pre_outgroup <dbl>,
#> #   mean_post_outgroup <dbl>, sd_post_outgroup <dbl>, mean_pre_gap <dbl>,
#> #   sd_pre_gap <dbl>, mean_post_gap <dbl>, sd_post_gap <dbl>,
#> #   mean_delta_outgroup <dbl>, sd_delta_outgroup <dbl>,
#> #   mean_delta_ingroup <dbl>, sd_delta_ingroup <dbl>, mean_delta_gap <dbl>,
#> #   sd_delta_gap <dbl>, mean <dbl>, se <dbl>, lower <dbl>, upper <dbl>,
#> #   ci <chr>
```
