# Prepare book-level data for forest plotting

Computes standard errors and 95% confidence intervals for book-level
estimates of affective polarization reduction. This function assumes the
input is already aggregated at the book level (e.g., using
`summarize_chapter_scores(..., aggregate_level = "book")`).

## Usage

``` r
prepare_forest_books(summary_books, add_ci_label = TRUE, digits = 2)
```

## Arguments

- summary_books:

  A data frame containing at least the following columns:

- add_ci_label:

  Logical. Default is TRUE. If TRUE, a formatted character column `ci`
  is added containing the estimate and its 95% confidence interval in
  the format: `mean [lower, upper]`. If FALSE, only numeric columns
  (`mean`, `se`, `lower`, `upper`) are returned.

- digits:

  Integer. Default is 2. Number of decimal places used when formatting
  the `ci` column. Ignored if `add_ci_label = FALSE`.

  book

  :   Character string identifying the book.

  sim

  :   Number of simulations.

  mean_diff

  :   Mean reduction score.

  sd_diff

  :   Standard deviation of the reduction score.

## Value

A tibble with added columns:

- mean:

  Mean effect (copied from `mean_diff`).

- se:

  Standard error of the mean.

- lower:

  Lower bound of the 95% CI.

- upper:

  Upper bound of the 95% CI.

## Details

Standard errors are computed as `sd_diff / sqrt(sim)`. Confidence
intervals are calculated using a normal approximation
(`mean +/- 1.96 * SE`).
