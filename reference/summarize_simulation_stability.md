# Summarize simulation stability across chapters

This helper provides a compact bird's-eye view of where repeated
simulation runs vary across chapters, books, parties, or models. It
reuses the simulation-level SD columns from
[`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md)
and reports how often each metric showed non-zero variation within the
requested grouping.

## Usage

``` r
summarize_simulation_stability(x, by = "party", metrics = NULL, tol = 0)
```

## Arguments

- x:

  A data frame or list-like object. This can be raw simulation metrics
  (for example from
  [`compute_run_ai_metrics()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics.md))
  or chapter summaries from
  [`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md).

- by:

  Character vector of columns used for the compact summary. Defaults to
  `"party"`.

- metrics:

  Optional character vector of metric names to inspect without the `sd_`
  prefix. Defaults to the four core ratings: `pre_ingroup`,
  `pre_outgroup`, `post_ingroup`, and `post_outgroup`.

- tol:

  Numeric tolerance for treating an SD as zero. Defaults to `0`.

## Value

A tibble with one row per grouping combination, including the number of
assessed units (`n_units`), the proportion of units showing any
pre-period variation, the proportion showing any post-period variation,
and an overall `all_stable` flag.

## Details

If `x` is already output from
[`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md),
the function uses it directly. Otherwise, it first computes
chapter-level summaries with `by_party = TRUE`, because party-specific
stability is the most common diagnostic use case.

Groups with only one simulation row have `NA` SD values from
[`stats::sd()`](https://rdrr.io/r/stats/sd.html). Those groups are
treated as not testable for variability and are excluded from the
variation counts.

## Examples

``` r
stability <- summarize_simulation_stability(toy_sim_results)
stability
#> # A tibble: 2 × 5
#>   party      n_units prop_units_any_pre_vari…¹ prop_units_any_post_…² all_stable
#>   <chr>        <int>                     <dbl>                  <dbl> <lgl>     
#> 1 Democrat         8                         1                      1 FALSE     
#> 2 Republican       8                         1                      1 FALSE     
#> # ℹ abbreviated names: ¹​prop_units_any_pre_variation,
#> #   ²​prop_units_any_post_variation

summarize_simulation_stability(
  toy_sim_results,
  by = c("model", "party")
)
#> # A tibble: 2 × 5
#>   party      n_units prop_units_any_pre_vari…¹ prop_units_any_post_…² all_stable
#>   <chr>        <int>                     <dbl>                  <dbl> <lgl>     
#> 1 Democrat         8                         1                      1 FALSE     
#> 2 Republican       8                         1                      1 FALSE     
#> # ℹ abbreviated names: ¹​prop_units_any_pre_variation,
#> #   ²​prop_units_any_post_variation
```
