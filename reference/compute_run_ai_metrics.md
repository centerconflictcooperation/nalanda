# Compute derived pre/post effect metrics from raw turn-level output

Separates post-processing from model execution so users can re-compute
metrics without re-running API calls.

## Usage

``` r
compute_run_ai_metrics(x, per_group = NULL)
```

## Arguments

- x:

  A data frame or list-like object from
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
  with turn-level rows including `chapter`, `sim`, `identity`,
  `turn_type`, and `rating`. If a list is supplied, the function will
  attempt to combine its data-frame elements with
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  before computing metrics. If cumulative output from
  [`run_ai_cumulative_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_cumulative_chapters.md)
  is detected, this function delegates to
  [`compute_run_ai_metrics_cumulative()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics_cumulative.md).

- per_group:

  Optional logical. Whether the run used per-group mode (`{group}` in
  question template). If `NULL` (default), mode is inferred from
  `target_group`:

  - per-group if any non-missing `target_group` values exist;

  - single-question if `target_group` is entirely missing.

## Value

A simulation-level tibble with derived metrics (for example
`pre_outgroup`, `post_outgroup`, `delta_outgroup`, and in per-group mode
also `pre_ingroup`, `post_ingroup`, `pre_gap`, `post_gap`,
`delta_ingroup`, `delta_gap`).

## Examples

``` r
metrics <- compute_run_ai_metrics(toy_run_ai_turns)
head(metrics)
#> # A tibble: 6 × 16
#>   chapter     sim identity   book    party pre_ingroup post_ingroup pre_outgroup
#>   <chr>     <int> <chr>      <chr>   <chr>       <dbl>        <dbl>        <dbl>
#> 1 chapter_1     1 Democrat   Bridge… Demo…          78           79           46
#> 2 chapter_1     1 Republican Bridge… Repu…          82           84           51
#> 3 chapter_1     2 Democrat   Bridge… Demo…          79           80           47
#> 4 chapter_1     2 Republican Bridge… Repu…          83           85           52
#> 5 chapter_2     1 Democrat   Bridge… Demo…          78           79           48
#> 6 chapter_2     1 Republican Bridge… Repu…          82           84           53
#> # ℹ 8 more variables: post_outgroup <dbl>, pre_gap <dbl>, post_gap <dbl>,
#> #   delta_ingroup <dbl>, delta_outgroup <dbl>, delta_gap <dbl>,
#> #   baseline_prompt <chr>, post_prompt <chr>

# The processed output can be passed on to summary and plotting helpers.
summary_by_chapter <- summarize_chapter_scores(metrics)
head(summary_by_chapter)
#> # A tibble: 6 × 23
#>   book           chapter   sim mean_pre_ingroup sd_pre_ingroup mean_post_ingroup
#>   <chr>          <chr>   <int>            <dbl>          <dbl>             <dbl>
#> 1 Bridge Stories chapte…     4             80.5           2.38                82
#> 2 Bridge Stories chapte…     4             80.5           2.38                82
#> 3 Bridge Stories chapte…     4             80.5           2.38                82
#> 4 Bridge Stories chapte…     4             80.5           2.38                82
#> 5 Common Ground  chapte…     4             79.5           2.38                81
#> 6 Common Ground  chapte…     4             79.5           2.38                81
#> # ℹ 17 more variables: sd_post_ingroup <dbl>, mean_pre_outgroup <dbl>,
#> #   sd_pre_outgroup <dbl>, mean_post_outgroup <dbl>, sd_post_outgroup <dbl>,
#> #   mean_pre_gap <dbl>, sd_pre_gap <dbl>, mean_post_gap <dbl>,
#> #   sd_post_gap <dbl>, mean_delta_outgroup <dbl>, sd_delta_outgroup <dbl>,
#> #   mean_delta_ingroup <dbl>, sd_delta_ingroup <dbl>, mean_delta_gap <dbl>,
#> #   sd_delta_gap <dbl>, chapter_excerpt <chr>, chapter_index <int>
```
