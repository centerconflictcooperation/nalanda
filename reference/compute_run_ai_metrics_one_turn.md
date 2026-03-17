# Compute one-turn ingroup/outgroup metrics from raw output

Compute one-turn ingroup/outgroup metrics from raw output

## Usage

``` r
compute_run_ai_metrics_one_turn(x, per_group = NULL)
```

## Arguments

- x:

  A data frame or list-like object from
  [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md)
  with single-turn rows including `chapter`, `sim`, `identity`, and
  `rating`.

- per_group:

  Optional logical. Whether the run used per-group mode. If `NULL`
  (default), mode is inferred from `target_group`.

## Value

A simulation-level tibble with one-turn metrics. In per-group mode this
includes `ingroup_rating`, `outgroup_rating`, and `gap`. In
single-question mode it includes `overall_rating` and `outgroup_rating`.

## Examples

``` r
one_turn_like <- toy_run_ai_turns |>
  dplyr::filter(turn_type == "post") |>
  dplyr::mutate(
    turn_type = "single",
    prompt = post_prompt
  ) |>
  dplyr::select(-baseline_prompt, -post_prompt)

compute_run_ai_metrics_one_turn(one_turn_like)
#> # A tibble: 32 × 9
#>    chapter     sim identity   book    party ingroup_rating outgroup_rating   gap
#>    <chr>     <int> <chr>      <chr>   <chr>          <dbl>           <dbl> <dbl>
#>  1 chapter_1     1 Democrat   Bridge… Demo…             79            50    29  
#>  2 chapter_1     1 Democrat   Common… Demo…             78            55    23  
#>  3 chapter_1     1 Republican Bridge… Repu…             84            64    20  
#>  4 chapter_1     1 Republican Common… Repu…             83            55    28  
#>  5 chapter_1     2 Democrat   Bridge… Demo…             80            51.5  28.5
#>  6 chapter_1     2 Democrat   Common… Demo…             79            56.5  22.5
#>  7 chapter_1     2 Republican Bridge… Repu…             85            64.5  20.5
#>  8 chapter_1     2 Republican Common… Repu…             84            55.5  28.5
#>  9 chapter_2     1 Democrat   Bridge… Demo…             79            55    24  
#> 10 chapter_2     1 Democrat   Common… Demo…             79            60    19  
#> # ℹ 22 more rows
#> # ℹ 1 more variable: prompt <chr>
```
