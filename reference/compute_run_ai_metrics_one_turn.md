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
