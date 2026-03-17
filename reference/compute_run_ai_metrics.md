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
  before computing metrics.

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
