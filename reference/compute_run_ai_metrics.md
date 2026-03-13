# Compute derived pre/post effect metrics from chapter simulation output

Separates post-processing from model execution so users can re-compute
metrics without re-running API calls.

## Usage

``` r
compute_run_ai_metrics(x, per_group = NULL)
```

## Arguments

- x:

  Tibble produced by the simulation runner with at least `pre_outgroup`
  and `post_outgroup` columns; in per-group mode also `pre_ingroup` and
  `post_ingroup`.

- per_group:

  Optional logical. Whether the run used per-group mode (`{group}` in
  question template). If `NULL` (default), mode is inferred from column
  names:

  - per-group if any `pre_rating_{group}` / `post_rating_{group}`
    columns exist;

  - single-question if `pre_rating` and `post_rating` exist.

## Value

Input tibble with derived metric columns appended.
