# Compute cumulative chapter metrics against the original baseline

Compute cumulative chapter metrics against the original baseline

## Usage

``` r
compute_run_ai_metrics_cumulative(x, per_group = NULL)
```

## Arguments

- x:

  A data frame or list-like object from
  [`run_ai_cumulative_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_cumulative_chapters.md)
  with turn-level rows including `book`, `chapter`, `chapter_index`,
  `sim`, `identity`, `turn_type`, and `rating`.

- per_group:

  Optional logical. Whether the run used per-group mode. If `NULL`, mode
  is inferred from `target_group`.

## Value

A chapter-level tibble comparing each post-chapter state against the
baseline turn from the same `book × sim × identity` conversation.
