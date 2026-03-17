# Plot chapter trajectories for one-turn simulations

Plot chapter trajectories for one-turn simulations

## Usage

``` r
plot_chapters_over_time_one_turn(chapters, dv = "outgroup_rating", ...)
```

## Arguments

- chapters:

  Raw output from
  [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md)
  or processed output from
  [`compute_run_ai_metrics_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics_one_turn.md).

- dv:

  Character. Name of the metric to plot. Defaults to
  `"outgroup_rating"`.

- ...:

  Additional arguments passed to
  [`plot_chapters_over_time()`](https://centerconflictcooperation.github.io/nalanda/reference/plot_chapters_over_time.md).

## Value

A ggplot2 object.
