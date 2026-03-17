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

## Examples

``` r
one_turn_like <- toy_run_ai_turns |>
  dplyr::filter(turn_type == "post") |>
  dplyr::mutate(
    turn_type = "single",
    prompt = post_prompt
  ) |>
  dplyr::select(-baseline_prompt, -post_prompt)

plot_chapters_over_time_one_turn(
  one_turn_like,
  dv = "gap",
  group = "party",
  facet = "book"
)
#> Scale for shape is already present.
#> Adding another scale for shape, which will replace the existing scale.
```
