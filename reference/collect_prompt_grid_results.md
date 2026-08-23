# Collect prompt-grid checkpoints into one reusable results table

Read every valid prompt-grid checkpoint in one or more directories and
combine it with optional prior results. This is useful after cost-gated
phases because checkpoints for models that are inactive in the current
configuration remain available for later analysis and reuse.

## Usage

``` r
collect_prompt_grid_results(output_dir, existing_results = NULL)
```

## Arguments

- output_dir:

  Character vector of checkpoint directories created by
  [`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md).

- existing_results:

  Optional results data frame, run bundle, RDS path, or CSV path
  accepted by the `existing_results` argument of
  [`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md).

## Value

A tibble of raw prompt-grid results. It can be passed directly to
`run_prompt_grid(existing_results = ...)`.

## Details

Checkpoint rows are identified by the strong `task_hash` stored with
each checkpoint and by `input_row`. Exact duplicate rows are removed.
Conflicting rows with the same task and input identities cause an error
rather than being silently resolved.
