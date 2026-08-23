# Tidy hierarchical forecast aggregation results

Converts the stage tables returned by
[`aggregate_model_forecasts()`](https://centerconflictcooperation.github.io/nalanda/reference/aggregate_model_forecasts.md)
into a single long table. The result is useful for diagnostics and
plotting, and retains the identifiers that apply at each aggregation
level.

## Usage

``` r
tidy_forecast_aggregation(
  aggregation,
  unit_by,
  outcomes = NULL,
  completion_col = "completion",
  prompt_col = "prompt_id",
  model_col = "model_id",
  family_col = "family"
)
```

## Arguments

- aggregation:

  A named list returned by
  [`aggregate_model_forecasts()`](https://centerconflictcooperation.github.io/nalanda/reference/aggregate_model_forecasts.md).

- unit_by:

  Character vector of columns identifying a forecast target.

- outcomes:

  Optional character vector of numeric outcome columns. When omitted,
  numeric columns other than count and identifier columns are used.

- completion_col, prompt_col, model_col, family_col:

  Names of the identifier columns used when the aggregation was made.
  `family_col` may be `NULL`.

## Value

A tibble with `aggregation_level`, the available identifiers, `outcome`,
`estimate`, `contributor_type`, and `n_contributors`.
