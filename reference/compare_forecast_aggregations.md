# Compare estimates from two forecast aggregations

Aligns a selected stage from two compatible outputs of
[`aggregate_model_forecasts()`](https://centerconflictcooperation.github.io/nalanda/reference/aggregate_model_forecasts.md)
and reports signed and absolute differences. Unlike a permissive join,
this helper errors when either result has missing or duplicate
unit–outcome identities.

## Usage

``` r
compare_forecast_aggregations(
  first,
  second,
  stage = "consensus",
  unit_by,
  outcomes,
  labels = c("first", "second"),
  scale_width = NULL
)
```

## Arguments

- first, second:

  Named aggregation lists to compare.

- stage:

  Aggregation stage: one of `"prompt"`, `"model"`, `"family"`, or
  `"consensus"`.

- unit_by:

  Character vector identifying a forecast target.

- outcomes:

  Character vector of outcome columns.

- labels:

  Two names for the estimate columns in the result.

- scale_width:

  Optional scale-width specification: a character column present in both
  selected stage tables, or a data frame with `unit_by`, optional
  `outcome`, and a numeric `scale_width` column.

## Value

A tibble with both estimates, `difference` (`second - first`),
`absolute_difference`, and optional scale-normalized differences.
