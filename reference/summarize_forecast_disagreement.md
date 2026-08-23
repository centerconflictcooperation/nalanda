# Summarize forecast disagreement within units

Computes distribution summaries across any contributor type, such as
models or prompt variants. Missing estimates are counted explicitly in
`n_nonmissing` and `n_missing`; with `na_rm = FALSE`, any missing
estimate makes the numeric summaries for that unit missing.

## Usage

``` r
summarize_forecast_disagreement(
  data,
  unit_by,
  estimate_col = "estimate",
  contributor_col,
  na_rm = TRUE,
  scale_width_col = NULL
)
```

## Arguments

- data:

  A data frame containing contributor-level estimates.

- unit_by:

  Character vector identifying the units to summarize within.

- estimate_col:

  Numeric estimate column.

- contributor_col:

  Contributor or source identifier column.

- na_rm:

  Whether to omit missing estimates from numeric summaries.

- scale_width_col:

  Optional positive numeric column giving the width of the outcome scale
  within each unit. If supplied, normalized SD, MAD, and range columns
  are returned.

## Value

A tibble with contributor counts, missingness counts, and mean, median,
SD, MAD, minimum, maximum, and range.
