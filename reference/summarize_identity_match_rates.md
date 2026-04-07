# Summarize identity match rates by model

This helper converts raw identity-adoption output into one row per
grouping combination, reporting how often the model's reported identity
matched the requested one.

## Usage

``` r
summarize_identity_match_rates(
  x,
  by = c("model", "identity"),
  compact = FALSE,
  expected_col = "identity",
  observed_col = "party"
)
```

## Arguments

- x:

  A data frame or list-like object from a simulation workflow containing
  `identity` and `party`.

- by:

  Character vector of columns to group by. Defaults to
  `c("model", "identity")`.

- compact:

  Logical. If `TRUE`, return a wide one-row-per-group summary with one
  rate column per requested identity plus a shared `n` column.

- expected_col:

  Character scalar naming the requested identity column.

- observed_col:

  Character scalar naming the model-reported identity column.

## Value

A tibble with counts and match rates, including `n_requested`,
`n_match`, `adoption_rate`, `n_mismatch`, and `mismatch_rate`.
