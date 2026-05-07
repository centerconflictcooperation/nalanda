# Summarize model agreement across analysis levels

Builds a slide-ready sensitivity table by running
[`model_agreement()`](https://centerconflictcooperation.github.io/nalanda/reference/model_agreement.md)
across several substantively useful unit definitions (for example
book-level, chapter-level, and party-specific agreement). Lower-level
rows are aggregated with an NA-safe mean before each agreement
calculation, so skipped chapters do not turn an entire book mean into
`NA`.

## Usage

``` r
model_agreement_sensitivity(
  data,
  outcome = "mean_outcome",
  model_col = "model",
  analyses = NULL,
  metrics = c("icc", "kendall_w"),
  format = c("wide", "long"),
  digits = 2,
  drop_missing = TRUE
)
```

## Arguments

- data:

  A data frame with one row per model-by-unit combination.

- outcome:

  Character string naming the score column (default `"mean_outcome"`).

- model_col:

  Character string naming the model column (default `"model"`).

- analyses:

  Optional named list defining analyses to run. Each element should be a
  list with `unit_by` and, optionally, `group_by`. If `NULL`, a default
  set is inferred from available book, chapter, and party/group columns.

- metrics:

  Character vector of metrics to compute. One or both of `"icc"` and
  `"kendall_w"` (default both).

- format:

  Character. `"wide"` returns one row per analysis/subgroup with ICC and
  Kendall's W side by side; `"long"` returns the stacked
  [`model_agreement()`](https://centerconflictcooperation.github.io/nalanda/reference/model_agreement.md)
  results with analysis labels.

- digits:

  Integer. Number of decimal places used in the formatted wide table.

- drop_missing:

  Logical. Whether to drop rows with missing model, unit, or grouping
  identifiers before computing each analysis (default `TRUE`).

## Value

A tibble. With `format = "wide"`, columns include `Analysis level`,
`Subgroup`, `N models`, `N units`, `ICC`, and `Kendall's W`.

## Examples

``` r
if (FALSE) { # \dontrun{
model_agreement_sensitivity(
  agg,
  outcome = "mean_delta_gap",
  model_col = "model"
)
} # }
```
