# Compute inter-model agreement

Quantifies how consistently different AI models score the same units
using ICC(2,1) (intraclass correlation, absolute agreement) and/or
Kendall's W (coefficient of concordance). Models produce continuous
scores on a 1–100 scale; this function operates on those raw scores
(typically after aggregating simulation runs via
[`aggregate_simulations()`](https://centerconflictcooperation.github.io/nalanda/reference/aggregate_simulations.md)).

## Usage

``` r
model_agreement(
  data,
  outcome = "mean_outcome",
  unit_by = c("book_id", "chapter_id", "group"),
  group_by = NULL,
  model_col = "model",
  metrics = c("icc", "kendall_w")
)
```

## Arguments

- data:

  A data frame with one row per model-by-unit combination.

- outcome:

  Character string naming the score column (default `"mean_outcome"`).

- unit_by:

  Character vector of columns that jointly identify a unit (default
  `c("book_id", "chapter_id", "group")`).

- group_by:

  Optional character vector. If provided, agreement metrics are computed
  separately within each level of these columns (e.g., `"group"` to get
  separate estimates for Democrats and Republicans).

- model_col:

  Character string naming the model column (default `"model"`).

- metrics:

  Character vector of metrics to compute. One or both of `"icc"` and
  `"kendall_w"` (default both).

## Value

A tibble with columns: any `group_by` columns, plus

- metric:

  `"icc"` or `"kendall_w"`.

- value:

  The agreement statistic (0–1 scale).

- interpretation:

  Qualitative label (e.g., "good", "moderate").

- n_models:

  Number of models (raters).

- n_units:

  Number of units (targets).

- p_value:

  p-value for the statistic (F-test for ICC, chi-squared approximation
  for Kendall's W).

## Details

Each model is treated as a **rater** and each unique combination of
`unit_by` columns as a **target**. ICC captures agreement in both level
and rank order; Kendall's W converts the continuous scores to ranks
internally and assesses rank-order concordance only.

### Which metric to report?

- **ICC(2,1)** is the primary recommendation for continuous scores. It
  penalises models that systematically differ in level **and** in rank
  ordering. Interpret with Cicchetti (1994) cut-offs: \< .40 poor,
  .40–.59 fair, .60–.74 good, \>= .75 excellent.

- **Kendall's W** converts the continuous 1–100 scores to ranks and asks
  only whether models rank the units the same way. Useful when the
  absolute scale is arbitrary or when the researcher cares about ordinal
  agreement (e.g., "which book scored highest?") rather than exact score
  match.

For a quick "single consistency score," report ICC. Add Kendall's W as a
supplementary rank-agreement check.

### Aggregation guidance

Always aggregate simulation runs first via
[`aggregate_simulations()`](https://centerconflictcooperation.github.io/nalanda/reference/aggregate_simulations.md).
Failing to do so inflates *n* and distorts agreement estimates.

Units with missing scores for one or more models are excluded from ICC
and Kendall's W because agreement metrics require the same units to be
scored by all raters. The reported `n_units` is the number of complete
units used in the calculation.

## Examples

``` r
if (FALSE) { # \dontrun{
# After aggregating simulations
agg <- aggregate_simulations(sim_data, outcome = "rating",
  by = c("model", "book_id", "chapter_id", "group"))

# Overall agreement
model_agreement(agg, outcome = "mean_rating",
  unit_by = c("book_id", "chapter_id", "group"))

# Agreement by political group
model_agreement(agg, outcome = "mean_rating",
  unit_by = c("book_id", "chapter_id"),
  group_by = "group")
} # }
```
