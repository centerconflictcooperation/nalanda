# Pairwise model correlations

Computes Pearson and/or Spearman correlations between every pair of
models on a shared set of units. This is a diagnostic complement to the
omnibus metrics in
[`model_agreement()`](https://centerconflictcooperation.github.io/nalanda/reference/model_agreement.md):
it reveals *which* models diverge.

## Usage

``` r
model_pairwise_cor(
  data,
  outcome = "mean_outcome",
  unit_by = c("book_id", "chapter_id", "group"),
  group_by = NULL,
  model_col = "model",
  methods = c("pearson", "spearman")
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

- methods:

  Character vector of correlation types. One or both of `"pearson"` and
  `"spearman"` (default both).

## Value

A tibble with columns: any `group_by` columns, plus `model_a`,
`model_b`, `method`, `correlation`, and `n_units`.

## Examples

``` r
if (FALSE) { # \dontrun{
pw <- model_pairwise_cor(agg, outcome = "mean_rating",
  unit_by = c("book_id", "chapter_id", "group"))
plot_model_agreement(pw, type = "heatmap")
} # }
```
