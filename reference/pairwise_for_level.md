# Pairwise model correlations at a chosen analysis level

Aggregates lower-level rows to the requested unit level, then calls
[`model_pairwise_cor()`](https://centerconflictcooperation.github.io/nalanda/reference/model_pairwise_cor.md).
This is a convenience wrapper for cases where data still contain
chapter, party, or simulation-detail rows but the researcher wants
correlations at a broader level, such as book-level correlations.

## Usage

``` r
pairwise_for_level(
  data,
  outcome = "mean_outcome",
  unit_by = c("book_id", "chapter_id", "group"),
  group_by = NULL,
  model_col = "model",
  methods = c("pearson", "spearman"),
  drop_missing = TRUE
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

- drop_missing:

  Logical. Whether to drop rows with missing model, unit, or grouping
  identifiers before aggregating (default `TRUE`).

## Value

Output of
[`model_pairwise_cor()`](https://centerconflictcooperation.github.io/nalanda/reference/model_pairwise_cor.md)
for the requested level.

## Examples

``` r
if (FALSE) { # \dontrun{
# Book-level pairwise correlations from chapter-party-level aggregated data
pw_book <- pairwise_for_level(
  agg,
  outcome = "mean_delta_gap",
  unit_by = "book",
  model_col = "model",
  methods = "pearson"
)

summarize_model_correlations(pw_book, method = "pearson")
} # }
```
