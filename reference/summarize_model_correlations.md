# Summarize pairwise model correlations

Condenses the output of
[`model_pairwise_cor()`](https://centerconflictcooperation.github.io/nalanda/reference/model_pairwise_cor.md)
into one row per correlation method and subgroup. The summary includes
the average pairwise correlation and the "most aligned" model, defined
as the model with the highest average correlation with all other models.
This is useful for adding a small headline annotation to
correlation-matrix slides.

## Usage

``` r
summarize_model_correlations(data, method = NULL, digits = 2)
```

## Arguments

- data:

  Output of
  [`model_pairwise_cor()`](https://centerconflictcooperation.github.io/nalanda/reference/model_pairwise_cor.md).

- method:

  Optional character. If supplied, keep only one correlation method,
  e.g. `"pearson"` or `"spearman"`.

- digits:

  Integer. Number of decimal places used in the display `label`.

## Value

A tibble with any subgroup columns from `data`, plus `method`,
`mean_correlation`, `median_correlation`, `min_correlation`,
`max_correlation`, `n_pairs`, `most_aligned_model`,
`most_aligned_correlation`, and `label`.

## Examples

``` r
if (FALSE) { # \dontrun{
pw <- model_pairwise_cor(agg, outcome = "mean_rating",
  unit_by = c("book_id", "chapter_id", "group"))
summarize_model_correlations(pw, method = "pearson")
} # }
```
