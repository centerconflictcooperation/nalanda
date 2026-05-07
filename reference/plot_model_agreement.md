# Plot inter-model agreement

Creates diagnostic visualizations for model agreement or pairwise
correlation results.

## Usage

``` r
plot_model_agreement(data, type = c("metrics", "heatmap"), method = NULL)
```

## Arguments

- data:

  Output of
  [`model_agreement()`](https://centerconflictcooperation.github.io/nalanda/reference/model_agreement.md)
  (for `type = "metrics"`) or
  [`model_pairwise_cor()`](https://centerconflictcooperation.github.io/nalanda/reference/model_pairwise_cor.md)
  (for `type = "heatmap"`).

- type:

  Character. `"metrics"` for a dot plot of agreement statistics;
  `"heatmap"` for a pairwise correlation tile plot.

- method:

  Character. Correlation method to plot when `type = "heatmap"`:
  `"spearman"` for rank correlations or `"pearson"` for linear
  correlations on the continuous scores. If `NULL` (default), Spearman
  is used when available, otherwise Pearson is used.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_model_agreement(model_agreement(agg, outcome = "mean_rating"),
  type = "metrics")
plot_model_agreement(model_pairwise_cor(agg, outcome = "mean_rating"),
  type = "heatmap")
plot_model_agreement(model_pairwise_cor(agg, outcome = "mean_rating"),
  type = "heatmap", method = "pearson")
} # }
```
