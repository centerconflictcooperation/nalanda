# Evaluate text-analysis outputs against reference labels

This helper computes common agreement metrics used in text-analysis
papers, including accuracy, macro precision/recall/F1, Spearman
correlation, and weighted Cohen's kappa.

## Usage

``` r
evaluate_text_analysis(
  data,
  truth_col,
  estimate_col,
  by = NULL,
  metric = c("accuracy", "macro_precision", "macro_recall", "macro_f1", "spearman",
    "weighted_kappa"),
  kappa_weights = c("quadratic", "linear")
)
```

## Arguments

- data:

  A data frame containing reference and predicted columns.

- truth_col:

  Name of the reference-label column.

- estimate_col:

  Name of the model-estimate column.

- by:

  Optional character vector of grouping columns.

- metric:

  Character vector of metrics to compute. Supported values are
  `"accuracy"`, `"macro_precision"`, `"macro_recall"`, `"macro_f1"`,
  `"spearman"`, and `"weighted_kappa"`.

- kappa_weights:

  Weighting scheme for Cohen's kappa. One of `"quadratic"` (default) or
  `"linear"`.

## Value

A tibble with one row per requested group and one column per metric.
