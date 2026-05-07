# Summarize units that rank consistently high across models

Aggregates lower-level rows to a chosen unit level, ranks units within
each model, and summarizes which units most consistently appear near the
top across models. This is useful for questions such as "Which books
consistently have the strongest effects across models?"

## Usage

``` r
summarize_top_units(
  data,
  outcome = "mean_outcome",
  item_by = "book_id",
  rank_within = NULL,
  model_col = "model",
  top_n = 3,
  higher_is_better = TRUE,
  include_ranks = FALSE,
  drop_missing = TRUE
)
```

## Arguments

- data:

  A data frame with one row per model-by-unit combination.

- outcome:

  Character string naming the score column (default `"mean_outcome"`).

- item_by:

  Character vector identifying the items to rank, e.g. `"book"` or
  `"book_id"`.

- rank_within:

  Optional character vector defining separate ranking contexts, e.g.
  `"party"` to rank books separately within party.

- model_col:

  Character string naming the model column (default `"model"`).

- top_n:

  Integer. Number of top-ranked items to count for each model.

- higher_is_better:

  Logical. If `TRUE` (default), larger outcome values receive better
  ranks. If `FALSE`, smaller values receive better ranks.

- include_ranks:

  Logical. If `TRUE`, return a list with both the summary table and the
  model-level ranks. If `FALSE` (default), return only the summary
  table.

- drop_missing:

  Logical. Whether to drop rows with missing model, item, or
  ranking-context identifiers before aggregating (default `TRUE`).

## Value

A tibble, or a list with `summary` and `ranks` when
`include_ranks = TRUE`.

The summary table contains:

- `rank_within` columns:

  Optional grouping columns used to define separate ranking contexts,
  such as party.

- `item_by` columns:

  The ranked item identifiers, such as book.

- `mean_score`:

  Mean outcome score for the item across models.

- `mean_rank`:

  Average rank of the item across models. Lower values indicate more
  consistently high-ranked items when `higher_is_better = TRUE`.

- `median_rank`:

  Median rank of the item across models.

- `top_n_models`:

  Number of models that ranked the item within the top `top_n` items in
  its ranking context. For example, if `top_n = 3` and
  `top_n_models = 4`, then 4 models placed that item in their top 3.

- `n_models`:

  Number of models with non-missing ranks for the item.

- `top_n`:

  The top-N threshold used to compute `top_n_models`.

- `top_n_label`:

  Compact display label combining `top_n_models` and `n_models`, such as
  `"4/5"`.

When `include_ranks = TRUE`, the `ranks` table contains one row per
model-by-item combination, including `score`, `rank`, and `top_n`.

## Examples

``` r
if (FALSE) { # \dontrun{
summarize_top_units(
  agg,
  outcome = "mean_delta_gap",
  item_by = "book",
  rank_within = "party",
  model_col = "model",
  top_n = 3
)
} # }
```
