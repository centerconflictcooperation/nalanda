# Plot model-by-unit rank heatmap

Creates a heatmap from the `ranks` element returned by
`summarize_top_units(..., include_ranks = TRUE)`. Rows are units,
columns are models, and cells show each model's rank for that unit.

## Usage

``` r
plot_top_unit_heatmap(
  data,
  item_col = NULL,
  model_col = "model",
  facet_by = NULL,
  top_n_items = NULL,
  show_values = TRUE,
  title = "Unit ranks by model"
)
```

## Arguments

- data:

  The `ranks` data frame from
  [`summarize_top_units()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_top_units.md)
  with `include_ranks = TRUE`.

- item_col:

  Character. Column identifying the ranked item. If `NULL`, the function
  tries to infer `"book"` or `"book_id"`.

- model_col:

  Character. Column identifying the model (default `"model"`).

- facet_by:

  Optional character vector of columns to facet by, e.g. `"party"`.

- top_n_items:

  Optional integer. If supplied, keep only the best `top_n_items` per
  facet, based on average rank across models.

- show_values:

  Logical. If `TRUE` (default), print rank values in cells.

- title:

  Optional plot title.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
top_books <- summarize_top_units(
  agg,
  outcome = "mean_delta_gap",
  item_by = "book",
  rank_within = "party",
  include_ranks = TRUE
)
plot_top_unit_heatmap(top_books$ranks, item_col = "book", facet_by = "party")
} # }
```
