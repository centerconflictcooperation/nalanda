# Plot units that rank consistently high across models

Creates a dot plot from
[`summarize_top_units()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_top_units.md)
output. Units are ordered by average rank across models, point size
shows the mean score, and text labels show how many models placed the
unit in the top `N`.

## Usage

``` r
plot_top_units(
  data,
  item_col = NULL,
  facet_by = NULL,
  top_n_items = NULL,
  title = "Units most consistently ranked highest",
  x_breaks = NULL,
  x_limits = NULL,
  caption = NULL,
  show_top_n_label = TRUE
)
```

## Arguments

- data:

  Output of
  [`summarize_top_units()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_top_units.md).

- item_col:

  Character. Column identifying the ranked item. If `NULL`, the function
  tries to infer `"book"` or `"book_id"`.

- facet_by:

  Optional character vector of columns to facet by, e.g. `"party"`.

- top_n_items:

  Optional integer. If supplied, keep only the best `top_n_items` per
  facet, based on `mean_rank`.

- title:

  Optional plot title.

- x_breaks:

  Optional numeric vector of x-axis breaks. If `NULL`, integer rank
  breaks are shown by default.

- x_limits:

  Optional numeric vector of length 2. If `NULL`, limits are chosen from
  the displayed mean ranks.

- caption:

  Optional plot caption. If `NULL`, a caption explaining the point-size
  and top-N label encodings is generated when possible.

- show_top_n_label:

  Logical. If `TRUE` (default), label points with the number of models
  placing the item in the top `N`.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
top_books <- summarize_top_units(
  agg,
  outcome = "mean_delta_gap",
  item_by = "book",
  rank_within = "party"
)
plot_top_units(top_books, item_col = "book", facet_by = "party")
} # }
```
