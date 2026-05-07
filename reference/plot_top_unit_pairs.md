# Plot paired subgroup ranks for top units

Creates a connected Cleveland dot plot from
[`summarize_top_units()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_top_units.md)
output when rankings were computed within a two-level subgroup, such as
party. Each row is an item, dots show subgroup-specific mean ranks, and
connecting lines show how much the ranking differs between subgroups.

## Usage

``` r
plot_top_unit_pairs(
  data,
  item_col = NULL,
  subgroup_col = "party",
  top_n_items = NULL,
  subgroup_order = NULL,
  title = "Paired subgroup ranks",
  x_breaks = NULL,
  x_limits = NULL
)
```

## Arguments

- data:

  Output of
  [`summarize_top_units()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_top_units.md)
  with a subgroup column such as `"party"`.

- item_col:

  Character. Column identifying the ranked item. If `NULL`, the function
  tries to infer `"book"` or `"book_id"`.

- subgroup_col:

  Character. Two-level subgroup column to connect, e.g. `"party"`.

- top_n_items:

  Optional integer. If supplied, keep only items with the best average
  `mean_rank` across subgroups.

- subgroup_order:

  Optional character vector giving the two subgroup levels in display
  order.

- title:

  Optional plot title.

- x_breaks:

  Optional numeric vector of x-axis breaks. If `NULL`, integer rank
  breaks are shown by default.

- x_limits:

  Optional numeric vector of length 2. If `NULL`, limits are chosen from
  the displayed mean ranks.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
top_books_party <- summarize_top_units(
  agg,
  outcome = "mean_delta_gap",
  item_by = "book",
  rank_within = "party"
)
plot_top_unit_pairs(top_books_party, item_col = "book", subgroup_col = "party")
} # }
```
