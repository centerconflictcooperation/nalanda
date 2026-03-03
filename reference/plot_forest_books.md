# Create a forest plot of book-level polarization reduction effects

Generates a forest plot displaying mean reduction in affective
polarization across books, including 95% confidence intervals.

## Usage

``` r
plot_forest_books(
  forest_df,
  label_cols = c("book", "ci"),
  header = NULL,
  title = "",
  xlab = "",
  zero = 0,
  show_overall = TRUE,
  ci.vertices = FALSE
)
```

## Arguments

- forest_df:

  A data frame produced by
  [`prepare_forest_books()`](https://centerconflictcooperation.github.io/nalanda/reference/prepare_forest_books.md),
  containing `book`, `mean`, `lower`, and `upper` columns.

- label_cols:

  The names of the columns to be included on the side of the plot.

- header:

  Labels of the columns to be displayed and as specified in
  `label_cols`.

- title:

  Plot title

- xlab:

  X-axis label

- zero:

  Where should the "zero" axis (graph) start.

- show_overall:

  Logical. If TRUE (default), a vertical dashed line indicating the
  overall mean effect is added.

## Value

A `forestplot` grob object.

## Details

Books are ordered from strongest to weakest mean effect.

The plot uses circular markers for point estimates and displays 95%
confidence intervals.

The vertical dashed line (if enabled) represents the average effect
across books.
