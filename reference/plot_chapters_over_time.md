# Plot chapters over time (multi-timepoint means)

Create a plot showing means over chapter timepoints using
rempsyc::plot_means_over_time for the wide-format response variables.

## Usage

``` r
plot_chapters_over_time(
  chapters,
  dv = "mean_diff",
  group = "book",
  xtitle = "Chapter",
  ytitle = "Simulated scores",
  plot_title = TRUE,
  ci_type = "between",
  legend.position = "bottom",
  text_size = 20,
  line_width = 3,
  point_size = 4,
  reverse_score = FALSE,
  error_bars = TRUE,
  neutrality_line = TRUE,
  facet = NULL
)
```

## Arguments

- chapters:

  A data frame or list of simulation rows containing columns `book`,
  `chapter`, and the desired `dv`.

- dv:

  Character. Name of the column to plot as the dependent variable
  (default: "score").

- group:

  The group by which to plot the variable

- xtitle:

  Character. X-axis label.

- ytitle:

  Character. Y-axis label.

- plot_title:

  Logical. Whether to include a title.

- ci_type:

  Character. Type of confidence interval to pass to
  [`rempsyc::plot_means_over_time`](https://rempsyc.remi-theriault.com/reference/plot_means_over_time.html).

- legend.position:

  Position for legend.

- text_size:

  Numeric. Base text size for axis/title text.

- line_width:

  Numeric. Line thickness used in `geom_line()`. Defaults to 3. Can be
  reduced for publication figures or increased for presentation slides.

- point_size:

  Numeric. Point size used in `geom_point()`. Defaults to 4. Adjust to
  improve readability depending on output format.

- reverse_score:

  Logical. Whether to reverse score scale using rempsyc::nice_reverse.

- error_bars:

  Logical. Show error bars.

- neutrality_line:

  Logical. Add a horizontal neutrality line at 50.

- facet:

  The variable by which to facet grid.

## Value

A ggplot2 object.
