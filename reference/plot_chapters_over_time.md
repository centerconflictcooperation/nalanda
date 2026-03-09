# Plot chapters over time (multi-timepoint means)

Create a plot showing means over chapter timepoints using
rempsyc::plot_means_over_time for the wide-format response variables.

## Usage

``` r
plot_chapters_over_time(
  chapters,
  dv = "delta_gap",
  group = "book",
  xtitle = "Chapter",
  ytitle = "Simulated scores",
  plot_title = TRUE,
  plot_subtitle = "",
  ci_type = "between",
  legend.position = "bottom",
  groups.order = "decreasing",
  text_size = 20,
  line_width = 3,
  point_size = 4,
  reverse_score = FALSE,
  error_bars = TRUE,
  neutrality_line = TRUE,
  facet = NULL,
  facets.order = "increasing"
)
```

## Arguments

- chapters:

  A data frame or list of simulation rows containing columns `book`,
  `chapter`, and the desired `dv`.

- dv:

  Character. Name of the column to plot as the dependent variable
  (default: "pre_post_outgroup_difference").

- group:

  The group by which to plot the variable

- xtitle:

  Character. X-axis label.

- ytitle:

  Character. Y-axis label.

- plot_title:

  Logical. Whether to include a title.

- plot_subtitle:

  Optional plot subtitle.

- ci_type:

  Character. Type of confidence interval to pass to
  [`rempsyc::plot_means_over_time`](https://rempsyc.remi-theriault.com/reference/plot_means_over_time.html).

- legend.position:

  Position for legend.

- groups.order:

  Specifies the desired display order of the groups on the legend.
  Either provide the levels directly, or a string: "increasing" or
  "decreasing", to order based on the average value of the variable on
  the y axis, or "string.length", to order from the shortest to the
  longest string (useful when working with long string names). "Defaults
  to "decreasing".

- text_size:

  Numeric. Base text size for axis/title text.

- line_width:

  Numeric. Line thickness used in `geom_line()`. Defaults to 3. Can be
  reduced for publication figures or increased for presentation slides.

- point_size:

  Numeric. Point size used in `geom_point()`. Defaults to 4. Adjust to
  improve readability depending on output format.

- reverse_score:

  Logical. Whether to reverse score scale.

- error_bars:

  Logical. Show error bars.

- neutrality_line:

  Logical. Add a horizontal neutrality line at 50.

- facet:

  The variable by which to facet grid.

- facets.order:

  Specifies the desired display order of facet panels. Either provide
  the levels directly, or a string: "increasing" or "decreasing", to
  order panels based on the average value of the y variable, or
  "string.length" to order panels by facet label length. Defaults to
  "increasing".

## Value

A ggplot2 object.
