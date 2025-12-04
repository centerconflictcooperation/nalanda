# Plot chapters over time (multi-timepoint means)

Create a plot showing means over chapter timepoints using
rempsyc::plot_means_over_time for the wide-format response variables.

## Usage

``` r
plot_chapters_over_time(
  chapters,
  dv = "score",
  xtitle = "Chapter",
  ytitle = "Simulated scores",
  plot_title = TRUE,
  ci_type = "between",
  legend.position = "bottom",
  text_size = 20,
  reverse_score = FALSE,
  error_bars = TRUE,
  neutrality_line = TRUE
)
```

## Arguments

- chapters:

  A data frame or list of simulation rows containing columns `book`,
  `chapter`, and the desired `dv`.

- dv:

  Character. Name of the column to plot as the dependent variable
  (default: "score").

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

- reverse_score:

  Logical. Whether to reverse score scale using rempsyc::nice_reverse.

- error_bars:

  Logical. Show error bars.

- neutrality_line:

  Logical. Add a horizontal neutrality line at 50.

## Value

A ggplot2 object.
