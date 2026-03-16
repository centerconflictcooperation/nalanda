
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nalanda: R Toolbox to answer the question: Do books really change lives?

## Overview

The **nalanda** package provides tools for simulating, summarizing, and
plotting chapter-level AI reading responses. It is designed for
workflows that ask whether books shift prosocial attitudes, outgroup
warmth, and affective polarization across chapters or across whole
books.

## Installation

You can install the development version of nalanda from
[GitHub](https://github.com/) with:

``` r
install.packages(
  'nalanda', repos = c(
    'https://centerconflictcooperation.r-universe.dev', 
    'https://cloud.r-project.org'))
```

## Example workflow

This example uses the bundled `toy_sim_results` dataset so it renders
quickly and does not require running live AI simulations.

``` r
library(nalanda)

img_paths <- list(
  Democrat = normalizePath("man/figures/dem.png"),
  Republican = normalizePath("man/figures/rep.png")
)

head(toy_sim_results[, c("book", "chapter", "sim", "party", "delta_gap")])
#>             book   chapter sim    party delta_gap
#> 1 Bridge Stories chapter_1   1 Democrat        -3
#> 2  Common Ground chapter_1   1 Democrat        -4
#> 3 Bridge Stories chapter_2   1 Democrat        -6
#> 4  Common Ground chapter_2   1 Democrat        -6
#> 5 Bridge Stories chapter_3   1 Democrat        -9
#> 6  Common Ground chapter_3   1 Democrat        -5
```

Use the raw chapter-level results directly with the time-series plotting
helper:

``` r
plot_chapters_over_time(
  chapters = toy_sim_results,
  dv = "delta_gap",
  group = "party",
  y_label = "Affective Polarization (Delta Gap)",
  plot_subtitle = "Bundled toy data: 2 simulations per party",
  plot_title = TRUE,
  error_bars = FALSE,
  reverse_score = TRUE,
  groups.order = "none",
  facet = "book",
  facets.order = "decreasing",
  line_width = 1.2,
  point_images = img_paths,
  image_size = 0.09
)
#> Scale for shape is already present.
#> Adding another scale for shape, which will replace the existing scale.
```

<div class="figure">

<img src="man/figures/README-pressure-1.png" alt="Synthetic chapter-level trajectories of affective polarization change by party." width="100%" />
<p class="caption">

Synthetic chapter-level trajectories of affective polarization change by
party.
</p>

</div>

The same workflow scales to:

- summary tables via `summarize_chapter_scores()`
- results returned by `run_ai_on_chapters()`
- saved `.rds` simulation outputs
- book-level summaries via
  `summarize_chapter_scores(..., aggregate_level = "book")`
- visualization with `plot_chapter_trajectories()`,
  `plot_chapters_over_time()`, and `plot_forest_books()`

For API setup and a live minimal simulation example, see the vignette:

``` r
vignette("getting-started", package = "nalanda")
```

## About the Name

The package is named after [Nalanda
Mahavihara](https://en.wikipedia.org/wiki/Nalanda), one of the most
renowned centers of learning in ancient India. Founded in the 5th
century CE, Nalanda was a Buddhist monastic university that attracted
scholars from across Asia and became a symbol of knowledge, wisdom, and
the pursuit of learning through texts and collaboration.

This name is particularly fitting for a package related to the study of
books and prosociality, reflecting the historical significance of
Nalanda as a center for both scholarly texts and the cooperative
exchange of ideas. The connection resonates with contemporary research
on how books and shared learning can foster prosocial behavior and
cooperation.

The package also includes a small helper to explore historical facts
about Nalanda University:

``` r
library(nalanda)

# Get a random fact about Nalanda University
nalanda()
#> [1] "Nalanda remained an active center of learning for roughly 700 years until the 12th century."
```

Learn more about related research on books, learning, and prosociality:
[Mind and Life Europe - 2024 EVA Recipients &
Projects](https://mindandlife-europe.org/2024-eva-recipients-projects/)
