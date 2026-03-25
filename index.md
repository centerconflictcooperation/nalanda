# nalanda: R Toolbox to answer the question: Do books really change lives?

## Overview

The **nalanda** package provides tools for simulating, summarizing, and
plotting chapter-level AI reading responses. It is designed for
workflows that ask whether books shift prosocial attitudes, outgroup
warmth, and affective polarization across chapters or across whole
books.

## Which simulation function should I use?

``` mermaid
flowchart TD
    A["What are you simulating?"] --> B{"Unit of analysis"}
    B -->|"Book chapters"| C{"Design"}
    B -->|"Rows of text\ntweets, headlines,\ncomments"| D["run_text_analysis()"]
    C -->|"Pre/post within the same simulated identity"| E["run_ai_on_chapters()"]
    C -->|"Baseline once, then cumulative chapter updates"| F["run_ai_cumulative_chapters()"]
    C -->|"One prompt only,\nno baseline turn"| G{"Need chapter-specific helper\nor generic prompt control?"}
    G -->|"Simple chapter workflow"| H["run_ai_on_chapters_one_turn()"]
    G -->|"Custom one-turn or multi-turn prompt sequence"| I["simulate_treatment()"]
```

In short:

- Use
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
  for the original two-turn chapter workflow.
- Use
  [`run_ai_cumulative_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_cumulative_chapters.md)
  when later chapters should be interpreted relative to a single
  baseline.
- Use
  [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md)
  for a chapter-based, single-prompt design.
- Use
  [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
  when you want direct control over custom prompt sequences and the
  intervention is not necessarily a chapter.
- Use
  [`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md)
  for dataset-first text annotation tasks such as sentiment, emotion,
  offensiveness, or moral-foundation coding.

## Overlap and consolidation

There is some real overlap now, but it is mostly layered rather than
accidental:

- [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
  and
  [`run_ai_cumulative_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_cumulative_chapters.md)
  are chapter-specific opinion-change workflows.
- [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md)
  is a simplified chapter wrapper for the common single-prompt case.
- [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
  is the more generic prompt-first engine for intervention-style
  simulations.
- [`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md)
  is the new dataset-first path for row-wise psychological text
  analysis.

The current direction should be to consolidate around fewer conceptual
families, not necessarily fewer total exported functions:

- `chapter workflows`:
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md),
  [`run_ai_cumulative_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_cumulative_chapters.md)
- `generic intervention workflows`:
  [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
- `dataset-first annotation workflows`:
  [`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md)

That likely means
[`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md)
should be treated as a convenience wrapper over time rather than as a
separate long-term family. The main redundancy is therefore between the
one-turn chapter helper and the more general
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
interface, not between all functions equally.

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

The main workflow has three steps:

1.  run
    [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
    to get raw turn-level output
2.  convert the raw turns to chapter-level metrics with
    [`compute_run_ai_metrics()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics.md)
3.  summarize or plot the processed results

This README uses bundled toy data so it renders quickly and does not
require live API calls.

``` r
library(nalanda)

raw_turns <- run_ai_on_chapters(
  book_texts = "A short chapter about people from different groups cooperating.",
  groups = c("Democrat", "Republican"),
  context_text = "You are simulating an American adult who politically identifies as a {identity}.",
  question_text = "On a scale from 0 to 100, how warmly do you feel towards {group}s?",
  n_simulations = 2,
  temperature = 0,
  model = "gemini-2.5-flash-lite"
)

chapter_results <- compute_run_ai_metrics(raw_turns)
```

``` r
library(nalanda)

img_paths <- list(
  Democrat = normalizePath("man/figures/dem.png"),
  Republican = normalizePath("man/figures/rep.png")
)

chapter_results <- compute_run_ai_metrics(toy_run_ai_turns)

head(chapter_results[, c("book", "chapter", "sim", "party", "delta_gap")])
#> # A tibble: 6 × 5
#>   book           chapter     sim party      delta_gap
#>   <chr>          <chr>     <int> <chr>          <dbl>
#> 1 Bridge Stories chapter_1     1 Democrat         3  
#> 2 Common Ground  chapter_1     1 Democrat         4  
#> 3 Bridge Stories chapter_1     1 Republican      11  
#> 4 Common Ground  chapter_1     1 Republican       5  
#> 5 Bridge Stories chapter_1     2 Democrat         3.5
#> 6 Common Ground  chapter_1     2 Democrat         4.5
```

Use the processed chapter-level results directly with the time-series
plotting helper:

``` r
plot_chapters_over_time(
  chapters = chapter_results,
  dv = "delta_gap",
  group = "party",
  y_label = "Affective Polarization (Delta Gap)",
  plot_subtitle = "Bundled toy data: 2 simulations per party",
  plot_title = "Results of 4 simulations per book per chapter",
  error_bars = FALSE,
  reverse_score = TRUE,
  groups.order = "none",
  facet = "book",
  facets.order = "decreasing",
  line_width = 1.2,
  point_images = img_paths,
  image_size = 0.09
)
```

![Synthetic chapter-level trajectories of affective polarization change
by party.](reference/figures/README-pressure-1.png)

Synthetic chapter-level trajectories of affective polarization change by
party.

The same workflow scales to:

- raw turn-level output from
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
  after processing with
  [`compute_run_ai_metrics()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics.md)
- summary tables via
  [`summarize_chapter_scores()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_chapter_scores.md)
- saved `.rds` simulation outputs
- book-level summaries via
  `summarize_chapter_scores(..., aggregate_level = "book")`
- visualization with
  [`plot_chapter_trajectories()`](https://centerconflictcooperation.github.io/nalanda/reference/plot_chapter_trajectories.md),
  [`plot_chapters_over_time()`](https://centerconflictcooperation.github.io/nalanda/reference/plot_chapters_over_time.md),
  and
  [`plot_forest_books()`](https://centerconflictcooperation.github.io/nalanda/reference/plot_forest_books.md)

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
