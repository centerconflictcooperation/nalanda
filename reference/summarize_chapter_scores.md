# Summarize simulated chapter scores

Aggregate simulation results by chapter (and book, if present) computing
number of simulations, mean and sd of scores, percent of Republican
responses, and retain a chapter excerpt.

## Usage

``` r
summarize_chapter_scores(
  x,
  aggregate_level = c("chapter", "book"),
  by_party = FALSE
)
```

## Arguments

- x:

  A data frame or list-like object containing simulation rows with at
  least `score` and `chapter` columns. If `book` and `party` are
  present, the summary will include those groupings.

- aggregate_level:

  Logical. Default is FALSE. If TRUE and a `book` column is present,
  chapter-level effects are aggregated to the book level using
  inverse-variance weighting (i.e., weights = 1 / (sd_diff^2 / sim)).
  This properly propagates within-chapter simulation uncertainty and
  returns one row per book instead of one row per chapter.

  When FALSE (default), results are summarized at the chapter level.

## Value

A tibble summarizing each chapter (and book if present). The returned
object will have the original `model` attribute copied to it.
