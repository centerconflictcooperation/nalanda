# Summarize simulated chapter scores

Aggregate simulation results by chapter (and book, if present) computing
number of simulations, mean and sd of scores, percent of Republican
responses, and retain a chapter excerpt.

## Usage

``` r
summarize_chapter_scores(x)
```

## Arguments

- x:

  A data frame or list-like object containing simulation rows with at
  least `score` and `chapter` columns. If `book` and `party` are
  present, the summary will include those groupings.

## Value

A tibble summarizing each chapter (and book if present). The returned
object will have the original `model` attribute copied to it.
