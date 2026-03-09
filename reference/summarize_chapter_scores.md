# Summarize simulated chapter scores

Aggregate simulation results by chapter (and book, if present) computing
number of simulations, means and SDs for ingroup/outgroup ratings (pre
and post), and difference scores. Retains a chapter excerpt.

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

  A data frame or list-like object containing simulation rows as
  produced by
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md).
  Expected columns include `chapter`, `pre_ingroup`, `post_ingroup`,
  `pre_outgroup`, `post_outgroup`. If `book` and `party` are present,
  the summary will include those groupings.

- aggregate_level:

  Character. One of `"chapter"` (default) or `"book"`. When `"book"`,
  results are aggregated to the book level.

- by_party:

  Logical. If TRUE, summaries are computed separately by party (if
  present).

## Value

A tibble summarizing each chapter (and book if present). The returned
object will have the original `model` attribute copied to it.
