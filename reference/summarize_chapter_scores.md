# Summarize simulated chapter scores

Aggregate simulation results by chapter (and book, if present) computing
number of simulations, means and SDs for core model outputs. In the
current schema, this includes ingroup/outgroup pre-post ratings plus
delta and gap metrics (for example delta_outgroup, delta_ingroup, and
delta_gap). Retains a chapter excerpt at chapter level.

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
  produced by run_ai_on_chapters(). Expected columns include chapter,
  pre/post ingroup-outgroup fields, and the derived difference columns
  used in summaries (pre_gap, post_gap, delta_outgroup, delta_ingroup,
  delta_gap). If book and party are present, the summary will include
  those groupings.

- aggregate_level:

  Character. One of "chapter" (default) or "book". When "book", results
  are aggregated to the book level.

- by_party:

  Logical. If TRUE, summaries are computed separately by party (if
  present).

## Value

A tibble summarizing each chapter (and book if present). The returned
object will have the original model attribute copied to it.
