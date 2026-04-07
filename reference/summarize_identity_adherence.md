# Summarize whether the model adopts the requested identity

This helper turns raw simulation output into a tally table showing which
reported `party` was returned for each requested `identity`. It is
especially useful for first-turn checks from
[`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md),
where the main question is whether the model actually accepts the
assigned identity.

## Usage

``` r
summarize_identity_adherence(
  x,
  by = c("model", "book", "chapter", "identity"),
  compact = FALSE,
  expected_col = "identity",
  observed_col = "party"
)
```

## Arguments

- x:

  A data frame or list-like object containing raw rows from
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md),
  [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md),
  or another workflow with `identity`, `party`, and `sim` columns.

- by:

  Character vector of columns to group by before tallying. Defaults to
  `c("model", "book", "chapter", "identity")`. Missing columns are
  silently ignored. When `compact = TRUE`, the default drops `identity`
  so the result is one row per model/chapter grouping unless you
  explicitly include `identity`.

- compact:

  Logical. If `TRUE`, return a wide compact summary with one row per
  grouping combination and one `rate_*` column per observed identity
  label.

- expected_col:

  Character scalar naming the requested identity column. Defaults to
  `"identity"`.

- observed_col:

  Character scalar naming the model-reported identity column. Defaults
  to `"party"`.

## Value

A tibble with one row per grouping combination and reported identity,
including counts (`n`), totals within group (`total_n`), proportions
(`prop`), and a logical `matches_requested`.

## Details

Because raw chapter outputs can contain repeated rows per simulation
(for example one row per target group, or one row per turn), this
function first reduces the input to one identity-assignment row per
simulated unit.

## Examples

``` r
x <- tibble::tibble(
  chapter = c("chapter_1", "chapter_1", "chapter_1", "chapter_1"),
  sim = c(1, 1, 2, 2),
  identity = c("Democrat", "Democrat", "Democrat", "Democrat"),
  party = c("Democrat", "Democrat", "Republican", "Republican"),
  target_group = c("Democrat", "Republican", "Democrat", "Republican"),
  rating = c(70, 40, 68, 35)
)

summarize_identity_adherence(x)
#> # A tibble: 2 × 7
#>   chapter   identity party          n total_n  prop matches_requested
#>   <chr>     <chr>    <chr>      <int>   <int> <dbl> <lgl>            
#> 1 chapter_1 Democrat Democrat       1       2   0.5 TRUE             
#> 2 chapter_1 Democrat Republican     1       2   0.5 FALSE            
```
