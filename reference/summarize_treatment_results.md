# Summarize generic treatment results

Aggregate raw outputs from
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
by chapter or book, computing counts plus means and SDs for numeric
response fields. This is useful for structured outputs such as
readability, sentiment, or any other custom numeric scores returned by
the model.

## Usage

``` r
summarize_treatment_results(
  x,
  aggregate_level = c("chapter", "book"),
  by_identity = FALSE,
  by_turn = TRUE,
  fields = NULL
)
```

## Arguments

- x:

  A data frame or list-like object containing raw rows as produced by
  [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md).
  If a list is supplied, nested data frames are flattened before
  summarising.

- aggregate_level:

  Character. One of `"chapter"` (default) or `"book"`.

- by_identity:

  Logical. If `TRUE`, summaries are computed separately by identity when
  an `identity` column is present.

- by_turn:

  Logical. If `TRUE` (default), summaries are computed separately by
  turn using `turn_type` when available, otherwise `turn_index`.

- fields:

  Optional character vector of numeric columns to summarize. Defaults to
  all numeric columns except common bookkeeping fields such as `sim`,
  `turn_index`, and `chapter_index`.

## Value

A tibble summarizing the requested aggregation level. Numeric fields are
returned as `mean_*` and `sd_*` columns, alongside a `sim` count. Model
metadata attributes are copied to the result.

## Examples

``` r
readability <- tibble::tibble(
  book = c("Book A", "Book A", "Book A"),
  chapter = c("chapter_1", "chapter_1", "chapter_2"),
  sim = c(1, 2, 1),
  turn_type = "turn_1",
  readability_score = c(6, 8, 7),
  readability_confidence = c(4, 5, 4)
)

summarize_treatment_results(readability)
#> # A tibble: 2 × 9
#>   book   chapter   turn_type   sim mean_readability_score sd_readability_score
#>   <chr>  <chr>     <chr>     <int>                  <dbl>                <dbl>
#> 1 Book A chapter_1 turn_1        2                      7                 1.41
#> 2 Book A chapter_2 turn_1        1                      7                NA   
#> # ℹ 3 more variables: mean_readability_confidence <dbl>,
#> #   sd_readability_confidence <dbl>, chapter_index <int>
summarize_treatment_results(readability, aggregate_level = "book")
#> # A tibble: 1 × 7
#>   book   turn_type   sim mean_readability_score sd_readability_score
#>   <chr>  <chr>     <int>                  <dbl>                <dbl>
#> 1 Book A turn_1        3                      7                    1
#> # ℹ 2 more variables: mean_readability_confidence <dbl>,
#> #   sd_readability_confidence <dbl>
```
