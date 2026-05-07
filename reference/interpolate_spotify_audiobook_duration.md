# Interpolate Spotify audiobook duration from text size

Estimate Spotify audiobook durations for new chapters or books from a
reference data set where Spotify duration is already known. The
predictor can be either text file size in bytes or word count. File size
is often the simplest option when all chapters are plain text files
created by the same workflow.

## Usage

``` r
interpolate_spotify_audiobook_duration(
  reference,
  target = NULL,
  duration_col,
  books_path = NULL,
  target_book = NULL,
  book_col = "book",
  extension = "txt",
  reference_book_col = NULL,
  size_col = NULL,
  words_col = NULL,
  file_col = NULL,
  text_col = NULL,
  measure = c("file_size", "word_count"),
  duration_unit = c("seconds", "minutes", "hours", "hms"),
  output_unit = c("minutes", "seconds", "hours", "hms"),
  method = c("ratio", "lm")
)
```

## Arguments

- reference:

  Data frame with known Spotify durations and, unless `books_path` is
  supplied, a text-size predictor.

- target:

  Data frame with chapters or books to estimate. When `books_path` and
  `target_book` are supplied, this can be left as `NULL`.

- duration_col:

  Character scalar. Column in `reference` containing known Spotify
  duration.

- books_path:

  Character scalar or `NULL`. Optional folder containing one subfolder
  per book, with chapter text files inside each book folder.

- target_book:

  Character vector or `NULL`. Book folder name(s) to estimate when
  `books_path` is supplied.

- book_col:

  Character scalar. Book identifier column in `reference`.

- extension:

  Character scalar. File extension to read from `books_path`.

- reference_book_col:

  Character scalar or `NULL`. Optional book identifier in `reference`.
  When supplied, the predictor is summed within each book and
  `duration_col` must contain one unique duration per book. Use this
  when reference rows are chapter-level but Spotify durations are
  book-level.

- size_col:

  Character scalar or `NULL`. Column containing file sizes in bytes. Use
  this when `measure = "file_size"`.

- words_col:

  Character scalar or `NULL`. Column containing word counts. Use this
  when `measure = "word_count"`.

- file_col:

  Character scalar or `NULL`. Column containing paths to text files. If
  supplied with `measure = "file_size"`, file sizes are computed with
  [`file.info()`](https://rdrr.io/r/base/file.info.html). If supplied
  with `measure = "word_count"`, words are counted from the files.

- text_col:

  Character scalar or `NULL`. Column containing text strings to measure
  directly.

- measure:

  Character scalar. Either `"file_size"` or `"word_count"`.

- duration_unit:

  Unit of `duration_col`: `"seconds"`, `"minutes"`, `"hours"`, or
  `"hms"` for strings like `"6:11:00"`.

- output_unit:

  Unit for the returned estimate column. Use `"hms"` for
  spreadsheet-friendly strings like `"5:56:00"`.

- method:

  Estimation method. `"ratio"` fits a single seconds-per-unit rate
  through the origin. `"lm"` fits a linear model with an intercept.

## Value

A tibble containing `target` plus `.duration_seconds`, an
`estimated_duration_*` column in `output_unit`, `.duration_measure`, and
`.duration_method`. The total estimated duration is also stored in the
`estimated_total_seconds` and `estimated_total_*` attributes.

## Examples

``` r
reference <- tibble::tibble(
  book = c("A", "B"),
  file_size_bytes = c(100000, 150000),
  spotify_duration_minutes = c(120, 180)
)

chapters <- tibble::tibble(
  chapter = c("chapter_1", "chapter_2"),
  file_size_bytes = c(25000, 50000)
)

interpolate_spotify_audiobook_duration(
  reference,
  chapters,
  duration_col = "spotify_duration_minutes",
  size_col = "file_size_bytes",
  duration_unit = "minutes"
)
#> # A tibble: 2 × 6
#>   chapter   file_size_bytes .duration_seconds estimated_duration_minutes
#>   <chr>               <dbl>             <dbl>                      <dbl>
#> 1 chapter_1           25000              1800                         30
#> 2 chapter_2           50000              3600                         60
#> # ℹ 2 more variables: .duration_measure <chr>, .duration_method <chr>
```
