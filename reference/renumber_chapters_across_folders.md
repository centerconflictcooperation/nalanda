# Renumber chapter files across ordered folders

Renumbers chapter files across multiple folders while preserving each
file's title slug. This is useful after splitting a book by part, where
each part folder starts at `01_...` but the full book needs one
chronological sequence.

## Usage

``` r
renumber_chapters_across_folders(
  folders,
  extension = "txt",
  start = 1L,
  width = 2L,
  dry_run = FALSE
)
```

## Arguments

- folders:

  Character vector of folders, in chronological order.

- extension:

  Character scalar file extension to match, without a leading dot by
  default. Defaults to `"txt"`.

- start:

  Integer scalar. First chapter number to use. Defaults to `1`.

- width:

  Integer scalar. Minimum zero-padding width. Defaults to `2`.

- dry_run:

  Logical scalar. If `TRUE`, return the renaming plan without changing
  files. Defaults to `FALSE`.

## Value

A tibble with one row per chapter file and columns describing the old
and new file paths.
