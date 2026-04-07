# Rename chapter text files in a folder to a sequential order

Scans a folder for chapter files and renames them to `chapter1.ext`,
`chapter2.ext`, ... using heuristics for ordering (intro, part 1/2,
numeric chapter numbers, appendix, etc.).

## Usage

``` r
rename_chapters(folder, extension = "txt")
```

## Arguments

- folder:

  Character scalar. Path to the folder containing chapter files.

- extension:

  Character scalar file extension to match, without a leading dot by
  default. Defaults to `"txt"`.

## Value

A tibble with columns `old_path`, `base`, `order_score`, `new_name`, and
`new_path`.
