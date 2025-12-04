# Rename chapter text files in a folder to a sequential order

Scans a folder for .txt files and renames them to chapter1.txt,
chapter2.txt, ... using heuristics for ordering (intro, part 1/2,
numeric chapter numbers, appendix, etc.).

## Usage

``` r
rename_chapters(folder)
```

## Arguments

- folder:

  Character scalar. Path to the folder containing .txt files.

## Value

A tibble with columns `old_path`, `base`, `order_score`, `new_name`, and
`new_path`.
