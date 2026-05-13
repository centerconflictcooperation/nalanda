# Combine split chapter chunk files

Combines text files named with page/range suffixes such as
`3_part1-001-050.txt`, `3_part1-051-100.txt`, and `3_part1-101-138.txt`
into a single `3_part1.txt` file. Files without a trailing `-start-end`
range are copied to `output_dir` when needed.

## Usage

``` r
combine_split_chapter_files(
  input_dir,
  output_dir = input_dir,
  extension = "txt",
  separator = "\n\n",
  overwrite = FALSE,
  remove_sources = FALSE
)
```

## Arguments

- input_dir:

  Character scalar. Folder containing chapter `.txt` files.

- output_dir:

  Character scalar. Folder where consolidated files should be written.
  Defaults to `input_dir`.

- extension:

  Character scalar file extension to match, without a leading dot by
  default. Defaults to `"txt"`.

- separator:

  Character scalar. Text inserted between chunks when combining them.
  Defaults to two line breaks.

- overwrite:

  Logical scalar. If `TRUE`, replace existing output files. Defaults to
  `FALSE`.

- remove_sources:

  Logical scalar. If `TRUE`, remove split source chunk files after all
  outputs are written successfully. Defaults to `FALSE`.

## Value

A tibble with one row per output file and columns describing the output
path, source files, and action taken.
