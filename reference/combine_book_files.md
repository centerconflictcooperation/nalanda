# Combine chapter text files into one numbered file per book

Takes chapter files named like `1_howcanyou.txt` and `2_howcanyou.txt`,
groups them by the shared title stem after the underscore, orders
chapters by their numeric prefix, and writes one combined `.txt` file
per group using filenames like `1_howcanyou.txt`.

## Usage

``` r
combine_book_files(
  input_dir,
  output_dir = file.path(input_dir, "combined"),
  separator = "\n\n",
  overwrite = FALSE
)
```

## Arguments

- input_dir:

  Character scalar. Folder containing chapter `.txt` files.

- output_dir:

  Character scalar. Folder where combined `.txt` files should be
  written. Defaults to a `combined/` subfolder inside `input_dir`.

- separator:

  Character scalar. Text inserted between chapters when combining them.
  Defaults to two line breaks.

- overwrite:

  Logical scalar. If `TRUE`, replace existing output files. Defaults to
  `FALSE`.

## Value

A tibble with one row per combined book and columns describing the
numeric output file, original title stem, and source chapter numbers.
