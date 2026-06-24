# Split a book section into chapter files using known headings

Splits one oversized section text file into chapter-level `.txt` files
by finding the ordered chapter headings supplied from a table of
contents. This is useful when uppercase headings alone are ambiguous
because non-chapter subsections use the same visual style.

## Usage

``` r
split_book_section_by_headings(
  input_file,
  chapter_titles,
  output_dir = file.path(dirname(input_file), "chapters"),
  chapter_ids = NULL,
  extension = "txt",
  overwrite = FALSE,
  include_heading = TRUE,
  allow_missing = FALSE
)
```

## Arguments

- input_file:

  Character scalar. Path to the section `.txt` file.

- chapter_titles:

  Character vector of chapter titles, in the order they appear in
  `input_file`.

- output_dir:

  Character scalar. Folder where chapter files should be written.
  Defaults to a `chapters/` subfolder beside `input_file`.

- chapter_ids:

  Optional character vector of file stems to use for output files.
  Defaults to numbered slugs based on `chapter_titles`.

- extension:

  Character scalar output extension, without a leading dot. Defaults to
  `"txt"`.

- overwrite:

  Logical scalar. If `TRUE`, replace existing output files. Defaults to
  `FALSE`.

- include_heading:

  Logical scalar. If `TRUE`, keep each chapter heading as the first line
  of its output file. Defaults to `TRUE`.

- allow_missing:

  Logical scalar. If `TRUE`, missing headings are skipped with a
  warning. Defaults to `FALSE`.

## Value

A tibble with one row per written chapter and columns for the chapter
title, output file, source line boundaries, and word count.
