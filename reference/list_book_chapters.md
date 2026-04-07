# List book chapter files inside a books directory

Given either a path to a directory of book folders or a single folder
that directly contains chapter files, return a named list where each
element is a character vector of chapter file paths (ordered by number
or name).

## Usage

``` r
list_book_chapters(books_path = "books", extension = "txt")
```

## Arguments

- books_path:

  Character scalar. Path containing subdirectories for each book
  (default "books").

- extension:

  Character scalar file extension to match, without a leading dot by
  default. Defaults to `"txt"`.

## Value

A named list of character vectors of file paths.
