# List book chapter files inside a books directory

Given a path to a directory of book folders, return a named list where
each element is a character vector of chapter file paths (ordered by
number or name).

## Usage

``` r
list_book_chapters(books_path = "books")
```

## Arguments

- books_path:

  Character scalar. Path containing subdirectories for each book
  (default "books").

## Value

A named list of character vectors of file paths.
