# Read book chapters into a nested list

Convert a list of chapter file paths (as produced by
[`list_book_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/list_book_chapters.md))
into a nested list of chapter texts: list(book -\> list(chapter_name -\>
text)).

## Usage

``` r
read_book_texts(chapter_list)
```

## Arguments

- chapter_list:

  A named list of character vectors with file paths.

## Value

A nested list of character scalars (texts) with chapter basenames as
names.
