# Validate that chapter labels map to unique parsed chapter numbers

Validate that chapter labels map to unique parsed chapter numbers

## Usage

``` r
validate_chapter_order(chapter, book = NULL, arg_name = "chapter")
```

## Arguments

- chapter:

  Character vector of chapter labels.

- book:

  Optional character scalar used to identify the source book in error
  messages.

- arg_name:

  Character scalar naming the input being validated.

## Value

Integer vector of parsed chapter numbers.
