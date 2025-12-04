# Fix text file encoding and normalize punctuation/whitespace

Read a file as raw bytes, drop NUL characters, guess or use a provided
source encoding, convert to UTF-8 and normalize common punctuation and
newlines.

## Usage

``` r
fix_text_file(path, from = NULL)
```

## Arguments

- path:

  Character scalar. Path to the text file.

- from:

  Optional character. Source encoding to feed to iconv. If NULL the
  function will try to guess and fall back to WINDOWS-1252.

## Value

A character scalar with cleaned text (UTF-8).
