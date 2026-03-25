# Build a numeric-response prompt for text analysis

This helper creates prompts in the same style used by Rathje et al.
(2024): a direct question, followed by a numeric response instruction,
followed by the text placeholder. The returned prompt is a template and
may include placeholders such as `{text}` or `{language}` that are
expanded later by
[`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md).

## Usage

``` r
make_annotation_prompt(
  question,
  labels = NULL,
  scale = NULL,
  anchors = NULL,
  text_label = "Here is the text:",
  text_placeholder = "{text}"
)
```

## Arguments

- question:

  Character scalar question shown before the response instructions.

- labels:

  Optional character vector of class labels in numeric order. For
  example, `c("positive", "neutral", "negative")`.

- scale:

  Optional numeric vector of length 2 giving the response scale range,
  such as `c(1, 7)`.

- anchors:

  Optional character vector of length 2 giving the low and high anchor
  labels used with `scale`.

- text_label:

  Character scalar introducing the text block.

- text_placeholder:

  Character scalar placeholder to insert where the text should appear.

## Value

A character scalar prompt template.
