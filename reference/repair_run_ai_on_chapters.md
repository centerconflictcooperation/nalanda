# Repair failed chapter simulation units

Reads a saved result (or accepts one in memory), reruns retryable failed
simulation units, and returns a copy with those units replaced.
Permanent Azure content-policy failures are retained without being
retried. The source file is never modified; inspect the result and call
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) yourself.

## Usage

``` r
repair_run_ai_on_chapters(
  x,
  book_texts,
  groups,
  context_text,
  question_text,
  output_mode = c("structured", "text"),
  temperature = NULL,
  seed = 42,
  model = NULL,
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  on_error = c("stop", "skip")
)
```

## Arguments

- x:

  A result from
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
  or a path to its `.Rds` file.

- book_texts:

  A single character (one chapter) or a nested list of books -\>
  chapters as returned by
  [`read_book_texts()`](https://centerconflictcooperation.github.io/nalanda/reference/read_book_texts.md).

- groups:

  Character vector of group labels (length \>= 2). These are the groups
  being compared. Example: `c("Democrat", "Republican")`.

- context_text:

  Character. Either:

  - A scalar template containing `{identity}`, which will be expanded
    once for each group (e.g.,
    `"You are simulating an American adult who politically identifies as a {identity}."`),
    or

  - A character vector of length equal to `length(groups)`, where each
    element is the full context for the corresponding group identity.

- question_text:

  Character scalar. A question template containing the placeholder
  `{group}`, which will be replaced with each group label. Example:
  `"On a scale from 0 to 100, how warmly do you feel towards {group}s?"`

- output_mode:

  Character. `"structured"` (default) uses the backend's
  structured-output support. `"text"` is a compatibility mode for models
  that do not support structured outputs (for example some Anthropic
  models): nalanda appends strict JSON-only instructions to the prompt,
  calls the model as free text, then parses the JSON back into the same
  fields used by the rest of the pipeline. Text mode is best-effort and
  stores the original model reply in `raw_response`.

- temperature:

  Numeric. Sampling temperature passed to the chat backend.

- seed:

  Integer. Random seed for reproducibility (incremented for each
  simulation).

- model:

  Character. Model name for the chat backend (for example,
  `"gemini-2.5-flash-lite"`). The value is passed directly to
  `ellmer::chat_portkey(model = ...)`.

- integration:

  Optional Portkey/gateway route slug. Should look like `"vertexai"` or
  another route returned by
  `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`.
  If supplied and `model` is not fully-qualified (does not start with
  `"@"`), nalanda will build `"@{integration}/{model}"`. In some
  gateways this slug is not the upstream provider name. When available,
  a fully-qualified model string such as `"@gpt-5-mini/gpt-5-mini"` is
  the most reliable option. When both `nalanda.integration` and
  `nalanda.virtual_key` options are set and neither argument is
  supplied, `integration` is preferred.

- virtual_key:

  Optional legacy virtual key. Should look like `"gemini-8c2498"` or
  similar. If supplied and `model` is not fully-qualified, nalanda will
  build `"@{virtual_key}/{model}"`. Use either `integration` or
  `virtual_key`, not both when explicitly supplying function arguments.

- base_url:

  Character. Base URL for API calls.

- excerpt_chars:

  Integer. Number of chapter characters to retain in the stored
  post-prompt preview shown in results.

- on_error:

  Error policy for the repair attempts.

## Value

A repaired result with the same outer shape as `x`. Attributes
`repaired_units` and `non_retryable_units` identify the units selected
for retry and those retained because of content-policy failures,
respectively.
