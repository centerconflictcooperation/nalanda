# Run structured prompts independently over data-frame rows

This is nalanda's neutral one-turn prompting primitive. It applies one
prompt template independently to each row, repeats that operation when
requested, and extracts a common structured response with `ellmer`.
Use-case functions such as
[`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md)
can wrap this function with domain-specific terminology while
[`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md)
adds models and independent prompt variants.

## Usage

``` r
run_structured_responses(
  data,
  content_col = "text",
  prompt,
  response_type,
  output_mode = c("structured", "text"),
  id_col = NULL,
  n_completions = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  max_active = 10,
  rpm = 500
)
```

## Arguments

- data:

  A data frame with at least one content column.

- content_col:

  Name of the column containing the primary content. This column is
  abbreviated in stored prompt previews; any column can still be
  referenced by the prompt template.

- prompt:

  Character scalar prompt template. It may reference any columns in
  `data` using `{column_name}` placeholders.

- response_type:

  An `ellmer` structured type specification, for example
  `ellmer::type_object(score = ellmer::type_number())`.

- output_mode:

  Character. `"structured"` (default) uses the backend's
  structured-output support. `"text"` is a compatibility mode for models
  that do not support structured outputs (for example some Anthropic
  models): nalanda appends strict JSON-only instructions to the prompt,
  calls the model as free text, then parses the JSON back into the same
  fields. Text mode is best-effort and stores the original model reply
  in `raw_response`.

- id_col:

  Optional column name identifying each input row. When omitted, a
  pre-existing `row_id` is used or a sequential `row_id` is created.

- n_completions:

  Integer. Number of independent completions per row.

- temperature:

  Numeric. Sampling temperature passed to the backend.

- seed:

  Integer. Random seed for reproducibility.

- model:

  Character. Model name for the chat backend.

- integration:

  Optional Portkey/gateway route slug. Use a route returned by
  `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`
  when working with the NYU gateway.

- virtual_key:

  Optional legacy virtual key.

- base_url:

  Character. Base URL for API calls.

- excerpt_chars:

  Integer. Number of content characters to retain in stored prompt
  previews.

- max_active:

  Integer. Maximum number of concurrent requests passed to
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html)
  in structured mode. Text mode runs plain chat requests sequentially.

- rpm:

  Integer. Requests-per-minute cap passed to
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html)
  in structured mode. Text mode runs plain chat requests sequentially.

## Value

A tibble containing the original row metadata, completion index,
structured response fields, and stored prompt previews.
