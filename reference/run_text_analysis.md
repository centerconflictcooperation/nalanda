# Run row-wise text analysis with a prompt template

This function applies a prompt template to each row of a text dataset
and extracts structured responses with `ellmer`. It is designed for
dataset-first workflows such as sentiment, emotion, offensiveness, or
moral-foundation annotation across many short texts.

## Usage

``` r
run_text_analysis(
  data,
  text_col = "text",
  prompt,
  response_type,
  output_mode = c("structured", "text"),
  id_col = NULL,
  n_simulations = 1,
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

  A data frame with at least one text column.

- text_col:

  Name of the column containing the text to analyze.

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

  Optional column name identifying each text row. When omitted, a
  sequential `text_id` is created.

- n_simulations:

  Integer. Number of repeated runs per row.

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

  Integer. Number of text characters to retain in stored prompt
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

A tibble containing the original row metadata, simulation index,
structured response fields, and stored prompt previews.
