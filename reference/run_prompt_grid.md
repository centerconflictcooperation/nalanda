# Run a multi-model, multi-prompt grid

This function is a deliberately thin orchestration layer over
[`run_structured_responses()`](https://centerconflictcooperation.github.io/nalanda/reference/run_structured_responses.md).
It expands validated active model and prompt configurations, runs each
completion as an independently resumable unit, and keeps model, family,
prompt, and completion provenance in the raw results.
[`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md)
remains available as the text-annotation use-case wrapper.

## Usage

``` r
run_prompt_grid(
  data,
  content_col = "text",
  prompt_variants,
  response_type,
  model_config,
  id_col = NULL,
  smoke_n = NULL,
  dry_run = FALSE,
  output_dir = NULL,
  resume = TRUE,
  on_error = c("stop", "continue"),
  progress = interactive(),
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

  A data frame accepted by
  [`run_structured_responses()`](https://centerconflictcooperation.github.io/nalanda/reference/run_structured_responses.md).

- content_col, id_col, response_type:

  Passed to
  [`run_structured_responses()`](https://centerconflictcooperation.github.io/nalanda/reference/run_structured_responses.md).

- prompt_variants:

  A named character vector of independent prompt templates, or a data
  frame with `prompt_id` and `prompt` columns. An optional logical
  `active` column can disable prompt variants without deleting them.

- model_config:

  A character vector of model names, or a data frame with a required
  `model` column. Supported optional columns are `model_id`, `family`,
  `integration`, `virtual_key`, `base_url`, `temperature`,
  `output_mode`, `seed`, `max_active`, `rpm`, `n_completions`, and
  `active`. Missing settings use the same defaults as
  [`run_structured_responses()`](https://centerconflictcooperation.github.io/nalanda/reference/run_structured_responses.md).

- smoke_n:

  Optional positive integer limiting the workflow to the first `smoke_n`
  input rows. `smoke_n = 1` is useful before an expensive run.

- dry_run:

  Logical. If `TRUE`, return the call plan without constructing a chat
  or making model calls.

- output_dir:

  Optional directory for one RDS checkpoint per completed
  model-prompt-completion unit. Files contain raw, unaggregated results.

- resume:

  Logical. If `TRUE` and `output_dir` is supplied, compatible completed
  units are read instead of rerun. Checkpoint identity includes inputs,
  prompts, response type, and model settings.

- on_error:

  Either `"stop"` or `"continue"`. With `"continue"`, failed units are
  recorded in the returned `errors` table and are not checkpointed, so a
  later resumed run retries them.

- progress:

  Logical. Emit one concise progress message per completion.

- integration, virtual_key, base_url, excerpt_chars, max_active, rpm:

  Global defaults passed to
  [`run_structured_responses()`](https://centerconflictcooperation.github.io/nalanda/reference/run_structured_responses.md).
  A non-missing value in a model configuration row overrides the
  corresponding global default.

## Value

If `dry_run = TRUE`, the plan tibble. Otherwise, a list with `results`
(combined raw results), `plan`, `tasks`, and `errors`.
