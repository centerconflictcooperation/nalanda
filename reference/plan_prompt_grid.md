# Plan a multi-model, multi-prompt grid

Validate model and prompt configuration tables and estimate the number
of model responses before making any calls. This is the dry-run
companion to
[`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md).
Prompt variants are independent calls, not sequential turns in one
conversation.

## Usage

``` r
plan_prompt_grid(data, prompt_variants, model_config, smoke_n = NULL)
```

## Arguments

- data:

  A data frame accepted by
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

## Value

A tibble with one row per active model-prompt combination and columns
for input rows, completion batches, and estimated response calls.
