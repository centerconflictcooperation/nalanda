# Plan and run a multi-turn survey simulation

`run_survey_simulation()` simulates respondents moving through ordered
survey screens. Unlike
[`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md),
screens are turns for the *same* respondent. `participants` is always
used row-wise: it is never crossed with conditions or profile fields.

## Usage

``` r
plan_survey_simulation(
  participants,
  survey_flow,
  participant_id = "participant_id",
  model_config,
  smoke_n = NULL
)

run_survey_simulation(
  participants,
  survey_flow,
  participant_id = "participant_id",
  model_config,
  memory = c("conversation", "profile", "block"),
  dry_run = FALSE,
  smoke_n = NULL,
  output_dir = NULL,
  resume = TRUE,
  on_error = c("stop", "continue"),
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  max_active = 10,
  rpm = 500
)
```

## Arguments

- participants:

  A data frame containing a unique `participant_id` column (or the
  column named by `participant_id`). Other columns are available as
  `{placeholders}` in every screen prompt.

- survey_flow:

  A data frame with unique `screen_id` and `prompt` columns. Optional
  columns are `block`, `response_type` (a list-column of ellmer types),
  `output_mode`, and `display_if` (a logical scalar or a function of
  `(participant, answers)`). `answers` is a named list of prior
  responses.

- participant_id:

  Stable participant identifier column.

- model_config:

  Model configuration accepted by
  [`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md).

- smoke_n:

  Restrict to the first participants for an inexpensive run.

- memory:

  One of `"conversation"` (all preceding turns remain in the chat),
  `"profile"` (a fresh chat for each screen), or `"block"` (a fresh chat
  at each block). Profile/context placeholders are included in every
  prompt under all policies.

- dry_run:

  Return a task plan without model calls.

- output_dir:

  Optional directory for atomic per-turn checkpoints.

- resume:

  Reuse compatible completed turn checkpoints.

- on_error:

  Either `"stop"` or `"continue"`.

- ...:

  Global connection defaults: `integration`, `virtual_key`, `base_url`,
  `excerpt_chars`, `max_active`, and `rpm`. The latter two are recorded
  as provenance; turns for one respondent are deliberately serial.

## Value

`plan_survey_simulation()` returns task rows. The runner returns a list
with turn-level `results`, respondent-level `wide`, `plan`, `tasks`, and
`errors`.
