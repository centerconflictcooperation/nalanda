# Simulate a generic multi-turn treatment workflow

This function provides a simpler, prompt-first interface for running one
or more turns against an intervention text. Each element of `prompt`
defines one turn in the chat sequence. When `groups` is supplied, the
same prompt sequence is repeated for each group identity; groups do not
create additional turns. For independent alternative prompts that must
start fresh conversations, use
[`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md)
instead.

## Usage

``` r
simulate_treatment(
  intervention_text = "",
  prompt,
  response_type,
  output_mode = c("structured", "text"),
  groups = NULL,
  context_text = NULL,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  checkpoint_dir = NULL,
  checkpoint_prefix = "simulate_treatment",
  save_dir = NULL,
  save_prefix = "results"
)
```

## Arguments

- intervention_text:

  A single character string or a nested list of intervention texts. This
  is mapped internally onto the same job grid used by
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md).
  Defaults to `""`, which is useful when the full treatment is already
  encoded in `prompt` and/or `context_text`.

- prompt:

  Character vector of prompt templates. Each element defines one ordered
  turn in the same conversation, not an independent prompt variant.
  Prompt templates may include `{intervention_text}`, `{identity}`, and
  `{group}` placeholders.

- response_type:

  An `ellmer` structured type specification applied to all turns (for
  example `ellmer::type_object(score = ellmer::type_number())`).

- output_mode:

  Character. `"structured"` (default) uses the backend's
  structured-output support. `"text"` is a compatibility mode for models
  that do not support structured outputs (for example some Anthropic
  models): nalanda appends strict JSON-only instructions to the prompt,
  calls the model as free text, then parses the JSON back into the same
  tabular fields. Text mode is best-effort and stores the original model
  reply in `raw_response`.

- groups:

  Optional character vector of group labels. If supplied, the full
  prompt sequence is rerun for each group identity.

- context_text:

  Optional character scalar or vector. If provided, it is prepended to
  every turn for the corresponding group. Scalar values are recycled
  across groups, and `{identity}` is expanded when present.

- n_simulations:

  Integer. Number of repeated simulations per intervention per identity.

- temperature:

  Numeric. Sampling temperature passed to the chat backend.

- seed:

  Integer. Random seed for reproducibility (incremented for each
  simulation index).

- model:

  Character. Model name for the chat backend.

- integration:

  Optional Portkey/gateway route slug. If supplied and `model` is not
  fully-qualified, nalanda will build `"@{integration}/{model}"`. Use a
  route returned by
  `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`
  when working with the NYU gateway.

- virtual_key:

  Optional legacy virtual key. If supplied and `model` is not
  fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.

- base_url:

  Character. Base URL for API calls.

- excerpt_chars:

  Integer. Number of intervention-text characters to retain in stored
  prompt previews.

- checkpoint_dir:

  Optional directory. If supplied, each completed
  treatment/identity/simulation unit is saved as its own `.Rds` file as
  soon as it finishes. If the same call is rerun with the same
  `checkpoint_dir`, `checkpoint_prefix`, model, treatments, groups, and
  simulations, completed units are loaded from disk and skipped.

- checkpoint_prefix:

  Character scalar used at the start of checkpoint filenames when
  `checkpoint_dir` is supplied.

- save_dir:

  Optional directory. If supplied, each intervention collection is saved
  as one `.Rds` file as soon as all of its treatments, identities, and
  simulations finish.

- save_prefix:

  Character scalar used in book-level filenames when `save_dir` is
  supplied. Files are named `{save_prefix}_{book}.Rds`.

## Value

A tibble of raw turn-level responses, or a named list of tibbles (one
per book/intervention collection). Each row includes `treatment`, `sim`,
`identity`, `turn_index`, `turn_type`, and one column per field returned
by `response_type`, plus stored prompt previews and metadata columns.

## Examples

``` r
if (FALSE) { # \dontrun{
simulate_treatment(
  intervention_text = "A short passage about people working together.",
  prompt = c(
    "Read the following text:\n\n{intervention_text}\n\nRate its readability from 0 to 100."
  ),
  response_type = ellmer::type_object(
    score = ellmer::type_number()
  ),
  n_simulations = 2,
  temperature = 0,
  seed = 42
)

simulate_treatment(
  groups = c("South African", "Danish"),
  context_text = "You are simulating an adult who identifies as {identity}.",
  prompt = c(
    climate_belief = paste(
      "Generally speaking, do you usually think of yourself as Danish or South African?",
      "On a scale from 0 to 100, how accurate do you think this statement is?",
      "Statement: Human activities are causing climate change"
    )
  ),
  response_type = ellmer::type_object(
    rating = ellmer::type_number()
  ),
  n_simulations = 2,
  temperature = 0,
  seed = 42
)
} # }
```
