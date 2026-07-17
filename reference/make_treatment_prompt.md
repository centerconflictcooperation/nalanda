# Build a concrete prompt for `simulate_treatment()`

This helper expands a single
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
prompt template into a concrete prompt string. It is useful for
inspecting prompt wording before launching a run, much like
[`make_baseline_prompt()`](https://centerconflictcooperation.github.io/nalanda/reference/make_baseline_prompt.md)
and
[`make_post_prompt()`](https://centerconflictcooperation.github.io/nalanda/reference/make_post_prompt.md)
are useful for
[`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md).

## Usage

``` r
make_treatment_prompt(
  prompt_template,
  intervention_text,
  identity_context = "",
  identity_label = NA_character_
)

build_simulate_treatment_prompt(
  prompt_template,
  intervention_text,
  identity_context = "",
  identity_label = NA_character_
)
```

## Arguments

- prompt_template:

  Character scalar. A single prompt template that may include
  `{intervention_text}`, `{identity}`, and `{group}`.

- intervention_text:

  Character scalar. The intervention text to insert into
  `{intervention_text}`. Invalid multibyte text is repaired as UTF-8,
  with Windows-1252 used as the primary fallback encoding.

- identity_context:

  Character scalar. Optional identity context to prepend to the prompt.

- identity_label:

  Character scalar. Optional identity label used to expand `{identity}`
  and `{group}`.

## Value

A character scalar containing the concrete prompt.

## Examples

``` r
make_treatment_prompt(
  prompt_template = "{intervention_text}\n\nRate this as {identity}.",
  intervention_text = "A short climate message.",
  identity_context = "You are simulating an American adult.",
  identity_label = "American"
)
#> [1] "You are simulating an American adult. A short climate message.\n\nRate this as American."
```
