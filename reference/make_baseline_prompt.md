# Build the baseline (Turn 1) prompt

Constructs the prompt: identity context + question(s). If the question
template contains `{group}`, it is expanded once per group (ingroup
first). Otherwise, the question is used as-is (single-question mode).

## Usage

``` r
make_baseline_prompt(
  identity_context,
  question_template,
  groups,
  identity_label
)
```

## Arguments

- identity_context:

  Character scalar. The full context string for this identity.

- question_template:

  Character scalar. Optionally contains `{group}` placeholder for
  per-group expansion.

- groups:

  Character vector of all group labels.

- identity_label:

  Character scalar. The group label assigned as identity (used to
  determine ingroup-first ordering).

## Value

Character scalar prompt.

## Examples

``` r
# Per-group mode (asks about each group, ingroup first):
make_baseline_prompt(
  identity_context = "You are simulating an American Democrat.",
  question_template = "How warmly (0-100) do you feel towards {group}s?",
  groups = c("Democrat", "Republican"),
  identity_label = "Democrat"
)
#> [1] "You are simulating an American Democrat. How warmly (0-100) do you feel towards Democrats? How warmly (0-100) do you feel towards Republicans?"

# Single-question mode (asks once, as-is):
make_baseline_prompt(
  identity_context = "You are simulating an American Democrat.",
  question_template = "How warmly (0-100) do you feel towards your outgroup?",
  groups = c("Democrat", "Republican"),
  identity_label = "Democrat"
)
#> [1] "You are simulating an American Democrat. How warmly (0-100) do you feel towards your outgroup?"
```
