# Run AI model on book chapters with a single prompt per simulation

This function implements a one-turn design where identity context,
chapter text, and the rating question are combined into a single prompt.
Independent prompts are executed in parallel with
[`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html).

## Usage

``` r
run_ai_on_chapters_one_turn(
  book_texts,
  groups,
  context_text,
  question_text,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  include_tokens = FALSE,
  include_cost = FALSE,
  max_active = 10,
  rpm = 500
)
```

## Arguments

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
    once for each group.

  - A character vector of length equal to `length(groups)`, where each
    element is the full context for the corresponding group identity.

- question_text:

  Character scalar. A question template containing the placeholder
  `{group}`, which will be replaced with each group label in per-group
  mode.

- n_simulations:

  Integer. Number of repeated simulations per chapter per identity.

- temperature:

  Numeric. Sampling temperature passed to the chat backend.

- seed:

  Integer. Random seed for reproducibility. As in
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md),
  the seed varies by simulation index only, so all chapters and
  identities within the same `sim` share the same seed.

- model:

  Character. Model name for the chat backend.

- integration:

  Optional integration/provider slug. If supplied and `model` is not
  fully-qualified, nalanda will build `"@{integration}/{model}"`.
  Preferred for new Portkey/NYU setups.

- virtual_key:

  Optional legacy virtual key. If supplied and `model` is not
  fully-qualified, nalanda will build `"@{virtual_key}/{model}"`. Use
  either `integration` or `virtual_key`, not both.

- base_url:

  Character. Base URL for API calls.

- excerpt_chars:

  Integer. Number of chapter characters to retain in the stored prompt
  preview shown in results.

- include_tokens:

  Logical. Return token counts if available.

- include_cost:

  Logical. Return cost info if available.

- max_active:

  Integer. Maximum number of concurrent requests passed to
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html).

- rpm:

  Integer. Requests-per-minute cap passed to
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html).

## Value

A tibble of raw single-turn ratings, or a named list of tibbles (one per
book). Each row is one rating observation and includes `chapter`, `sim`,
`identity`, `turn_index`, `turn_type`, `target_group`, and `rating`,
plus prompt and metadata columns. Use
[`compute_run_ai_metrics_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics_one_turn.md)
to derive chapter-level one-turn summaries.

## Examples

``` r
if (FALSE) { # \dontrun{
run_ai_on_chapters_one_turn(
  book_texts = list(
    "Toy Book" = list(
      chapter1 = toy_sim_results$chapter_excerpt[[1]]
    )
  ),
  groups = c("Democrat", "Republican"),
  context_text = "You are simulating an American adult who identifies as a {identity}.",
  question_text = "On a 0 to 100 scale, how warmly do you feel towards {group}s?",
  n_simulations = 1
)
} # }
```
