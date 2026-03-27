# Run AI model on book chapters and collect structured responses

This function implements a two-turn sequential chat design to measure
the effect of reading book chapters on attitudes. For each simulation
and each identity assignment, the function:

1.  Establishes a baseline by assigning an identity, then asking for
    ratings of each group (ingroup first, outgroup second).

2.  Shows the chapter and asks for post-intervention ratings in the same
    chat session (same ordering: ingroup first, outgroup second).

This design creates a within-agent pre-post comparison, with
conversation memory maintained between turns. Ingroup and outgroup
columns are computed post-hoc from the assigned identity and the group
labels.

## Usage

``` r
run_ai_on_chapters(
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
  excerpt_chars = 200
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
    once for each group (e.g.,
    `"You are simulating an American adult who politically identifies as a {identity}."`),
    or

  - A character vector of length equal to `length(groups)`, where each
    element is the full context for the corresponding group identity.

- question_text:

  Character scalar. A question template containing the placeholder
  `{group}`, which will be replaced with each group label. Example:
  `"On a scale from 0 to 100, how warmly do you feel towards {group}s?"`

- n_simulations:

  Integer. Number of repeated simulations per chapter per identity (each
  simulation = 2 chat turns).

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

## Value

A tibble of raw turn-level ratings, or a named list of tibbles (one per
book). Each row is one rating observation and includes: `chapter`,
`sim`, `identity`, `turn_index`, `turn_type`, `target_group`, and
`rating`, plus prompt and metadata columns. Use
[`compute_run_ai_metrics()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics.md)
to derive ingroup/outgroup summaries and gap/delta metrics. The object
has class `nalanda` and model attributes.

## Details

Authentication uses `PORTKEY_API_KEY` via
[`ellmer::chat_portkey()`](https://ellmer.tidyverse.org/reference/chat_portkey.html).
Set it persistently in `.Renviron`:

    usethis::edit_r_environ()
    # Add a line like:
    # PORTKEY_API_KEY=your_api_key_here

Then restart your R session.

## Examples

``` r
# Per-group mode (asks about each group, ingroup first):
make_baseline_prompt(
  identity_context = "You are simulating an American Democrat.",
  question_template = "How warmly do you feel towards {group}s?",
  groups = c("Democrat", "Republican"),
  identity_label = "Democrat"
)
#> [1] "You are simulating an American Democrat. How warmly do you feel towards Democrats? How warmly do you feel towards Republicans?"

# Single-question mode (asks once, as-is):
make_baseline_prompt(
  identity_context = "You are simulating an American Democrat.",
  question_template = "How warmly do you feel towards your political outgroup?",
  groups = c("Democrat", "Republican"),
  identity_label = "Democrat"
)
#> [1] "You are simulating an American Democrat. How warmly do you feel towards your political outgroup?"
```
