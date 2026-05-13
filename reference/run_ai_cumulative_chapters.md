# Run AI model on books with cumulative chapter context

This function implements a cumulative multi-turn design where each
simulation creates one persistent chat per book and identity. The chat
first establishes a baseline, then processes chapters sequentially in
order, one turn per chapter, preserving context across the full book.

## Usage

``` r
run_ai_cumulative_chapters(
  book_texts,
  groups,
  context_text,
  question_text,
  output_mode = c("structured", "text"),
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  checkpoint_dir = NULL,
  checkpoint_prefix = "run_ai_cumulative_chapters",
  save_dir = NULL,
  save_prefix = "results"
)
```

## Arguments

- book_texts:

  A nested list of books -\> chapters as returned by
  [`read_book_texts()`](https://centerconflictcooperation.github.io/nalanda/reference/read_book_texts.md).

- groups:

  Character vector of group labels (length \>= 2).

- context_text:

  Character. Either a scalar template containing `{identity}` or a
  character vector of length `length(groups)`.

- question_text:

  Character scalar. A question template containing the placeholder
  `{group}`, which will be replaced with each group label.

- output_mode:

  Character. `"structured"` (default) uses the backend's
  structured-output support. `"text"` is a compatibility mode for models
  that do not support structured outputs (for example some Anthropic
  models): nalanda appends strict JSON-only instructions to the prompt,
  calls the model as free text, then parses the JSON back into the same
  fields used by the rest of the pipeline. Text mode is best-effort and
  stores the original model reply in `raw_response`.

- n_simulations:

  Integer. Number of repeated simulations per book per identity.

- temperature:

  Numeric. Sampling temperature passed to the chat backend.

- seed:

  Integer. Random seed for reproducibility (incremented for each
  simulation).

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

  Integer. Number of chapter characters to retain in the stored prompt
  previews shown in results.

- checkpoint_dir:

  Optional directory. If supplied, each completed
  book/identity/simulation conversation is saved as its own `.Rds` file
  as soon as it finishes. If the same call is rerun with the same
  `checkpoint_dir`, `checkpoint_prefix`, model, books, groups, and
  simulations, completed conversations are loaded from disk and skipped.

- checkpoint_prefix:

  Character scalar used at the start of checkpoint filenames when
  `checkpoint_dir` is supplied.

- save_dir:

  Optional directory. If supplied, each book is saved as one `.Rds` file
  as soon as all of its identities and simulations finish.

- save_prefix:

  Character scalar used in book-level filenames when `save_dir` is
  supplied. Files are named `{save_prefix}_{book}.Rds`.

## Value

A tibble or named list of tibbles with cumulative turn-level rows. The
baseline turn is followed by one post turn per chapter, all within the
same chat per book/identity/simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
raw_cumulative <- run_ai_cumulative_chapters(
  book_texts = list(
    "Book A" = list(
      chapter_1 = "A first chapter about cooperation.",
      chapter_2 = "A second chapter about conflict and repair."
    )
  ),
  groups = c("Democrat", "Republican"),
  context_text = "You are simulating an American adult who politically identifies as a {identity}.",
  question_text = "On a scale from 0 to 100, how warmly do you feel towards {group}s?",
  n_simulations = 1,
  temperature = 0,
  seed = 42
)

compute_run_ai_metrics_cumulative(raw_cumulative)
} # }
```
