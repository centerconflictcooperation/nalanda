# Run AI model on book chapters and collect structured responses

This wraps model/chat utilities to run the same question against one or
more chapter texts and return a tibble (or list of tibbles by book) with
predictions.

## Usage

``` r
run_ai_on_chapters(
  book_texts,
  context_text,
  question_text,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  base_model = "gemini-2.5-flash-lite",
  virtual_key = NULL,
  base_url = NULL,
  excerpt_chars = 200,
  include_tokens = FALSE,
  include_cost = FALSE
)
```

## Arguments

- book_texts:

  A single character (one chapter) or a nested list of books -\>
  chapters as returned by
  [`read_book_texts()`](https://centerconflictcooperation.github.io/nalanda/reference/read_book_texts.md).

- context_text:

  Character vector. Context to prepend to each chapter when prompting.
  Can be a vector of multiple contexts - the function will run once for
  each context and combine results.

- question_text:

  Character. Question to ask the model.

- n_simulations:

  Integer. Number of repeated prompts/simulations per chapter.

- temperature:

  Numeric. Sampling temperature passed to the chat backend.

- seed:

  Integer. Random seed for reproducibility.

- base_model:

  Character. Model name to label results with.

- virtual_key:

  Optional API key/virtual key prefix used by chat_portkey.

- base_url:

  Optional base url for API calls.

- excerpt_chars:

  Integer. Number of characters to keep as excerpt in results.

- include_tokens:

  Logical. Return token counts if available.

- include_cost:

  Logical. Return cost info if available.

## Value

A tibble of results, or a named list of tibbles (one per book). The
object has class `nalanda` and an attribute `model` with the model name.
