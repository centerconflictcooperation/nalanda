# Run AI model on book chapters and collect structured responses

This function implements a two-turn sequential chat design to measure
the effect of reading book chapters on attitudes. For each simulation,
the function:

1.  Establishes a baseline by asking the model to choose a party and
    rate the outgroup

2.  Shows the chapter and asks for a post-intervention rating in the
    same chat session

This design creates a within-agent pre-post comparison, with
conversation memory maintained between turns.

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
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
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

  Character vector. Context used in the baseline prompt to establish
  party identity. Can be a vector of multiple contexts - the function
  will run once for each context and combine results. Example: "You are
  simulating an American adult who politically identifies as a
  Democrat."

- question_text:

  Character. Question to ask the model in both baseline and
  post-intervention turns. The post-intervention turn will append "
  after reading this chapter?" to this question. Example: "On a scale
  from 0 to 100, how warmly do you feel towards your political
  outgroup?"

- n_simulations:

  Integer. Number of repeated simulations per chapter (each simulation =
  2 chat turns).

- temperature:

  Numeric. Sampling temperature passed to the chat backend.

- seed:

  Integer. Random seed for reproducibility (incremented for each
  simulation).

- base_model:

  Character. Model name to label results with.

- virtual_key:

  Optional API key/virtual key prefix used by chat_portkey.

- base_url:

  Optional base url for API calls.

- excerpt_chars:

  Integer. Number of characters to keep as excerpt in results.

- include_tokens:

  Logical. Return token counts if available (summed across both turns).

- include_cost:

  Logical. Return cost info if available (summed across both turns).

## Value

A tibble of results, or a named list of tibbles (one per book). Each row
represents one simulation and includes: `chapter`, `sim`, `party`,
`baseline_score` (pre-intervention), `score` (post-intervention),
`chapter_excerpt`, `context`, and `question`. The object has class
`nalanda` and an attribute `model` with the model name.
