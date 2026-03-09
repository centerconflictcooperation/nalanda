# Build the post-intervention (Turn 2) prompt

Constructs the prompt: chapter text + question(s). If the question
template contains `{group}`, it is expanded once per group (ingroup
first). Otherwise, the question is used as-is (single-question mode).

## Usage

``` r
make_post_prompt(chapter_text, question_template, groups, identity_label)
```

## Arguments

- chapter_text:

  Character scalar. The full chapter text.

- question_template:

  Character scalar. Optionally contains `{group}` placeholder for
  per-group expansion.

- groups:

  Character vector of all group labels.

- identity_label:

  Character scalar. The group label assigned as identity.

## Value

Character scalar prompt.

## Examples

``` r
# Per-group mode:
make_post_prompt(
  chapter_text = "This is a chapter about cooperation...",
  question_template = "How warmly (0-100) do you feel towards {group}s?",
  groups = c("Democrat", "Republican"),
  identity_label = "Democrat"
)
#> [1] "You have just read the chapter below.\n\nThis is a chapter about cooperation...\n\nYou have now just finished reading the book chapter. Now that this is done:\nHow warmly (0-100) do you feel towards Democrats? How warmly (0-100) do you feel towards Republicans?"

# Single-question mode:
make_post_prompt(
  chapter_text = "This is a chapter about cooperation...",
  question_template = "How warmly (0-100) do you feel towards your outgroup?",
  groups = c("Democrat", "Republican"),
  identity_label = "Democrat"
)
#> [1] "You have just read the chapter below.\n\nThis is a chapter about cooperation...\n\nYou have now just finished reading the book chapter. Now that this is done:\nHow warmly (0-100) do you feel towards your outgroup?"
```
