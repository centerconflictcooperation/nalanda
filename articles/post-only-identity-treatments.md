# Post-Only Identity Treatments with \`simulate_treatment()\`

## Purpose

This vignette shows a short workflow for collaborators who currently use
[`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
but only keep the post-treatment responses.

For that use case,
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
is usually simpler:

1.  You define the prompt directly.
2.  You can still use `{identity}` and `{intervention_text}`
    placeholders.
3.  You get one row per simulation turn, without pre/post delta columns
    that you do not need.

The example below compares `American` and `Brazilian` identities in a
post-only design.

## 1. Set package options

As in other `nalanda` workflows, it is easiest to set the model routing
once at the start of the script.

``` r
library(nalanda)

options(
  nalanda.integration = "vertexai",
  nalanda.base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/"
)
```

## 2. Define the treatment text and prompt

The intervention text can still be passed separately and inserted into
the prompt with `{intervention_text}`. This is useful when your script
generates many treatment variants programmatically.

``` r
intervention_text <- "Imagine you come across a short community message about 
climate action. It explains that small everyday choices can add up when many 
people participate, and it encourages residents to back clean-energy policies, 
talk with others about practical climate solutions, and choose lower-carbon 
habits when possible. The message ends by saying that collective action now 
can help protect future generations."

prompt <- "{intervention_text}

On a scale from 0 to 100, how accurate do you think this statement is?
Statement: Climate change is a global emergency

On a scale from 0 to 100, how much do you support public policies that reduce greenhouse gas emissions?"
```

Here:

1.  `{identity}` will be filled from `groups` via `context_text`.
2.  `{intervention_text}` will be filled from the function argument.

## 3. Preview the prompt before running

If you like to inspect prompt text before launching a run, you can build
a single concrete prompt from the same elements you will pass to
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md).

``` r
groups <- c("American", "Brazilian")
context_text <- "You are simulating an adult who identifies as {identity}."

prompt_preview <- make_treatment_prompt(
  prompt_template = prompt,
  intervention_text = intervention_text,
  identity_context = gsub("{identity}", groups[1], context_text, fixed = TRUE),
  identity_label = groups[1]
)

cat(prompt_preview)
#> You are simulating an adult who identifies as American. Imagine you come across a short community message about 
#> climate action. It explains that small everyday choices can add up when many 
#> people participate, and it encourages residents to back clean-energy policies, 
#> talk with others about practical climate solutions, and choose lower-carbon 
#> habits when possible. The message ends by saying that collective action now 
#> can help protect future generations.
#> 
#> On a scale from 0 to 100, how accurate do you think this statement is?
#> Statement: Climate change is a global emergency
#> 
#> On a scale from 0 to 100, how much do you support public policies that reduce greenhouse gas emissions?
```

This plays the same role as
[`make_baseline_prompt()`](https://centerconflictcooperation.github.io/nalanda/reference/make_baseline_prompt.md)
or
[`make_post_prompt()`](https://centerconflictcooperation.github.io/nalanda/reference/make_post_prompt.md),
but for the one-turn
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
workflow.

## 4. Run the post-only simulation

``` r
res <- simulate_treatment(
  intervention_text = intervention_text,
  groups = groups,
  context_text = context_text,
  prompt = prompt,
  response_type = ellmer::type_object(
    climate_belief = ellmer::type_number(),
    policy_support = ellmer::type_number()
  ),
  n_simulations = 2,
  temperature = 0,
  model = "gemini-2.5-flash-lite"
)
```

This is the key difference from
[`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md):

1.  there is no baseline turn,
2.  there are no `pre_*` or `delta_*` columns,
3.  the output matches the post-only design directly.

If you want a multi-turn design later,
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
can also take a prompt vector of length greater than 1, with one element
per turn.

## 5. Inspect the raw output

Each row is one simulated response for one identity and one simulation
draw.

| chapter        | sim | identity  | turn_index | turn_type | climate_belief | policy_support |
|:---------------|----:|:----------|-----------:|:----------|---------------:|---------------:|
| intervention_1 |   1 | American  |          1 | turn_1    |             84 |             71 |
| intervention_1 |   2 | American  |          1 | turn_1    |             82 |             69 |
| intervention_1 |   1 | Brazilian |          1 | turn_1    |             76 |             62 |
| intervention_1 |   2 | Brazilian |          1 | turn_1    |             74 |             60 |

## 6. Summarize by identity

Use
[`summarize_treatment_results()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_treatment_results.md)
to compute mean scores by identity.

``` r
summary_by_identity <- summarize_treatment_results(
  res,
  by_identity = TRUE
)

summary_by_identity
```

| ch             | id        | turn   |   n | m_climate_belief | sd_climate_belief | m_policy_support | sd_policy_support | idx |
|:---------------|:----------|:-------|----:|-----------------:|------------------:|-----------------:|------------------:|----:|
| intervention_1 | American  | turn_1 |   2 |               83 |               1.4 |               70 |               1.4 |   1 |
| intervention_1 | Brazilian | turn_1 |   2 |               75 |               1.4 |               61 |               1.4 |   1 |

## 7. Compare another group to Americans

If `American` is your reference group, a simple downstream comparison
is:

``` r
library(dplyr)
library(tidyr)

comparison_vs_american <- summary_by_identity |>
  select(
    identity,
    mean_climate_belief,
    mean_policy_support
  ) |>
  pivot_wider(
    names_from = identity,
    values_from = c(mean_climate_belief, mean_policy_support)
  ) |>
  mutate(
    climate_belief_gap_vs_american =
      mean_climate_belief_Brazilian - mean_climate_belief_American,
    policy_support_gap_vs_american =
      mean_policy_support_Brazilian - mean_policy_support_American
  )

comparison_vs_american
```

| climate_us | climate_br | policy_us | policy_br | gap_climate | gap_policy |
|-----------:|-----------:|----------:|----------:|------------:|-----------:|
|         83 |         75 |        70 |        61 |          -8 |         -9 |

## When to use which function

Use
[`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
when you want a true pre/post design and plan to use within-agent change
scores such as `delta_outgroup` or `delta_gap`.

Use
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
when:

1.  the design is post-only,
2.  the treatment is not really a book chapter,
3.  you want direct control over the prompt wording, and
4.  you want to insert `{intervention_text}` programmatically.
