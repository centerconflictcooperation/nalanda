# Prompt Grids for Multi-Model Structured Forecasting

## Purpose

Structured forecasting projects often cross several dimensions at once:

- many input rows,
- several prompt variants,
- several models or routes, and
- different numbers of repeated completions for different models.

Hand-written nested loops can run this design, but they also make it
easy to lose provenance, miscount calls, overwrite raw output, or
accidentally give a model more weight merely because it has more
completions. This vignette shows the neutral prompt-grid orchestration
provided by `nalanda`.

All executed code in this vignette is local and no-cost. Live model
calls are shown with `eval = FALSE`.

[`run_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/run_prompt_grid.md)
treats prompt variants as **independent alternatives**: each variant
starts a fresh conversation. This differs from
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md),
where the elements of `prompt` are **ordered turns** in one
conversation. Use
[`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
when conversational memory is part of the treatment design; use a prompt
grid when prompts are parallel variants that must not influence one
another.

## Inputs and configurations

Start with one row per forecast target. Any columns can be interpolated
into a prompt template.

``` r

library(nalanda)

interventions <- tibble::tibble(
  condition_id = c("control", "cooperation", "perspective"),
  text = c(
    "A neutral informational message.",
    "A message emphasizing a shared cooperative goal.",
    "A message asking the reader to consider another person's perspective."
  )
)

prompts <- tibble::tibble(
  prompt_id = c("direct", "contextual"),
  prompt = c(
    "Forecast the effects of this intervention:\n{text}",
    paste(
      "Consider the likely study population and implementation context.",
      "Then forecast the effects of this intervention:\n{text}"
    )
  ),
  active = c(TRUE, TRUE)
)

models <- tibble::tibble(
  model_id = c("fast_a", "reasoning_a", "fast_b", "candidate_off"),
  model = c("model-a-fast", "model-a-reasoning", "model-b-fast", "model-c"),
  integration = c("route-a", "route-a", "route-b", "route-c"),
  family = c("developer-a", "developer-a", "developer-b", "developer-c"),
  temperature = c(0, 0, 0.3, 0),
  n_completions = c(2L, 1L, 3L, 5L),
  active = c(TRUE, TRUE, TRUE, FALSE)
)
```

`model_id` is a stable analysis label, while `model` and `integration`
specify the backend route. `family` is an analysis grouping; it need not
be identical to a provider name. Inactive rows remain in the
configuration for provenance but are not planned or run.

## Preview before spending

Use
[`plan_prompt_grid()`](https://centerconflictcooperation.github.io/nalanda/reference/plan_prompt_grid.md)
before constructing a response schema or contacting a backend.

``` r

smoke_plan <- plan_prompt_grid(
  data = interventions,
  prompt_variants = prompts,
  model_config = models,
  smoke_n = 1
)

smoke_plan[, c(
  "model_id", "family", "prompt_id", "n_rows",
  "n_completions", "estimated_calls"
)]
#> # A tibble: 6 × 6
#>   model_id    family      prompt_id  n_rows n_completions estimated_calls
#>   <chr>       <chr>       <chr>       <int>         <int>           <int>
#> 1 fast_a      developer-a direct          1             2               2
#> 2 fast_a      developer-a contextual      1             2               2
#> 3 reasoning_a developer-a direct          1             1               1
#> 4 reasoning_a developer-a contextual      1             1               1
#> 5 fast_b      developer-b direct          1             3               3
#> 6 fast_b      developer-b contextual      1             3               3
sum(smoke_plan$estimated_calls)
#> [1] 12
```

Here the smoke run uses one input row. The full plan is the same call
with `smoke_n = NULL`.

``` r

full_plan <- plan_prompt_grid(
  data = interventions,
  prompt_variants = prompts,
  model_config = models
)

sum(full_plan$estimated_calls)
#> [1] 36
```

`estimated_calls` counts requested model responses: input rows
multiplied by model-specific completions. A backend may submit those
responses concurrently or in batches, but the response count is the
useful quantity for budgeting. Calling
`run_prompt_grid(..., dry_run = TRUE)` returns this same plan.

## Define a structured forecast

The response type can contain any number of fields. For a 13-outcome
benchmark, one concise construction is:

``` r

outcome_names <- sprintf("effect_%02d", 1:13)
response_type <- do.call(
  ellmer::type_object,
  stats::setNames(
    replicate(13, ellmer::type_number(), simplify = FALSE),
    outcome_names
  )
)
```

Use meaningful field names in a real study. The schema should describe
the scale and direction of each forecast clearly enough that every model
returns comparable numbers.

## Smoke, then run and resume

The live smoke call is:

``` r

smoke <- run_prompt_grid(
  data = interventions,
  id_col = "condition_id",
  prompt_variants = prompts,
  response_type = response_type,
  model_config = models,
  smoke_n = 1,
  output_dir = "results/forecast-smoke",
  resume = TRUE,
  on_error = "continue"
)
```

After inspecting `smoke$results` and `smoke$errors`, remove `smoke_n`
and use a separate directory for the full run.

``` r

full <- run_prompt_grid(
  data = interventions,
  id_col = "condition_id",
  prompt_variants = prompts,
  response_type = response_type,
  model_config = models,
  output_dir = "results/forecast-full",
  resume = TRUE,
  on_error = "continue"
)

raw_forecasts <- full$results
full$tasks
full$errors
```

Each successful model-prompt-completion unit is written to its own RDS
file before the workflow advances. The filename contains readable IDs
and a hash of the inputs and settings. With `resume = TRUE`, a
compatible file is loaded instead of rerun. Failed units are listed in
`errors` and are deliberately not checkpointed, so a later run retries
them.

The combined raw table retains the input metadata and adds `model_id`,
`model`, `family`, `prompt_id`, `prompt_template`, `completion`,
`temperature`, `seed`, `integration`, and `output_mode`. Keep both this
combined table and the per-completion files. Aggregation should never be
the only saved artifact.

## Simulations or repeated expert forecasts?

The mechanics are identical, but the interpretation is not.

Repeated completions are **simulations** when the prompt and design
treat each draw as a synthetic respondent or stochastic realization from
a target population. The repetition count then describes simulated
sample size, and the prompt should define the simulated unit.

Repeated completions are **repeated expert forecasts** when the model is
being used as a forecaster and repeated sampling measures its
within-model variability. Those draws are not independent experts.
Calling them separate experts would overstate the number and diversity
of information sources.

Temperature zero does not guarantee identical output across every hosted
backend, and a nonzero temperature does not by itself turn completions
into valid population simulations. That claim depends on the research
design.

## Aggregate without accidental row-count weights

The raw mean is usually wrong when models have different completion
counts or belong to families with different numbers of models. The
helper below returns every stage rather than hiding a final scientific
choice.

This small local table mimics one outcome from a completed run:

``` r

mock_raw <- tibble::tibble(
  condition_id = "cooperation",
  family = c("developer-a", "developer-a", "developer-a", "developer-a", "developer-b"),
  model_id = c("fast_a", "fast_a", "fast_a", "reasoning_a", "fast_b"),
  prompt_id = c("direct", "direct", "contextual", "direct", "direct"),
  completion = c(1L, 2L, 1L, 1L, 1L),
  effect_support = c(0, 2, 3, 6, 10),
  effect_trust = c(2, 4, 5, 7, 9)
)

aggregated <- aggregate_model_forecasts(
  mock_raw,
  outcomes = c("effect_support", "effect_trust"),
  unit_by = "condition_id",
  family_col = "family"
)

aggregated$prompt
#> # A tibble: 4 × 7
#>   condition_id family      model_id    prompt_id  effect_support effect_trust
#>   <chr>        <chr>       <chr>       <chr>               <dbl>        <dbl>
#> 1 cooperation  developer-a fast_a      contextual              3            5
#> 2 cooperation  developer-a fast_a      direct                  1            3
#> 3 cooperation  developer-a reasoning_a direct                  6            7
#> 4 cooperation  developer-b fast_b      direct                 10            9
#> # ℹ 1 more variable: n_completions <int>
aggregated$model
#> # A tibble: 3 × 6
#>   condition_id family      model_id    effect_support effect_trust n_prompts
#>   <chr>        <chr>       <chr>                <dbl>        <dbl>     <int>
#> 1 cooperation  developer-a fast_a                   2            4         2
#> 2 cooperation  developer-a reasoning_a              6            7         1
#> 3 cooperation  developer-b fast_b                  10            9         1
aggregated$family
#> # A tibble: 2 × 5
#>   condition_id family      effect_support effect_trust n_models
#>   <chr>        <chr>                <dbl>        <dbl>    <int>
#> 1 cooperation  developer-a              4          5.5        2
#> 2 cooperation  developer-b             10          9          1
aggregated$consensus
#> # A tibble: 1 × 4
#>   condition_id effect_support effect_trust n_families
#>   <chr>                 <dbl>        <dbl>      <int>
#> 1 cooperation               7         7.25          2
```

The hierarchy is explicit:

1.  completions receive equal weight within a prompt,
2.  prompt estimates receive equal weight within a model,
3.  model estimates receive equal weight within a family, and
4.  family estimates receive equal weight in the consensus.

Set `family_col = NULL` if the intended estimand gives every model equal
weight directly. The count columns (`n_completions`, `n_prompts`,
`n_models`, and `n_families`) make each stage auditable. A raw mean
would instead give extra weight to whichever model produced more rows.

## What remains downstream

`nalanda` handles configuration validation, expansion, budgeting,
provenance, checkpointing, resume, and arithmetic aggregation. The
following choices stay in downstream analysis because they depend on the
study:

- which models and prompt variants belong in the estimand,
- whether a `family` groups developers, model lineages, or another
  dependence structure,
- calibration, transformations, bounds, and missing-data rules for the
  13 outcomes,
- uncertainty intervals and dependence-aware inference,
- robustness and sensitivity analyses across prompts, models, or
  families, and
- whether repeated completions can be interpreted as simulations at all.

That separation keeps the package API reusable without silently turning
one benchmark’s scientific assumptions into defaults for every project.
