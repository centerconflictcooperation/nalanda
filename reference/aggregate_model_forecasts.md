# Aggregate structured forecasts through an explicit weighting hierarchy

Reduce repeated completions within prompt, prompts within model, models
within family, and then families into a consensus. Each stage operates
on the already-aggregated rows from the previous stage, so unequal
completion counts do not give a model more weight. Supplying
`family_col` explicitly requests equal family weight at the final stage;
with `family_col = NULL`, the final consensus gives models equal weight.

## Usage

``` r
aggregate_model_forecasts(
  data,
  outcomes,
  unit_by,
  completion_col = "completion",
  prompt_col = "prompt_id",
  model_col = "model_id",
  family_col = "family",
  method = c("mean", "median")
)
```

## Arguments

- data:

  Raw workflow results in long row form.

- outcomes:

  Character vector naming numeric forecast columns.

- unit_by:

  Character vector of columns that uniquely identify a forecast target,
  such as a condition ID. Include every necessary unit column.

- completion_col, prompt_col, model_col:

  Column names identifying the repeated completion, prompt variant, and
  model.

- family_col:

  Optional column identifying model families. Set to `NULL` to combine
  models directly into the consensus.

- method:

  Aggregation statistic applied at every stage: `"mean"` (the
  backward-compatible default) or `"median"`.

## Value

A named list of tibbles: `prompt`, `model`, optional `family`, and
`consensus`. Count columns make the weight at each stage auditable.

## Details

`method = "mean"` preserves the original arithmetic-mean behavior.
`method = "median"` applies the median at every stage of the same
hierarchy. Missing values are removed independently for each outcome at
each stage. A stage returns `NA_real_` when all values for that outcome
and group are missing. Count columns count distinct configured
contributors regardless of outcome missingness, so they audit the
weighting structure rather than the non-missing sample size for an
individual outcome.
