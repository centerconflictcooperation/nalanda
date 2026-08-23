# Aggregate structured forecasts through an explicit weighting hierarchy

Average repeated completions within prompt, prompts within model, models
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
  family_col = "family"
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

  Optional column identifying model families. Set to `NULL` to average
  models directly into the consensus.

## Value

A named list of tibbles: `prompt`, `model`, optional `family`, and
`consensus`. Count columns make the weight at each stage auditable.

## Details

This helper only computes arithmetic means. Choosing this hierarchy,
deciding whether model outputs represent simulations or expert
forecasts, and any uncertainty analysis remain scientific decisions for
the caller.
