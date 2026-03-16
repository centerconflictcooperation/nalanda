# Toy raw turn-level AI simulation output

A synthetic turn-level dataset that mimics the raw output returned by
[`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
in per-group mode. It is intended for examples, documentation, and
testing the full workflow from raw turns to derived metrics with
[`compute_run_ai_metrics()`](https://centerconflictcooperation.github.io/nalanda/reference/compute_run_ai_metrics.md).

## Usage

``` r
toy_run_ai_turns
```

## Format

A tibble with 128 rows and 11 variables:

- book:

  Book title.

- chapter:

  Chapter identifier.

- sim:

  Simulation index.

- identity:

  Simulated respondent identity.

- party:

  Political party grouping.

- turn_index:

  Conversation turn index.

- turn_type:

  Whether the row comes from the `baseline` or `post` turn.

- target_group:

  Group being rated on that row.

- rating:

  Synthetic rating on a 0-100 scale.

- baseline_prompt:

  Stored baseline prompt preview.

- post_prompt:

  Stored post prompt preview.

## Source

Synthetic example created for package documentation.
