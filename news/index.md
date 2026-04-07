# Changelog

## nalanda 0.0.1.4

- Added shared multi-model support across the simulation pipeline so
  [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md),
  [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md),
  [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md),
  and
  [`run_ai_cumulative_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_cumulative_chapters.md)
  can accept `length(model) >= 1`.
- Added a `model` column to raw simulation outputs and updated
  downstream summaries to preserve model-specific results instead of
  collapsing across models.
- Added
  [`summarize_identity_adherence()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_identity_adherence.md)
  and
  [`summarize_identity_match_rates()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_identity_match_rates.md)
  to tally whether models adopt the requested identity and to summarize
  per-model adoption rates.

## nalanda 0.0.1.3

- Renamed
  [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md)
  output columns to use `treatment` terminology instead of `chapter`.
- Simplified
  [`summarize_treatment_results()`](https://centerconflictcooperation.github.io/nalanda/reference/summarize_treatment_results.md)
  to summarize treatment-level outputs only.
- Updated README and treatment documentation/examples to match the
  revised treatment workflow API.

## nalanda 0.0.1

- Added `BugReports` field in `DESCRIPTION` pointing to the GitHub issue
  tracker.
- Fixed README title grammar: “Do books really change lives?”.
- Replaced placeholder tests with basic offline tests for prompt
  builders and chapter summary helpers.
- Updated `summarize_chapter_scores` documentation to match current
  delta/gap output columns.
