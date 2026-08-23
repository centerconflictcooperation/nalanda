# nalanda 0.0.2.1

- Restored compatibility with `ellmer` 0.4.0 by passing structured prompt
  batches as lists in `run_text_analysis()` and
  `run_ai_on_chapters_one_turn()`.
- Fixed the `nalanda` class order on `run_text_analysis()` results so current
  `dplyr` and `vctrs` operations work without a downstream class workaround.

# nalanda 0.0.2.0

- Added `repair_run_ai_on_chapters()` to rerun only failed or fully missing
  simulation units and return a merged result without overwriting the source.
- Fixed `repair_run_ai_on_chapters()` model selection so fully-qualified
  Portkey names match their normalized model labels in saved results.
- `repair_run_ai_on_chapters()` now retains Azure content-policy failures
  without retrying their unchanged prompts, while continuing to repair
  transient failures in the same result.
- Added early validation for `gpt-5-mini`, which only supports `temperature = 1`, so invalid simulation configurations fail before API calls are made.
- Added fail-fast handling for unrecoverable model/integration route mismatches and unreachable gateway errors, so `run_ai_on_chapters(on_error = "skip")` no longer repeats the same backend or network failure across every simulation unit.
- Added `split_book_section_by_headings()` to split oversized section text files into chapter-level files using known chapter headings.
- Added `renumber_chapters_across_folders()` to renumber chapter files across ordered folders while preserving title slugs.

# nalanda 0.0.1.4

- Added shared multi-model support across the simulation pipeline so `simulate_treatment()`, `run_ai_on_chapters()`, `run_ai_on_chapters_one_turn()`, and `run_ai_cumulative_chapters()` can accept `length(model) >= 1`.
- Added a `model` column to raw simulation outputs and updated downstream summaries to preserve model-specific results instead of collapsing across models.
- Added `summarize_identity_adherence()` and `summarize_identity_match_rates()` to tally whether models adopt the requested identity and to summarize per-model adoption rates.

# nalanda 0.0.1.3

- Renamed `simulate_treatment()` output columns to use `treatment` terminology instead of `chapter`.
- Simplified `summarize_treatment_results()` to summarize treatment-level outputs only.
- Updated README and treatment documentation/examples to match the revised treatment workflow API.

# nalanda 0.0.1

- Added `BugReports` field in `DESCRIPTION` pointing to the GitHub issue tracker.
- Fixed README title grammar: "Do books really change lives?".
- Replaced placeholder tests with basic offline tests for prompt builders and chapter summary helpers.
- Updated `summarize_chapter_scores` documentation to match current delta/gap output columns.
