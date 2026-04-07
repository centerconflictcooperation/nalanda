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
