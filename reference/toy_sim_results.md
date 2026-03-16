# Toy simulated chapter results

A small synthetic dataset that mimics the row-level structure returned
by
[`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md).
It is intended for examples, documentation, and testing plotting and
summarization workflows without running live AI simulations. The object
also includes `model` and `temperature` attributes so plotting helpers
can display realistic metadata in subtitles.

## Usage

``` r
toy_sim_results
```

## Format

A tibble with 32 rows and 15 variables:

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

- pre_ingroup:

  Pre-reading ingroup rating.

- post_ingroup:

  Post-reading ingroup rating.

- pre_outgroup:

  Pre-reading outgroup rating.

- post_outgroup:

  Post-reading outgroup rating.

- pre_gap:

  Pre-reading ingroup minus outgroup gap.

- post_gap:

  Post-reading ingroup minus outgroup gap.

- delta_ingroup:

  Change in ingroup rating.

- delta_outgroup:

  Change in outgroup rating.

- delta_gap:

  Change in affective polarization gap.

- chapter_excerpt:

  Short synthetic chapter excerpt.

## Source

Synthetic example created for package documentation.
