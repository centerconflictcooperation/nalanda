# Convert long survey turns to one row per simulated respondent

Convert long survey turns to one row per simulated respondent

## Usage

``` r
survey_responses_wide(results, participant_id = "participant_id")
```

## Arguments

- results:

  Turn-level results returned by
  [`run_survey_simulation()`](https://centerconflictcooperation.github.io/nalanda/reference/plan_survey_simulation.md).

- participant_id:

  Participant identifier column.
