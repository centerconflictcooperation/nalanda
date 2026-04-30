# Aggregate simulation runs

Computes the mean and standard deviation of an outcome across simulation
replicates within each model-by-unit cell. This is the recommended first
step before computing inter-model agreement: it collapses intra-model
sampling noise so that downstream metrics reflect genuine model
differences rather than Monte Carlo variance.

## Usage

``` r
aggregate_simulations(
  data,
  outcome = "outcome",
  by = c("model", "book_id", "chapter_id", "group")
)
```

## Arguments

- data:

  A data frame with one row per simulation run, containing columns for
  model identity, unit identifiers, and the outcome variable.

- outcome:

  Character string naming the outcome column to aggregate (default
  `"outcome"`).

- by:

  Character vector of column names to group by. Must include a column
  identifying the model (typically `"model"`). Default
  `c("model", "book_id", "chapter_id", "group")`.

## Value

A tibble with one row per unique combination of `by`, plus:

- mean\_{outcome}:

  Mean of `outcome` across simulation runs.

- sd\_{outcome}:

  Standard deviation across runs.

- n_sims:

  Number of simulation replicates in the cell.

## Examples

``` r
sim_data <- data.frame(
  model = rep(c("gpt-4o", "gemini-2.5-flash"), each = 40),
  book_id = rep("BookA", 80),
  chapter_id = rep(paste0("ch", 1:4), each = 10, times = 2),
  group = rep(c("Democrat", "Republican"), 40),
  sim = rep(1:10, 8),
  rating = rnorm(80, 60, 10)
)
aggregate_simulations(sim_data, outcome = "rating",
  by = c("model", "book_id", "chapter_id", "group"))
#> # A tibble: 16 × 7
#>    model            book_id chapter_id group      mean_rating sd_rating n_sims
#>    <chr>            <chr>   <chr>      <chr>            <dbl>     <dbl>  <int>
#>  1 gemini-2.5-flash BookA   ch1        Democrat          61.3      1.88      5
#>  2 gemini-2.5-flash BookA   ch1        Republican        61.1     17.3       5
#>  3 gemini-2.5-flash BookA   ch2        Democrat          56.1     11.5       5
#>  4 gemini-2.5-flash BookA   ch2        Republican        66.4     11.8       5
#>  5 gemini-2.5-flash BookA   ch3        Democrat          65.1      9.80      5
#>  6 gemini-2.5-flash BookA   ch3        Republican        53.1      7.02      5
#>  7 gemini-2.5-flash BookA   ch4        Democrat          63.9     11.5       5
#>  8 gemini-2.5-flash BookA   ch4        Republican        61.7      2.69      5
#>  9 gpt-4o           BookA   ch1        Democrat          49.4     12.3       5
#> 10 gpt-4o           BookA   ch1        Republican        61.7      5.86      5
#> 11 gpt-4o           BookA   ch2        Democrat          64.1     10.7       5
#> 12 gpt-4o           BookA   ch2        Republican        52.3     10.5       5
#> 13 gpt-4o           BookA   ch3        Democrat          58.6     13.2       5
#> 14 gpt-4o           BookA   ch3        Republican        58.9      8.54      5
#> 15 gpt-4o           BookA   ch4        Democrat          58.1     10.6       5
#> 16 gpt-4o           BookA   ch4        Republican        64.9      8.21      5
```
