# Summarize simulated chapter scores

Aggregate simulation results by chapter (and book, if present) computing
number of simulations, means and SDs for core model outputs. In the
current schema, this includes ingroup/outgroup pre-post ratings plus
delta and gap metrics (for example delta_outgroup, delta_ingroup, and
delta_gap).

## Usage

``` r
summarize_chapter_scores(
  x,
  aggregate_level = c("chapter", "book"),
  by_party = FALSE
)
```

## Arguments

- x:

  A data frame or list-like object containing simulation rows as
  produced by run_ai_on_chapters(). Expected columns include chapter,
  pre/post ingroup-outgroup fields, and the derived difference columns
  used in summaries (pre_gap, post_gap, delta_outgroup, delta_ingroup,
  delta_gap). If book and party are present, the summary will include
  those groupings.

- aggregate_level:

  Character. One of "chapter" (default) or "book". When "book", results
  are aggregated to the book level.

- by_party:

  Logical. If TRUE, summaries are computed separately by party (if
  present).

## Value

A tibble summarizing each chapter (and book if present). The returned
object will have the original model attribute copied to it.

## Examples

``` r
chapter_summary <- summarize_chapter_scores(toy_sim_results)
chapter_summary
#> # A tibble: 8 × 22
#>   book           chapter   sim mean_pre_ingroup sd_pre_ingroup mean_post_ingroup
#>   <chr>          <chr>   <int>            <dbl>          <dbl>             <dbl>
#> 1 Bridge Stories chapte…     4             80.5           2.38                82
#> 2 Bridge Stories chapte…     4             80.5           2.38                82
#> 3 Bridge Stories chapte…     4             80.5           2.38                82
#> 4 Bridge Stories chapte…     4             80.5           2.38                82
#> 5 Common Ground  chapte…     4             79.5           2.38                81
#> 6 Common Ground  chapte…     4             79.5           2.38                81
#> 7 Common Ground  chapte…     4             79.5           2.38                81
#> 8 Common Ground  chapte…     4             79.5           2.38                81
#> # ℹ 16 more variables: sd_post_ingroup <dbl>, mean_pre_outgroup <dbl>,
#> #   sd_pre_outgroup <dbl>, mean_post_outgroup <dbl>, sd_post_outgroup <dbl>,
#> #   mean_pre_gap <dbl>, sd_pre_gap <dbl>, mean_post_gap <dbl>,
#> #   sd_post_gap <dbl>, mean_delta_outgroup <dbl>, sd_delta_outgroup <dbl>,
#> #   mean_delta_ingroup <dbl>, sd_delta_ingroup <dbl>, mean_delta_gap <dbl>,
#> #   sd_delta_gap <dbl>, chapter_index <int>

book_summary <- summarize_chapter_scores(
  toy_sim_results,
  aggregate_level = "book"
)
book_summary
#> # A tibble: 2 × 20
#>   book     sim mean_pre_ingroup sd_pre_ingroup mean_post_ingroup sd_post_ingroup
#>   <chr>  <int>            <dbl>          <dbl>             <dbl>           <dbl>
#> 1 Bridg…    16             80.5           2.13                82            2.19
#> 2 Commo…    16             79.5           2.13                81            2.19
#> # ℹ 14 more variables: mean_pre_outgroup <dbl>, sd_pre_outgroup <dbl>,
#> #   mean_post_outgroup <dbl>, sd_post_outgroup <dbl>, mean_pre_gap <dbl>,
#> #   sd_pre_gap <dbl>, mean_post_gap <dbl>, sd_post_gap <dbl>,
#> #   mean_delta_outgroup <dbl>, sd_delta_outgroup <dbl>,
#> #   mean_delta_ingroup <dbl>, sd_delta_ingroup <dbl>, mean_delta_gap <dbl>,
#> #   sd_delta_gap <dbl>

party_summary <- summarize_chapter_scores(
  toy_sim_results,
  by_party = TRUE
)
head(party_summary)
#> # A tibble: 6 × 23
#>   book     chapter party   sim mean_pre_ingroup sd_pre_ingroup mean_post_ingroup
#>   <chr>    <chr>   <chr> <int>            <dbl>          <dbl>             <dbl>
#> 1 Bridge … chapte… Demo…     2             78.5          0.707              79.5
#> 2 Bridge … chapte… Repu…     2             82.5          0.707              84.5
#> 3 Bridge … chapte… Demo…     2             78.5          0.707              79.5
#> 4 Bridge … chapte… Repu…     2             82.5          0.707              84.5
#> 5 Bridge … chapte… Demo…     2             78.5          0.707              80.5
#> 6 Bridge … chapte… Repu…     2             82.5          0.707              83.5
#> # ℹ 16 more variables: sd_post_ingroup <dbl>, mean_pre_outgroup <dbl>,
#> #   sd_pre_outgroup <dbl>, mean_post_outgroup <dbl>, sd_post_outgroup <dbl>,
#> #   mean_pre_gap <dbl>, sd_pre_gap <dbl>, mean_post_gap <dbl>,
#> #   sd_post_gap <dbl>, mean_delta_outgroup <dbl>, sd_delta_outgroup <dbl>,
#> #   mean_delta_ingroup <dbl>, sd_delta_ingroup <dbl>, mean_delta_gap <dbl>,
#> #   sd_delta_gap <dbl>, chapter_index <int>
```
