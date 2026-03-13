# Getting Started with Ellmer and Nalanda

This vignette provides a minimal, reliable path to get started:

1.  First verify your basic LLM setup with a small `ellmer` example.
2.  Then run a minimal `nalanda` example.

## Step 1: Verify your connection with `ellmer`

Install and load:

``` r
install.packages("ellmer")
library(ellmer)
```

Set your key in `.Renviron` (recommended):

``` r
usethis::edit_r_environ()
# Add:
# PORTKEY_API_KEY=your_real_key_here
```

Restart R after editing `.Renviron`.

**NYU users:** If you are affiliated with NYU, please contact
<Genai-research-support@nyu.edu> to obtain your NYU Portkey API key.

Run a small smoke test directly with `ellmer`:

``` r
library(ellmer)

virtual_key = "gemini-8c2498" # portkey model choice
model <- "gemini-2.5-flash-lite"
model_string = paste0("@", virtual_key, "/", model)

chat <- chat_portkey(
  model = model_string,
  base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/"
)

chat$chat("Tell me one short joke about books.")

## Why did the book go to therapy?
## 
## Because it had too many issues!
```

If this call returns successfully, your base configuration is working.

## Step 2: Run a minimal `nalanda` workflow

Once `ellmer` works,
[`nalanda::run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md)
uses the same setup and returns structured outputs for chapter
simulations.

We also set package options at the beginning of a script so we do not
need to repeat the same arguments in each call.

``` r
library(nalanda)

options(
  nalanda.model_provider = "gemini-8c2498",
  nalanda.base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/"
)

demo_chapter <- "A short chapter about people from different groups cooperating."

res <- run_ai_on_chapters(
  book_texts = demo_chapter,
  groups = c("Democrat", "Republican"),
  context_text = "You are simulating an American adult who politically identifies as a {identity}.",
  question_text = "On a scale from 0 to 100, how warmly do you feel towards {group}s?",
  n_simulations = 1,
  temperature = 0,
  model = "gemini-2.5-flash-lite"
)

res
## # A tibble: 2 × 20
##   chapter     sim identity party baseline_prompt post_prompt pre_rating_democrat post_rating_democrat pre_rating_republican
##   <chr>     <int> <chr>    <chr> <chr>           <chr>                     <int>                <int>                 <int>
## 1 chapter_1     1 Democrat Demo… You are simula… "You have …                  85                   85                    20
## 2 chapter_1     1 Republi… Repu… You are simula… "You have …                  25                   40                    85
## # ℹ 11 more variables: post_rating_republican <int>, pre_ingroup <int>, post_ingroup <int>, pre_outgroup <dbl>,
## #   post_outgroup <dbl>, pre_gap <dbl>, post_gap <dbl>, delta_ingroup <int>, delta_outgroup <dbl>, delta_gap <dbl>,
## #   chapter_excerpt <chr>
```

## Step 3: Inspect a real example output in a scrollable interactive table

The output can be wide.
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) with
horizontal scrolling is helpful. For website builds, we load a recorded
example generated from a real API run on the NYU network. To refresh it,
run `data-raw/refresh-getting-started-example.R` and commit
`inst/extdata/getting-started-res.rds`.

``` r
library(DT)

DT::datatable(
  res,
  rownames = FALSE,
  filter = "top",
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    autoWidth = TRUE
  )
)
```
