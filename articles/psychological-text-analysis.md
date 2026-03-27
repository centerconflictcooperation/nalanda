# Psychological Text Analysis with \`nalanda\`

## Purpose

This vignette shows how to use `nalanda` for the kind of workflow
described by Rathje et al. (2024): apply a simple prompt to many short
texts, ask for a numeric response, and compare model outputs to human
annotations.

The goal here is not to reproduce every benchmark in the paper. The goal
is to give a simple getting-started pattern you can adapt for:

1.  categorical sentiment,
2.  discrete emotions,
3.  offensiveness,
4.  Likert-style sentiment or emotion ratings, and
5.  multilingual datasets with a `language` column.

## 1. Set package options

As in the other live `nalanda` workflows, it is easiest to set model
routing once at the top of your script.

``` r
library(nalanda)

options(
  nalanda.integration = "gpt-5-mini",
  nalanda.base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/"
)

# In some Portkey/gateway setups the route slug is not the provider name.
# Verify the route with ellmer::models_portkey() or use a fully-qualified
# model string such as "@gpt-5-mini/gpt-5-mini" if that is the route that works
# in your gateway.
```

``` r
ellmer::models_portkey(
  base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/"
)
```

## 2. Create a small text dataset

The paper works row-wise over tweets or headlines.
[`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md)
uses the same pattern: one row per text.

``` r
texts <- tibble::tibble(
  id = 1:4,
  language = c("English", "English", "Arabic", "Simplified Chinese"),
  text = c(
    "I love this new community project.",
    "This policy announcement is fine, I guess.",
    "\u0647\u0630\u0627 \u0627\u0644\u062e\u0628\u0631 \u0631\u0627\u0626\u0639 \u0644\u0644\u063a\u0627\u064a\u0629",
    "\u6211\u4e0d\u559c\u6b22\u4ed6\u4eec\u5904\u7406\u8fd9\u4e2a\u95ee\u9898\u7684\u65b9\u5f0f\u3002"
  ),
  human_sentiment = c(1, 2, 1, 3)
)

texts
#> # A tibble: 4 × 4
#>      id language           text                                  human_sentiment
#>   <int> <chr>              <chr>                                           <dbl>
#> 1     1 English            I love this new community project.                  1
#> 2     2 English            This policy announcement is fine, I …               2
#> 3     3 Arabic             هذا الخبر رائع للغاية                               1
#> 4     4 Simplified Chinese 我不喜欢他们处理这个问题的方式。                    3
```

If a right-to-left language such as Arabic looks visually out of order
in your console or knitted output, that is usually a bidi rendering
issue rather than a row-order issue. One safe display-only workaround is
to wrap the printed Arabic string in Unicode directional isolates:

``` r
texts_display <- texts
arabic_row <- texts_display$language == "Arabic"
texts_display$text[arabic_row] <- paste0(
  "\u2067",
  texts_display$text[arabic_row],
  "\u2069"
)

texts_display
#> # A tibble: 4 × 4
#>      id language           text                                  human_sentiment
#>   <int> <chr>              <chr>                                           <dbl>
#> 1     1 English            I love this new community project.                  1
#> 2     2 English            This policy announcement is fine, I …               2
#> 3     3 Arabic             ⁧هذا الخبر رائع للغاية⁩                               1
#> 4     4 Simplified Chinese 我不喜欢他们处理这个问题的方式。                    3
```

Use the original `texts$text` values for API calls. The isolated version
is mainly useful when printing or rendering tables.

Here the human labels follow the same coding style used in the paper:

1.  `1 = positive`
2.  `2 = neutral`
3.  `3 = negative`

## 3. Build the prompt

The screenshot tutorial shows a very direct prompt. You can build the
same kind of prompt with
[`make_annotation_prompt()`](https://centerconflictcooperation.github.io/nalanda/reference/make_annotation_prompt.md).

``` r
sentiment_prompt <- make_annotation_prompt(
  question = "Is the sentiment of this {language} text positive, neutral, or negative?",
  labels = c("positive", "neutral", "negative")
)

cat(sentiment_prompt)
#> Is the sentiment of this {language} text positive, neutral, or negative?
#> Answer only with a number: 1 if positive, 2 if neutral, 3 if negative
#> Here is the text:
#> {text}
```

This returns a prompt template, not a final prompt. The `{language}` and
[text](https://r-text.org/) placeholders will be filled separately for
each row.

## 4. Run the analysis

Now apply the prompt to every row with
[`run_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/run_text_analysis.md).
The result schema is defined with `ellmer` just like in the other
`nalanda` workflows.

``` r
res <- run_text_analysis(
  data = texts,
  id_col = "id",
  text_col = "text",
  prompt = sentiment_prompt,
  response_type = ellmer::type_object(
    gpt = ellmer::type_number()
  ),
  n_simulations = 1,
  temperature = 0,
  model = "gpt-5-mini"
)
```

The important differences from the older chapter-based functions are:

1.  the input is a data frame, not book/chapter text,
2.  each row is analyzed directly,
3.  any column can be interpolated into the prompt with `{column_name}`,
    and
4.  the output stays aligned to the original row metadata.

## 5. Inspect the output

Each row of the result corresponds to one text and one simulation run.

|  id | language           | sim | human_sentiment | gpt | text                                       |
|----:|:-------------------|----:|----------------:|----:|:-------------------------------------------|
|   1 | English            |   1 |               1 |   1 | I love this new community project.         |
|   2 | English            |   1 |               2 |   2 | This policy announcement is fine, I guess. |
|   3 | Arabic             |   1 |               1 |   1 | هذا الخبر رائع للغاية                      |
|   4 | Simplified Chinese |   1 |               3 |   3 | 我不喜欢他们处理这个问题的方式。           |

This is the same basic structure as the screenshot workflow, but the
parsing is already handled for you because the response is extracted as
a structured numeric field.

## 6. Evaluate GPT against human labels

Rathje et al. compare GPT output to human annotations with metrics such
as accuracy, macro F1, and Spearman correlations.
[`evaluate_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/evaluate_text_analysis.md)
provides a simple package-native version of that step.

``` r
scores <- evaluate_text_analysis(
  res,
  truth_col = "human_sentiment",
  estimate_col = "gpt",
  by = "language",
  metric = c("accuracy", "macro_precision", "macro_recall", "macro_f1")
)

scores
```

| language           |   n | accuracy | macro_precision | macro_recall | macro_f1 |
|:-------------------|----:|---------:|----------------:|-------------:|---------:|
| Arabic             |   1 |        1 |               1 |            1 |        1 |
| English            |   2 |        1 |               1 |            1 |        1 |
| Simplified Chinese |   1 |        1 |               1 |            1 |        1 |

For Likert-style tasks, switch the metric set to something like:

``` r
evaluate_text_analysis(
  res,
  truth_col = "human_rating",
  estimate_col = "gpt",
  metric = c("spearman", "weighted_kappa")
)
```

## 7. Likert-style sentiment or emotion

The paper also evaluates headline sentiment and emotions on 1 to 7
scales. That prompt style is also supported.

``` r
likert_prompt <- make_annotation_prompt(
  question = "How negative or positive is this headline on a 1 to 7 scale?",
  scale = c(1, 7),
  anchors = c("very negative", "very positive"),
  text_label = "Here is the headline:"
)

cat(likert_prompt)
#> How negative or positive is this headline on a 1 to 7 scale?
#> Answer only with a number, with 1 being "very negative" and 7 being "very positive".
#> Here is the headline:
#> {text}
```

The live call looks the same, except the response field now represents a
Likert rating instead of a class code.

``` r
headline_res <- run_text_analysis(
  data = headlines,
  id_col = "headline_id",
  text_col = "headline",
  prompt = likert_prompt,
  response_type = ellmer::type_object(
    gpt = ellmer::type_number()
  ),
  temperature = 0,
  model = "gpt-5-mini"
)
```

## 8. Repeated runs for reliability

The paper also checks whether repeated runs produce similar outputs. To
do that, increase `n_simulations`.

``` r
res_repeated <- run_text_analysis(
  data = texts,
  id_col = "id",
  text_col = "text",
  prompt = sentiment_prompt,
  response_type = ellmer::type_object(
    gpt = ellmer::type_number()
  ),
  n_simulations = 2,
  temperature = 0,
  model = "gpt-5-mini"
)
```

Then compare run 1 and run 2 with
[`evaluate_text_analysis()`](https://centerconflictcooperation.github.io/nalanda/reference/evaluate_text_analysis.md)
after reshaping the results into one column per run.

## 9. When to use this workflow

Use this vignette’s workflow when:

1.  your unit is a row of text, not a chapter,
2.  you want direct zero-shot annotation with a simple prompt,
3.  you need multilingual prompt interpolation from dataset columns, or
4.  you want agreement metrics against human labels.

Use the chapter-oriented workflows when your unit is still a book
chapter and you care about pre/post changes across simulated identities.

## Reference

Rathje, S., Mirea, D. M., Sucholutsky, I., Marjieh, R., Robertson, C.
E., & Van Bavel, J. J. (2024). *GPT is an effective tool for
multilingual psychological text analysis*. *Proceedings of the National
Academy of Sciences, 121*(34), e2308950121.
<https://doi.org/10.1073/pnas.2308950121>
