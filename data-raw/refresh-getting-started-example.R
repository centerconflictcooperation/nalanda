# Refresh vignette example output for vignettes/getting-started.Rmd
# Run this script from an NYU-network environment where PORTKEY_API_KEY works.

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

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
saveRDS(res, "inst/extdata/getting-started-res.rds")
