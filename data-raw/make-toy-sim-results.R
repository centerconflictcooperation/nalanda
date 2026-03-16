# Create a small exported toy dataset for README and vignette examples.

toy_sim_results <- tibble::tribble(
  ~book, ~chapter, ~sim, ~identity, ~party,
  ~pre_ingroup, ~post_ingroup, ~pre_outgroup, ~post_outgroup,
  ~pre_gap, ~post_gap, ~delta_ingroup, ~delta_outgroup, ~delta_gap,
  ~chapter_excerpt,
  "Bridge Stories", "chapter_1", 1, "Democrat",   "Democrat",   78, 79, 42, 50, 36, 29,  1,  8, -7, "Two neighbors rebuild a bridge together.",
  "Bridge Stories", "chapter_1", 1, "Republican", "Republican", 80, 81, 38, 46, 42, 35,  1,  8, -7, "Two neighbors rebuild a bridge together.",
  "Bridge Stories", "chapter_2", 2, "Democrat",   "Democrat",   77, 79, 45, 54, 32, 25,  2,  9, -7, "A shared school project softens group boundaries.",
  "Bridge Stories", "chapter_2", 2, "Republican", "Republican", 79, 80, 40, 49, 39, 31,  1,  9, -8, "A shared school project softens group boundaries.",
  "Common Ground",  "chapter_1", 1, "Democrat",   "Democrat",   76, 77, 48, 54, 28, 23,  1,  6, -5, "A town hall meeting ends in cooperation.",
  "Common Ground",  "chapter_1", 1, "Republican", "Republican", 81, 82, 44, 49, 37, 33,  1,  5, -4, "A town hall meeting ends in cooperation.",
  "Common Ground",  "chapter_2", 2, "Democrat",   "Democrat",   75, 77, 50, 58, 25, 19,  2,  8, -6, "Volunteers coordinate after a flood.",
  "Common Ground",  "chapter_2", 2, "Republican", "Republican", 80, 81, 46, 53, 34, 28,  1,  7, -6, "Volunteers coordinate after a flood."
)

dir.create("data", showWarnings = FALSE)
save(toy_sim_results, file = "data/toy_sim_results.rda", compress = "bzip2")
