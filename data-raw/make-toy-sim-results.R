# Create a small exported toy dataset for README and vignette examples.

books <- c("Bridge Stories", "Common Ground")
chapters <- paste0("chapter_", 1:4)
parties <- c("Democrat", "Republican")

toy_sim_results <- expand.grid(
  book = books,
  chapter = chapters,
  sim = 1:2,
  party = parties,
  stringsAsFactors = FALSE
)

toy_sim_results <- tibble::as_tibble(toy_sim_results) |>
  dplyr::mutate(
    identity = .data$party,
    chapter_num = as.integer(sub("chapter_", "", .data$chapter)),
    chapter_excerpt = dplyr::case_when(
      .data$chapter == "chapter_1" ~ "Two neighbors rebuild a bridge together.",
      .data$chapter == "chapter_2" ~ "A shared school project softens group boundaries.",
      .data$chapter == "chapter_3" ~ "Families collaborate during a city festival.",
      TRUE ~ "Volunteers coordinate after a flood."
    ),
    pre_ingroup = dplyr::case_when(
      .data$book == "Bridge Stories" & .data$party == "Democrat" ~ 77L + .data$sim,
      .data$book == "Bridge Stories" & .data$party == "Republican" ~ 81L + .data$sim,
      .data$party == "Democrat" ~ 76L + .data$sim,
      TRUE ~ 80L + .data$sim
    ),
    pre_outgroup = dplyr::case_when(
      .data$book == "Bridge Stories" & .data$party == "Democrat" ~ 43 + 2 * .data$chapter_num + .data$sim,
      .data$book == "Bridge Stories" & .data$party == "Republican" ~ 48 + 2 * .data$chapter_num + .data$sim,
      .data$book == "Common Ground" & .data$party == "Democrat" ~ 47 + 2 * .data$chapter_num + .data$sim,
      TRUE ~ 45 + 2 * .data$chapter_num + .data$sim
    ),
    target_delta_gap = dplyr::case_when(
      .data$book == "Bridge Stories" & .data$party == "Democrat" ~ c(3, 6, 9, 12)[.data$chapter_num] + 0.5 * (.data$sim - 1),
      .data$book == "Bridge Stories" & .data$party == "Republican" ~ c(11, 8, 5, 2)[.data$chapter_num] - 0.5 * (.data$sim - 1),
      .data$book == "Common Ground" & .data$party == "Democrat" ~ c(4, 6, 5, 7)[.data$chapter_num] + 0.5 * (.data$sim - 1),
      TRUE ~ c(5, 4, 6, 5)[.data$chapter_num] - 0.5 * (.data$sim - 1)
    ),
    delta_ingroup = dplyr::case_when(
      .data$book == "Bridge Stories" & .data$party == "Democrat" ~ c(1, 1, 2, 2)[.data$chapter_num],
      .data$book == "Bridge Stories" & .data$party == "Republican" ~ c(2, 2, 1, 1)[.data$chapter_num],
      .data$book == "Common Ground" & .data$party == "Democrat" ~ c(1, 2, 1, 2)[.data$chapter_num],
      TRUE ~ c(2, 1, 2, 1)[.data$chapter_num]
    ),
    delta_outgroup = .data$delta_ingroup + .data$target_delta_gap,
    post_ingroup = .data$pre_ingroup + .data$delta_ingroup,
    post_outgroup = .data$pre_outgroup + .data$delta_outgroup,
    pre_gap = .data$pre_ingroup - .data$pre_outgroup,
    post_gap = .data$post_ingroup - .data$post_outgroup,
    delta_gap = .data$delta_outgroup - .data$delta_ingroup
  ) |>
  dplyr::select(
    "book", "chapter", "sim", "identity", "party",
    "pre_ingroup", "post_ingroup", "pre_outgroup", "post_outgroup",
    "pre_gap", "post_gap", "delta_ingroup", "delta_outgroup", "delta_gap",
    "chapter_excerpt"
  )

attr(toy_sim_results, "model") <- "gemini-2.5-flash-lite"
attr(toy_sim_results, "temperature") <- 0
attr(toy_sim_results, "out.attrs") <- NULL

other_party <- function(x) ifelse(x == "Democrat", "Republican", "Democrat")

toy_run_ai_turns <- dplyr::bind_rows(
  toy_sim_results |>
    dplyr::transmute(
      book, chapter, sim, identity, party,
      turn_index = 1L,
      turn_type = "baseline",
      target_group = identity,
      rating = pre_ingroup,
      baseline_prompt = paste0(
        "You are simulating an American adult who politically identifies as a ",
        identity,
        ". How warmly do you feel towards ", identity,
        "s? How warmly do you feel towards ", other_party(identity), "s?"
      ),
      post_prompt = paste0("Chapter text preview: ", chapter_excerpt)
    ),
  toy_sim_results |>
    dplyr::transmute(
      book, chapter, sim, identity, party,
      turn_index = 1L,
      turn_type = "baseline",
      target_group = other_party(identity),
      rating = pre_outgroup,
      baseline_prompt = paste0(
        "You are simulating an American adult who politically identifies as a ",
        identity,
        ". How warmly do you feel towards ", identity,
        "s? How warmly do you feel towards ", other_party(identity), "s?"
      ),
      post_prompt = paste0("Chapter text preview: ", chapter_excerpt)
    ),
  toy_sim_results |>
    dplyr::transmute(
      book, chapter, sim, identity, party,
      turn_index = 2L,
      turn_type = "post",
      target_group = identity,
      rating = post_ingroup,
      baseline_prompt = paste0(
        "You are simulating an American adult who politically identifies as a ",
        identity,
        ". How warmly do you feel towards ", identity,
        "s? How warmly do you feel towards ", other_party(identity), "s?"
      ),
      post_prompt = paste0("Chapter text preview: ", chapter_excerpt)
    ),
  toy_sim_results |>
    dplyr::transmute(
      book, chapter, sim, identity, party,
      turn_index = 2L,
      turn_type = "post",
      target_group = other_party(identity),
      rating = post_outgroup,
      baseline_prompt = paste0(
        "You are simulating an American adult who politically identifies as a ",
        identity,
        ". How warmly do you feel towards ", identity,
        "s? How warmly do you feel towards ", other_party(identity), "s?"
      ),
      post_prompt = paste0("Chapter text preview: ", chapter_excerpt)
    )
) |>
  dplyr::arrange(book, chapter, sim, identity, turn_index, target_group)

attr(toy_run_ai_turns, "model") <- "gemini-2.5-flash-lite"
attr(toy_run_ai_turns, "temperature") <- 0
attr(toy_run_ai_turns, "chapter_excerpts") <- toy_sim_results |>
  dplyr::distinct(book, chapter, chapter_excerpt)

dir.create("data", showWarnings = FALSE)
save(toy_run_ai_turns, file = "data/toy_run_ai_turns.rda", compress = "bzip2")
save(toy_sim_results, file = "data/toy_sim_results.rda", compress = "bzip2")
