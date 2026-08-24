test_that("survey simulation preserves row-wise assignments and turn schemas", {
  calls <- list()
  fake_chat <- list(chat_structured = function(prompt, type) {
    calls[[length(calls) + 1L]] <<- prompt
    if (identical(type$fields, "choice")) {
      list(choice = "yes")
    } else {
      list(score = 7)
    }
  })
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) fake_chat,
    .package = "nalanda"
  )
  people <- tibble::tibble(
    participant_id = c("a", "b"),
    condition = c("control", "treated"),
    bio = c("Ada", "Bea")
  )
  flow <- tibble::tibble(
    screen_id = c("consent", "rating"),
    prompt = c("{bio}: consent", "{bio} in {condition}: rate"),
    response_type = list(list(fields = "choice"), list(fields = "score"))
  )
  out <- run_survey_simulation(people, flow, model_config = c(m = "mock"))
  expect_equal(nrow(out$results), 4)
  expect_match(calls[[4]], "Bea in treated")
  expect_named(out$results$response[[1]], "choice")
  expect_named(out$results$response[[2]], "score")
  expect_true("rating__score" %in% names(out$wide))
})

test_that("memory policies control chat reuse and checkpoints resume a partial survey", {
  chats <- 0L
  calls <- 0L
  fail <- TRUE
  fake_chat <- function(...) {
    chats <<- chats + 1L
    list(chat_structured = function(...) {
      calls <<- calls + 1L
      if (fail && calls == 2L) {
        stop("interrupt")
      }
      list(answer = calls)
    })
  }
  testthat::local_mocked_bindings(
    new_portkey_chat = fake_chat,
    .package = "nalanda"
  )
  flow <- tibble::tibble(
    screen_id = c("one", "two"),
    prompt = "Q",
    response_type = list(list(fields = "answer"), list(fields = "answer"))
  )
  d <- file.path(tempdir(), paste0("survey-", Sys.getpid()))
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(
    run_survey_simulation(
      tibble::tibble(participant_id = "p"),
      flow,
      model_config = c(m = "mock"),
      output_dir = d
    ),
    "interrupt"
  )
  fail <- FALSE
  resumed <- run_survey_simulation(
    tibble::tibble(participant_id = "p"),
    flow,
    model_config = c(m = "mock"),
    output_dir = d
  )
  expect_equal(calls, 3L)
  expect_equal(resumed$tasks$status, c("resumed", "completed"))
  chats <- 0L
  run_survey_simulation(
    tibble::tibble(participant_id = "q"),
    flow,
    model_config = c(m = "mock"),
    memory = "profile"
  )
  expect_equal(chats, 2L)
})

test_that("survey plans expand model-specific repetitions and can continue errors", {
  plan <- plan_survey_simulation(
    tibble::tibble(participant_id = 1:2),
    tibble::tibble(
      screen_id = "x",
      prompt = "x",
      response_type = list(list(fields = "x"))
    ),
    model_config = tibble::tibble(
      model_id = c("a", "b"),
      model = c("a", "b"),
      n_completions = c(1L, 2L)
    )
  )
  expect_equal(nrow(plan), 6L)
  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, ...) {
      list(chat_structured = function(...) {
        if (model == "bad") {
          stop("bad")
        }
        list(x = 1)
      })
    },
    .package = "nalanda"
  )
  out <- run_survey_simulation(
    tibble::tibble(participant_id = "p"),
    tibble::tibble(
      screen_id = "x",
      prompt = "x",
      response_type = list(list(fields = "x"))
    ),
    model_config = c(good = "good", bad = "bad"),
    on_error = "continue"
  )
  expect_equal(nrow(out$errors), 1L)
  expect_equal(nrow(out$results), 1L)
})

test_that("display conditions and text responses work without sharing skipped turns", {
  seen <- character()
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      list(
        chat = function(prompt) {
          seen <<- c(seen, prompt)
          '{"note":"ok"}'
        }
      )
    },
    .package = "nalanda"
  )
  flow <- tibble::tibble(
    screen_id = c("shown", "hidden"),
    prompt = c("hello {condition}", "never"),
    response_type = list(
      list(fields = list(note = NULL)),
      list(fields = list(note = NULL))
    ),
    output_mode = c("text", "text"),
    display_if = list(
      function(participant, answers) participant$condition[[1]] == "yes",
      FALSE
    )
  )
  out <- run_survey_simulation(
    tibble::tibble(participant_id = "p", condition = "yes"),
    flow,
    model_config = c(m = "mock")
  )
  expect_equal(nrow(out$results), 1L)
  expect_equal(out$results$response[[1]]$note, "ok")
  expect_length(seen, 1L)
})
