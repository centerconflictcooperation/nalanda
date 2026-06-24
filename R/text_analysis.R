#' Build a numeric-response prompt for text analysis
#'
#' This helper creates prompts in the same style used by Rathje et al. (2024):
#' a direct question, followed by a numeric response instruction, followed by
#' the text placeholder. The returned prompt is a template and may include
#' placeholders such as `{text}` or `{language}` that are expanded later by
#' [run_text_analysis()].
#'
#' @param question Character scalar question shown before the response
#'   instructions.
#' @param labels Optional character vector of class labels in numeric order.
#'   For example, `c("positive", "neutral", "negative")`.
#' @param scale Optional numeric vector of length 2 giving the response scale
#'   range, such as `c(1, 7)`.
#' @param anchors Optional character vector of length 2 giving the low and high
#'   anchor labels used with `scale`.
#' @param text_label Character scalar introducing the text block.
#' @param text_placeholder Character scalar placeholder to insert where the text
#'   should appear.
#'
#' @return A character scalar prompt template.
#' @export
make_annotation_prompt <- function(
  question,
  labels = NULL,
  scale = NULL,
  anchors = NULL,
  text_label = "Here is the text:",
  text_placeholder = "{text}"
) {
  if (!is.character(question) || length(question) != 1 || !nzchar(question)) {
    stop("`question` must be a single non-empty string.")
  }

  has_labels <- !is.null(labels)
  has_scale <- !is.null(scale)
  if (has_labels && has_scale) {
    stop("Please supply either `labels` or `scale`, not both.")
  }
  if (!has_labels && !has_scale) {
    stop("Please supply either `labels` or `scale`.")
  }

  if (has_labels) {
    if (!is.character(labels) || length(labels) < 2) {
      stop("`labels` must be a character vector with at least 2 entries.")
    }
    label_bits <- paste0(seq_along(labels), " if ", labels)
    answer_line <- paste(
      "Answer only with a number:",
      paste(label_bits, collapse = ", ")
    )
  } else {
    if (!is.numeric(scale) || length(scale) != 2) {
      stop("`scale` must be a numeric vector of length 2.")
    }
    scale <- as.integer(scale)
    if (scale[[1]] >= scale[[2]]) {
      stop("`scale` must be increasing, such as `c(1, 7)`.")
    }
    if (!is.null(anchors)) {
      if (!is.character(anchors) || length(anchors) != 2) {
        stop("`anchors` must be a character vector of length 2.")
      }
      answer_line <- paste0(
        "Answer only with a number, with ",
        scale[[1]], " being \"", anchors[[1]], "\" and ",
        scale[[2]], " being \"", anchors[[2]], "\"."
      )
    } else {
      answer_line <- paste0(
        "Answer only with a number from ",
        scale[[1]], " to ", scale[[2]], "."
      )
    }
  }

  paste(
    question,
    answer_line,
    text_label,
    text_placeholder,
    sep = "\n"
  )
}

#' Run row-wise text analysis with a prompt template
#'
#' This function applies a prompt template to each row of a text dataset and
#' extracts structured responses with `ellmer`. It is designed for dataset-first
#' workflows such as sentiment, emotion, offensiveness, or moral-foundation
#' annotation across many short texts.
#'
#' @param data A data frame with at least one text column.
#' @param text_col Name of the column containing the text to analyze.
#' @param prompt Character scalar prompt template. It may reference any columns
#'   in `data` using `{column_name}` placeholders.
#' @param response_type An `ellmer` structured type specification, for example
#'   `ellmer::type_object(score = ellmer::type_number())`.
#' @param output_mode Character. `"structured"` (default) uses the backend's
#'   structured-output support. `"text"` is a compatibility mode for models
#'   that do not support structured outputs (for example some Anthropic models):
#'   nalanda appends strict JSON-only instructions to the prompt, calls the
#'   model as free text, then parses the JSON back into the same fields.
#'   Text mode is best-effort and stores the original model reply in
#'   `raw_response`.
#' @param id_col Optional column name identifying each text row. When omitted, a
#'   sequential `text_id` is created.
#' @param n_simulations Integer. Number of repeated runs per row.
#' @param temperature Numeric. Sampling temperature passed to the backend.
#' @param seed Integer. Random seed for reproducibility.
#' @param model Character. Model name for the chat backend.
#' @param integration Optional Portkey/gateway route slug. Use a route returned
#'   by `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`
#'   when working with the NYU gateway.
#' @param virtual_key Optional legacy virtual key.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of text characters to retain in stored
#'   prompt previews.
#' @param max_active Integer. Maximum number of concurrent requests passed to
#'   `ellmer::parallel_chat_structured()` in structured mode. Text mode runs
#'   plain chat requests sequentially.
#' @param rpm Integer. Requests-per-minute cap passed to
#'   `ellmer::parallel_chat_structured()` in structured mode. Text mode runs
#'   plain chat requests sequentially.
#'
#' @return A tibble containing the original row metadata, simulation index,
#'   structured response fields, and stored prompt previews.
#' @export
run_text_analysis <- function(
  data,
  text_col = "text",
  prompt,
  response_type,
  output_mode = c("structured", "text"),
  id_col = NULL,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  max_active = 10,
  rpm = 500
) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(text_col) || length(text_col) != 1 || !nzchar(text_col)) {
    stop("`text_col` must be a single non-empty column name.")
  }
  if (!text_col %in% names(data)) {
    stop("`text_col` was not found in `data`.")
  }
  if (!is.null(id_col) && (!is.character(id_col) || length(id_col) != 1 || !id_col %in% names(data))) {
    stop("`id_col` must be NULL or a single column name present in `data`.")
  }
  if (!is.character(prompt) || length(prompt) != 1 || !nzchar(prompt)) {
    stop("`prompt` must be a single non-empty string.")
  }
  if (missing(response_type) || is.null(response_type)) {
    stop("Please provide `response_type`.")
  }
  output_mode <- normalize_output_mode(output_mode)
  if (n_simulations < 1) {
    stop("`n_simulations` must be >= 1.")
  }

  route <- resolve_model_route(
    integration = integration,
    virtual_key = virtual_key,
    integration_missing = missing(integration),
    virtual_key_missing = missing(virtual_key)
  )
  integration <- route$integration
  virtual_key <- route$virtual_key

  if (!startsWith(model, "@")) {
    prefix <- NULL
    if (!is.null(integration) && nzchar(integration)) {
      prefix <- integration
    } else if (!is.null(virtual_key) && nzchar(virtual_key)) {
      prefix <- virtual_key
    }
    if (!is.null(prefix)) {
      model <- paste0("@", prefix, "/", model)
    }
  }
  validate_model_parameters(model = model, temperature = temperature)

  df <- tibble::as_tibble(data)
  if (is.null(id_col)) {
    df$text_id <- seq_len(nrow(df))
    id_col <- "text_id"
  }

  rows <- vector("list", nrow(df) * n_simulations)
  row_i <- 0L

  for (k in seq_len(n_simulations)) {
    chat <- new_portkey_chat(
      model = model,
      base_url = base_url,
      temperature = temperature,
      seed = seed + k - 1L
    )

    prompts <- character(nrow(df))
    prompt_preview <- character(nrow(df))

    for (i in seq_len(nrow(df))) {
      row_values <- as.list(df[i, , drop = FALSE])
      prompts[[i]] <- interpolate_prompt_template(prompt, row_values)

      preview_values <- row_values
      preview_values[[text_col]] <- compact_chapter_text(
        as.character(preview_values[[text_col]]),
        excerpt_chars = excerpt_chars
      )
      prompt_preview[[i]] <- interpolate_prompt_template(prompt, preview_values)
    }

    if (identical(output_mode, "structured")) {
      responses <- ellmer::parallel_chat_structured(
        chat = chat,
        prompts = prompts,
        type = response_type,
        convert = TRUE,
        max_active = max_active,
        rpm = rpm,
        on_error = "stop"
      )
      response_list <- normalize_parallel_chat_structured_output(responses)
    } else {
      response_fields <- infer_response_type_fields(response_type)
      response_list <- lapply(
        prompts,
        function(prompt) {
          chat_model_response(
            chat = chat,
            prompt = prompt,
            type = response_type,
            output_mode = output_mode,
            fields = response_fields
          )
        }
      )
    }

    if (length(response_list) != nrow(df)) {
      stop(
        "Expected ", nrow(df),
        " responses from `ellmer::parallel_chat_structured()`, got ",
        length(response_list), "."
      )
    }

    for (i in seq_len(nrow(df))) {
      base_row <- as.list(df[i, , drop = FALSE])
      base_row$sim <- k
      base_row$prompt <- prompt_preview[[i]]

      row_i <- row_i + 1L
      rows[[row_i]] <- c(base_row, response_list[[i]])
    }
  }

  out <- tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  ))

  long_cols <- intersect(c(text_col, "prompt"), names(out))
  if (length(long_cols) > 0) {
    out <- out[, c(setdiff(names(out), long_cols), long_cols)]
  }

  class(out) <- c(class(out), "nalanda")
  attr(out, "model") <- normalize_model_name(model)
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
  attr(out, "text_col") <- text_col
  attr(out, "id_col") <- id_col
  attr(out, "prompt_template") <- prompt
  out
}

#' Evaluate text-analysis outputs against reference labels
#'
#' This helper computes common agreement metrics used in text-analysis papers,
#' including accuracy, macro precision/recall/F1, Spearman correlation, and
#' weighted Cohen's kappa.
#'
#' @param data A data frame containing reference and predicted columns.
#' @param truth_col Name of the reference-label column.
#' @param estimate_col Name of the model-estimate column.
#' @param by Optional character vector of grouping columns.
#' @param metric Character vector of metrics to compute. Supported values are
#'   `"accuracy"`, `"macro_precision"`, `"macro_recall"`, `"macro_f1"`,
#'   `"spearman"`, and `"weighted_kappa"`.
#' @param kappa_weights Weighting scheme for Cohen's kappa. One of
#'   `"quadratic"` (default) or `"linear"`.
#'
#' @return A tibble with one row per requested group and one column per metric.
#' @export
evaluate_text_analysis <- function(
  data,
  truth_col,
  estimate_col,
  by = NULL,
  metric = c(
    "accuracy",
    "macro_precision",
    "macro_recall",
    "macro_f1",
    "spearman",
    "weighted_kappa"
  ),
  kappa_weights = c("quadratic", "linear")
) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame.")
  }
  if (!truth_col %in% names(data)) {
    stop("`truth_col` was not found in `data`.")
  }
  if (!estimate_col %in% names(data)) {
    stop("`estimate_col` was not found in `data`.")
  }
  if (!is.null(by) && !all(by %in% names(data))) {
    stop("All `by` columns must be present in `data`.")
  }

  kappa_weights <- match.arg(kappa_weights)
  metric <- unique(metric)
  valid_metrics <- c(
    "accuracy",
    "macro_precision",
    "macro_recall",
    "macro_f1",
    "spearman",
    "weighted_kappa"
  )
  bad_metrics <- setdiff(metric, valid_metrics)
  if (length(bad_metrics) > 0) {
    stop("Unsupported metric(s): ", paste(bad_metrics, collapse = ", "))
  }

  df <- tibble::as_tibble(data)
  if (is.null(by) || length(by) == 0) {
    groups <- list(`__all__` = seq_len(nrow(df)))
  } else {
    group_key <- interaction(df[by], drop = TRUE, lex.order = TRUE)
    groups <- split(seq_len(nrow(df)), group_key)
  }

  out <- vector("list", length(groups))
  out_i <- 0L

  for (idx in groups) {
    out_i <- out_i + 1L
    xi <- df[idx, , drop = FALSE]
    keep <- stats::complete.cases(xi[, c(truth_col, estimate_col), drop = FALSE])
    xi <- xi[keep, , drop = FALSE]

    row <- if (length(by) > 0) {
      as.list(df[idx[1], by, drop = FALSE])
    } else {
      list()
    }
    row$n <- nrow(xi)

    truth <- xi[[truth_col]]
    estimate <- xi[[estimate_col]]

    if ("accuracy" %in% metric) {
      row$accuracy <- if (nrow(xi) == 0) {
        NA_real_
      } else {
        mean(as.character(truth) == as.character(estimate))
      }
    }

    if (any(c("macro_precision", "macro_recall", "macro_f1") %in% metric)) {
      prf <- classification_summary(truth, estimate)
      if ("macro_precision" %in% metric) row$macro_precision <- prf$macro_precision
      if ("macro_recall" %in% metric) row$macro_recall <- prf$macro_recall
      if ("macro_f1" %in% metric) row$macro_f1 <- prf$macro_f1
    }

    if ("spearman" %in% metric) {
      row$spearman <- if (nrow(xi) < 2) {
        NA_real_
      } else {
        suppressWarnings(stats::cor(
          as.numeric(truth),
          as.numeric(estimate),
          method = "spearman"
        ))
      }
    }

    if ("weighted_kappa" %in% metric) {
      row$weighted_kappa <- weighted_kappa_score(
        truth = truth,
        estimate = estimate,
        weights = kappa_weights
      )
    }

    out[[out_i]] <- row
  }

  tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(out, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  ))
}

interpolate_prompt_template <- function(template, values) {
  out <- template
  if (length(values) == 0) {
    return(out)
  }

  for (nm in names(values)) {
    value <- values[[nm]]
    if (length(value) != 1) {
      next
    }
    replacement <- as.character(value)
    if (is.na(replacement)) {
      replacement <- ""
    }
    out <- gsub(
      paste0("\\{", nm, "\\}"),
      replacement,
      out
    )
  }

  out
}

classification_summary <- function(truth, estimate) {
  truth_chr <- as.character(truth)
  estimate_chr <- as.character(estimate)
  labels <- sort(unique(c(truth_chr, estimate_chr)))
  labels <- labels[!is.na(labels) & nzchar(labels)]

  if (length(labels) == 0) {
    return(list(
      macro_precision = NA_real_,
      macro_recall = NA_real_,
      macro_f1 = NA_real_
    ))
  }

  precision <- recall <- f1 <- numeric(length(labels))

  for (i in seq_along(labels)) {
    label <- labels[[i]]
    tp <- sum(truth_chr == label & estimate_chr == label, na.rm = TRUE)
    fp <- sum(truth_chr != label & estimate_chr == label, na.rm = TRUE)
    fn <- sum(truth_chr == label & estimate_chr != label, na.rm = TRUE)

    precision[[i]] <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall[[i]] <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1[[i]] <- if (is.na(precision[[i]]) || is.na(recall[[i]]) ||
      (precision[[i]] + recall[[i]]) == 0) {
      NA_real_
    } else {
      2 * precision[[i]] * recall[[i]] / (precision[[i]] + recall[[i]])
    }
  }

  list(
    macro_precision = mean(precision, na.rm = TRUE),
    macro_recall = mean(recall, na.rm = TRUE),
    macro_f1 = mean(f1, na.rm = TRUE)
  )
}

weighted_kappa_score <- function(truth, estimate, weights = c("quadratic", "linear")) {
  weights <- match.arg(weights)

  truth_num <- suppressWarnings(as.numeric(as.character(truth)))
  estimate_num <- suppressWarnings(as.numeric(as.character(estimate)))
  keep <- !is.na(truth_num) & !is.na(estimate_num)
  truth_num <- truth_num[keep]
  estimate_num <- estimate_num[keep]

  if (length(truth_num) == 0) {
    return(NA_real_)
  }

  labels <- sort(unique(c(truth_num, estimate_num)))
  k <- length(labels)
  if (k == 1) {
    return(1)
  }

  truth_fac <- factor(truth_num, levels = labels)
  estimate_fac <- factor(estimate_num, levels = labels)
  observed <- as.matrix(table(truth_fac, estimate_fac))
  n <- sum(observed)

  row_marginals <- rowSums(observed)
  col_marginals <- colSums(observed)
  expected <- outer(row_marginals, col_marginals) / n

  distance <- outer(seq_len(k), seq_len(k), "-")
  if (weights == "quadratic") {
    w <- 1 - (distance / (k - 1))^2
  } else {
    w <- 1 - abs(distance) / (k - 1)
  }

  p_o <- sum(w * observed) / n
  p_e <- sum(w * expected) / n

  if (isTRUE(all.equal(1 - p_e, 0))) {
    return(NA_real_)
  }

  (p_o - p_e) / (1 - p_e)
}
