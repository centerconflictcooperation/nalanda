#' Extract text from a PDF with a multimodal LLM
#'
#' This helper uploads a local PDF to a multimodal model through `ellmer` and
#' asks the model to return clean running text. It is useful when ordinary OCR
#' struggles with stamps, overlays, or poor scan quality but the target model
#' can read PDFs directly.
#'
#' In testing through the NYU Portkey/gateway path, PDF extraction was more
#' reliable with `gpt-5-mini` than with Gemini routes. Gemini-family models may
#' still work in other environments, but PDF handling through
#' `chat_portkey()` was inconsistent in our tests.
#'
#' @param pdf_path Character scalar path to a local PDF file, a character vector
#'   of PDF paths, or a named/nested list of PDF paths such as the output of
#'   `list_book_chapters(extension = "pdf")`.
#' @param prompt Character scalar instruction shown alongside the PDF. The
#'   default asks for faithful transcription while ignoring obvious non-book
#'   overlays such as repeated stamps, page numbers, and headers/footers.
#' @param model Character. Model name for the chat backend.
#' @param integration Optional Portkey/gateway route slug. If supplied and
#'   `model` is not fully-qualified, nalanda will build
#'   `"@{integration}/{model}"`.
#' @param virtual_key Optional legacy virtual key. If supplied and `model` is
#'   not fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#' @param base_url Character. Base URL for API calls.
#' @param temperature Numeric. Sampling temperature passed to the backend.
#' @param seed Integer. Random seed for reproducibility.
#' @param output_path Optional output target. For a single PDF, this may be
#'   either an exact `.txt` file path or a directory path. For a character
#'   vector or nested list of PDFs, supply a directory-like path without a file
#'   extension; nalanda will write one `.txt` per PDF incrementally,
#'   preserving partial progress if a later file fails.
#' @param overwrite Logical scalar. If `TRUE`, replace existing output files at
#'   `output_path`. Defaults to `FALSE`.
#'
#' @return If `pdf_path` is a single file, a character scalar containing the
#'   extracted text. If `pdf_path` is a character vector or nested list, returns
#'   text with the same structure and names as the input. If `output_path` is
#'   supplied, text files are also written to disk.
#' @export
extract_pdf_text_with_llm <- function(
  pdf_path,
  prompt = paste(
    "Transcribe the main body text from this PDF as plain UTF-8 text.",
    "Keep the wording faithful to the source.",
    "Ignore repeated stamps, watermarks, page numbers, headers, footers,",
    "and other obvious non-book overlays when they are not part of the book.",
    "Preserve paragraph breaks.",
    "Return only the extracted text.",
    "Do not add any introduction, explanation, summary, XML, markdown fences,",
    "or labels such as 'The following is the main body text from the PDF:'.",
    sep = " "
  ),
  model = "gpt-5-mini",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  temperature = 1,
  seed = 42,
  output_path = NULL,
  overwrite = FALSE
) {
  if (is.list(pdf_path)) {
    if (!is.null(output_path)) {
      validate_batch_output_path(output_path)
    }

    out <- vector("list", length(pdf_path))
    node_names <- names(pdf_path)

    for (i in seq_along(pdf_path)) {
      x <- pdf_path[[i]]
      child_output_path <- NULL
      if (!is.null(output_path)) {
        child_name <- if (!is.null(node_names) && nzchar(node_names[[i]])) {
          node_names[[i]]
        } else {
          NULL
        }
        child_output_path <- if (!is.null(child_name)) {
          file.path(output_path, child_name)
        } else {
          output_path
        }
      }

      out[[i]] <- extract_pdf_text_with_llm(
        pdf_path = x,
        prompt = prompt,
        model = model,
        integration = integration,
        virtual_key = virtual_key,
        base_url = base_url,
        temperature = temperature,
        seed = seed,
        output_path = child_output_path,
        overwrite = overwrite
      )
    }

    names(out) <- node_names
    return(out)
  }

  if (!is.character(pdf_path) || length(pdf_path) < 1 || any(!nzchar(pdf_path))) {
    stop("`pdf_path` must be a non-empty path, character vector of paths, or list of paths.")
  }

  if (length(pdf_path) > 1) {
    if (!is.null(output_path)) {
      validate_batch_output_path(output_path)
    }

    out <- character(length(pdf_path))
    for (i in seq_along(pdf_path)) {
      path <- pdf_path[[i]]
      out[[i]] <- extract_pdf_text_with_llm(
        pdf_path = path,
        prompt = prompt,
        model = model,
        integration = integration,
        virtual_key = virtual_key,
        base_url = base_url,
        temperature = temperature,
        seed = seed,
        output_path = output_path,
        overwrite = overwrite
      )
    }

    names(out) <- names(pdf_path)
    return(out)
  }

  if (!file.exists(pdf_path[[1]])) {
    stop("`pdf_path` does not exist: ", pdf_path[[1]])
  }
  if (!identical(tolower(tools::file_ext(pdf_path[[1]])), "pdf")) {
    stop("`pdf_path` must point to a `.pdf` file.")
  }
  if (!is.character(prompt) || length(prompt) != 1 || !nzchar(prompt)) {
    stop("`prompt` must be a single non-empty string.")
  }
  if (!is.null(output_path) &&
    (!is.character(output_path) || length(output_path) != 1 || !nzchar(output_path))) {
    stop("`output_path` must be NULL or a single non-empty string.")
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

  chat <- new_portkey_chat(
    model = model,
    base_url = base_url,
    temperature = temperature,
    seed = seed
  )

  prompts <- list(list(
    prompt,
    ellmer::content_pdf_file(pdf_path[[1]])
  ))

  text <- ellmer::parallel_chat_text(
    chat = chat,
    prompts = prompts,
    max_active = 1,
    rpm = 60,
    on_error = "stop"
  )[[1]]

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop("The model did not return a single text response.")
  }

  text <- gsub("\r\n?", "\n", text, perl = TRUE)
  text <- strip_pdf_preface_boilerplate(text)

  if (!is.null(output_path)) {
    output_path <- resolve_single_output_path(output_path, pdf_path[[1]])
    if (file.exists(output_path) && !isTRUE(overwrite)) {
      stop(
        "Output file already exists. Set `overwrite = TRUE` to replace it: ",
        output_path
      )
    }
    readr::write_lines(text, output_path)
  }

  text
}

strip_pdf_preface_boilerplate <- function(text) {
  pattern <- paste0(
    "^(?:\\s*",
    "The following is the main body text from the PDF:\\s*",
    ")+"
  )
  gsub(pattern, "", text, perl = TRUE)
}

resolve_single_output_path <- function(output_path, pdf_path) {
  ext <- tolower(tools::file_ext(output_path))
  if (dir.exists(output_path) || !nzchar(ext)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    return(file.path(
      output_path,
      paste0(tools::file_path_sans_ext(basename(pdf_path)), ".txt")
    ))
  }

  output_path
}

validate_batch_output_path <- function(output_path) {
  if (!is.character(output_path) || length(output_path) != 1 || !nzchar(output_path)) {
    stop("`output_path` must be NULL or a single non-empty string.")
  }
  if (nzchar(tools::file_ext(output_path)) && !dir.exists(output_path)) {
    stop(
      "When `pdf_path` has length > 1 or is a list, `output_path` must be a directory-like path without a file extension."
    )
  }
}
