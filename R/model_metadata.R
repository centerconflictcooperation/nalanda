# Normalize model metadata for storage/display. Fully-qualified Portkey model
# strings should be reduced to the actual model name in nalanda attributes.
normalize_model_name <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }

  model <- as.character(model)[1]
  if (!nzchar(model)) {
    return(model)
  }

  sub("^@?[^/]+/", "", model)
}

resolve_model_route <- function(
  integration,
  virtual_key,
  integration_missing = FALSE,
  virtual_key_missing = FALSE
) {
  has_integration_arg <- !integration_missing &&
    !is.null(integration) &&
    nzchar(integration)
  has_virtual_key_arg <- !virtual_key_missing &&
    !is.null(virtual_key) &&
    nzchar(virtual_key)

  if (has_integration_arg && has_virtual_key_arg) {
    stop("Please provide only one of `integration` or `virtual_key`.")
  }

  if (has_integration_arg) {
    virtual_key <- NULL
  } else if (has_virtual_key_arg) {
    integration <- NULL
  } else if (!is.null(integration) && nzchar(integration) &&
    !is.null(virtual_key) && nzchar(virtual_key)) {
    message(
      "Both `nalanda.integration` and `nalanda.virtual_key` options are set; ",
      "prioritizing `integration`."
    )
    virtual_key <- NULL
  }

  list(
    integration = integration,
    virtual_key = virtual_key
  )
}

expand_model_specs <- function(
  model,
  integration,
  virtual_key,
  integration_missing = FALSE,
  virtual_key_missing = FALSE
) {
  model <- as.character(model)
  if (length(model) < 1 || any(is.na(model)) || any(!nzchar(model))) {
    stop("`model` must contain at least one non-empty string.")
  }

  n_models <- length(model)

  recycle_arg <- function(x, name) {
    if (is.null(x)) {
      return(rep(list(NULL), n_models))
    }

    x <- as.character(x)
    if (length(x) == 1) {
      return(as.list(rep(x, n_models)))
    }
    if (length(x) == n_models) {
      return(as.list(x))
    }

    stop(
      "`", name, "` must have length 1 or length(model) (",
      n_models,
      ")."
    )
  }

  integration_vec <- recycle_arg(integration, "integration")
  virtual_key_vec <- recycle_arg(virtual_key, "virtual_key")

  specs <- vector("list", n_models)
  for (i in seq_len(n_models)) {
    route <- resolve_model_route(
      integration = integration_vec[[i]],
      virtual_key = virtual_key_vec[[i]],
      integration_missing = integration_missing,
      virtual_key_missing = virtual_key_missing
    )

    model_i <- model[[i]]
    if (!startsWith(model_i, "@")) {
      prefix <- NULL
      if (!is.null(route$integration) && nzchar(route$integration)) {
        prefix <- route$integration
      } else if (!is.null(route$virtual_key) && nzchar(route$virtual_key)) {
        prefix <- route$virtual_key
      }
      if (!is.null(prefix)) {
        model_i <- paste0("@", prefix, "/", model_i)
      }
    }

    specs[[i]] <- list(
      model_input = model[[i]],
      model = model_i,
      model_label = normalize_model_name(model_i),
      integration = route$integration,
      virtual_key = route$virtual_key
    )
  }

  specs
}

normalize_model_metadata <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(NULL)
  }

  unique(vapply(x, normalize_model_name, character(1)))
}
