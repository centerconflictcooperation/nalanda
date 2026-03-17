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
