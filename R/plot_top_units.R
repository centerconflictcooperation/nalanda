#' Plot units that rank consistently high across models
#'
#' Creates a dot plot from [summarize_top_units()] output. Units are ordered by
#' average rank across models, point size shows the mean score, and text labels
#' show how many models placed the unit in the top `N`.
#'
#' @param data Output of [summarize_top_units()].
#' @param item_col Character. Column identifying the ranked item. If `NULL`,
#'   the function tries to infer `"book"` or `"book_id"`.
#' @param facet_by Optional character vector of columns to facet by, e.g.
#'   `"party"`.
#' @param top_n_items Optional integer. If supplied, keep only the best
#'   `top_n_items` per facet, based on `mean_rank`.
#' @param title Optional plot title.
#' @param x_breaks Optional numeric vector of x-axis breaks. If `NULL`, integer
#'   rank breaks are shown by default.
#' @param x_limits Optional numeric vector of length 2. If `NULL`, limits are
#'   chosen from the displayed mean ranks.
#' @param caption Optional plot caption. If `NULL`, a caption explaining the
#'   point-size and top-N label encodings is generated when possible.
#' @param show_top_n_label Logical. If `TRUE` (default), label points with the
#'   number of models placing the item in the top `N`.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @examples
#' \dontrun{
#' top_books <- summarize_top_units(
#'   agg,
#'   outcome = "mean_delta_gap",
#'   item_by = "book",
#'   rank_within = "party"
#' )
#' plot_top_units(top_books, item_col = "book", facet_by = "party")
#' }
plot_top_units <- function(data,
                           item_col = NULL,
                           facet_by = NULL,
                           top_n_items = NULL,
                           title = "Units most consistently ranked highest",
                           x_breaks = NULL,
                           x_limits = NULL,
                           caption = NULL,
                           show_top_n_label = TRUE) {
  stopifnot(is.data.frame(data))
  item_col <- resolve_top_unit_item_col(data, item_col)

  required <- c(item_col, facet_by, "mean_score", "mean_rank", "top_n_models", "n_models")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  plot_data <- filter_top_unit_items(data, item_col, facet_by, top_n_items)
  plot_data <- order_top_unit_items(plot_data, item_col)
  x_scale <- top_unit_x_scale(plot_data$mean_rank, x_breaks = x_breaks, x_limits = x_limits)
  if (is.null(caption)) {
    caption <- top_unit_caption(plot_data)
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["mean_rank"]],
      y = .data[["..item_label.."]],
      size = .data[["mean_score"]]
    )
  ) +
    ggplot2::geom_point(
      shape = 21,
      fill = "#2b8cbe",
      color = "white",
      stroke = 0.7,
      alpha = 0.9
    ) +
    ggplot2::scale_size_continuous(
      range = c(2.5, 9),
      name = "Mean evaluation\nscore",
      guide = ggplot2::guide_legend(
        override.aes = list(fill = "#6baed6", color = "#6baed6", alpha = 1)
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = x_scale$limits,
      breaks = x_scale$breaks,
      expand = ggplot2::expansion(mult = c(0.02, 0.06))
    ) +
    ggplot2::labs(
      title = title,
      x = "Mean rank (lower = better)",
      y = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#eeeeee", linewidth = 0.4),
      plot.caption = ggplot2::element_text(color = "#666666", hjust = 0)
    )

  if (isTRUE(show_top_n_label) && "top_n_models" %in% names(plot_data)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = .data[["top_n_models"]]),
        color = "white",
        size = 3,
        fontface = "bold"
      )
  }

  add_top_unit_facets(p, facet_by)
}

top_unit_caption <- function(data) {
  if (!("top_n" %in% names(data))) {
    return("Point labels show how many models ranked the item in their top N; point size shows mean evaluation score.")
  }

  top_n_values <- unique(stats::na.omit(data$top_n))
  if (length(top_n_values) != 1) {
    return("Point labels show how many models ranked the item in their top N; point size shows mean evaluation score.")
  }

  paste0(
    "Point labels show how many models ranked the item in their top ",
    top_n_values[[1]],
    "; point size shows mean evaluation score."
  )
}

top_unit_x_scale <- function(x, x_breaks = NULL, x_limits = NULL) {
  finite_x <- x[is.finite(x)]
  if (length(finite_x) == 0) {
    return(list(limits = x_limits, breaks = x_breaks))
  }

  if (is.null(x_limits)) {
    x_limits <- c(floor(min(finite_x)), ceiling(max(finite_x)))
  }
  if (is.null(x_breaks)) {
    x_breaks <- seq(floor(x_limits[[1]]), ceiling(x_limits[[2]]), by = 1)
  }

  list(limits = x_limits, breaks = x_breaks)
}

#' Plot model-by-unit rank heatmap
#'
#' Creates a heatmap from the `ranks` element returned by
#' `summarize_top_units(..., include_ranks = TRUE)`. Rows are units, columns are
#' models, and cells show each model's rank for that unit.
#'
#' @param data The `ranks` data frame from [summarize_top_units()] with
#'   `include_ranks = TRUE`.
#' @param item_col Character. Column identifying the ranked item. If `NULL`,
#'   the function tries to infer `"book"` or `"book_id"`.
#' @param model_col Character. Column identifying the model (default
#'   `"model"`).
#' @param facet_by Optional character vector of columns to facet by, e.g.
#'   `"party"`.
#' @param top_n_items Optional integer. If supplied, keep only the best
#'   `top_n_items` per facet, based on average rank across models.
#' @param show_values Logical. If `TRUE` (default), print rank values in cells.
#' @param title Optional plot title.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @examples
#' \dontrun{
#' top_books <- summarize_top_units(
#'   agg,
#'   outcome = "mean_delta_gap",
#'   item_by = "book",
#'   rank_within = "party",
#'   include_ranks = TRUE
#' )
#' plot_top_unit_heatmap(top_books$ranks, item_col = "book", facet_by = "party")
#' }
plot_top_unit_heatmap <- function(data,
                                  item_col = NULL,
                                  model_col = "model",
                                  facet_by = NULL,
                                  top_n_items = NULL,
                                  show_values = TRUE,
                                  title = "Unit ranks by model") {
  stopifnot(is.data.frame(data))
  item_col <- resolve_top_unit_item_col(data, item_col)

  required <- c(item_col, model_col, facet_by, "rank")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  rank_summary <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(facet_by, item_col)))) |>
    dplyr::summarise(mean_rank = .mean_or_na(.data$rank), .groups = "drop")

  keep_items <- filter_top_unit_items(
    rank_summary,
    item_col = item_col,
    facet_by = facet_by,
    top_n_items = top_n_items
  ) |>
    dplyr::select(dplyr::all_of(c(facet_by, item_col)))

  plot_data <- data |>
    dplyr::inner_join(keep_items, by = c(facet_by, item_col)) |>
    dplyr::left_join(rank_summary, by = c(facet_by, item_col))
  plot_data <- order_top_unit_items(plot_data, item_col)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[model_col]],
      y = .data[["..item_label.."]],
      fill = .data[["rank"]]
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::scale_fill_gradient(
      low = "#1f78b4",
      high = "#f7fbff",
      na.value = "#eeeeee",
      name = "Rank"
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (isTRUE(show_values)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(is.na(.data[["rank"]]), "", round(.data[["rank"]], 1))),
        size = 3
      )
  }

  add_top_unit_facets(p, facet_by)
}

#' Plot paired subgroup ranks for top units
#'
#' Creates a connected Cleveland dot plot from [summarize_top_units()] output
#' when rankings were computed within a two-level subgroup, such as party. Each
#' row is an item, dots show subgroup-specific mean ranks, and connecting lines
#' show how much the ranking differs between subgroups.
#'
#' @param data Output of [summarize_top_units()] with a subgroup column such as
#'   `"party"`.
#' @param item_col Character. Column identifying the ranked item. If `NULL`,
#'   the function tries to infer `"book"` or `"book_id"`.
#' @param subgroup_col Character. Two-level subgroup column to connect, e.g.
#'   `"party"`.
#' @param top_n_items Optional integer. If supplied, keep only items with the
#'   best average `mean_rank` across subgroups.
#' @param subgroup_order Optional character vector giving the two subgroup
#'   levels in display order.
#' @param title Optional plot title.
#' @param x_breaks Optional numeric vector of x-axis breaks. If `NULL`, integer
#'   rank breaks are shown by default.
#' @param x_limits Optional numeric vector of length 2. If `NULL`, limits are
#'   chosen from the displayed mean ranks.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @examples
#' \dontrun{
#' top_books_party <- summarize_top_units(
#'   agg,
#'   outcome = "mean_delta_gap",
#'   item_by = "book",
#'   rank_within = "party"
#' )
#' plot_top_unit_pairs(top_books_party, item_col = "book", subgroup_col = "party")
#' }
plot_top_unit_pairs <- function(data,
                                item_col = NULL,
                                subgroup_col = "party",
                                top_n_items = NULL,
                                subgroup_order = NULL,
                                title = "Paired subgroup ranks",
                                x_breaks = NULL,
                                x_limits = NULL) {
  stopifnot(is.data.frame(data))
  item_col <- resolve_top_unit_item_col(data, item_col)

  required <- c(item_col, subgroup_col, "mean_rank")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  subgroup_levels <- unique(as.character(stats::na.omit(data[[subgroup_col]])))
  if (!is.null(subgroup_order)) {
    missing_levels <- setdiff(subgroup_order, subgroup_levels)
    if (length(missing_levels) > 0) {
      stop(
        "`subgroup_order` contains level(s) not found in `data`: ",
        paste(missing_levels, collapse = ", "),
        call. = FALSE
      )
    }
    subgroup_levels <- subgroup_order
  }
  if (length(subgroup_levels) != 2) {
    stop(
      "`subgroup_col` must contain exactly two non-missing levels for a paired plot.",
      call. = FALSE
    )
  }

  item_summary <- data |>
    dplyr::filter(.data[[subgroup_col]] %in% subgroup_levels) |>
    dplyr::group_by(.data[[item_col]]) |>
    dplyr::summarise(
      mean_rank_overall = .mean_or_na(.data$mean_rank),
      n_subgroups = dplyr::n_distinct(.data[[subgroup_col]][!is.na(.data$mean_rank)]),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_subgroups == 2)

  if (!is.null(top_n_items)) {
    item_summary <- filter_top_unit_items(
      dplyr::rename(item_summary, mean_rank = "mean_rank_overall"),
      item_col = item_col,
      top_n_items = top_n_items
    ) |>
      dplyr::rename(mean_rank_overall = "mean_rank")
  }

  plot_data <- data |>
    dplyr::inner_join(item_summary[, c(item_col, "mean_rank_overall"), drop = FALSE],
      by = item_col
    ) |>
    dplyr::filter(.data[[subgroup_col]] %in% subgroup_levels)

  item_order <- item_summary |>
    dplyr::arrange(dplyr::desc(.data$mean_rank_overall), .data[[item_col]])
  item_order <- item_order[[item_col]]

  plot_data <- plot_data |>
    dplyr::mutate(
      "..item_label.." = factor(.data[[item_col]], levels = item_order),
      "..subgroup_label.." = factor(.data[[subgroup_col]], levels = subgroup_levels)
    )

  x_scale <- top_unit_x_scale(
    plot_data$mean_rank,
    x_breaks = x_breaks,
    x_limits = x_limits
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$mean_rank,
      y = .data[["..item_label.."]]
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data[[item_col]]),
      color = "#bdbdbd",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[["..subgroup_label.."]]),
      size = 3.5,
      alpha = 0.95
    ) +
    ggplot2::scale_color_manual(
      values = top_unit_pair_palette(subgroup_levels),
      name = subgroup_col
    ) +
    ggplot2::scale_x_continuous(
      limits = x_scale$limits,
      breaks = x_scale$breaks,
      expand = ggplot2::expansion(mult = c(0.02, 0.06))
    ) +
    ggplot2::labs(
      title = title,
      x = "Mean rank (lower = better)",
      y = NULL,
      caption = "Lines connect subgroup-specific mean ranks for the same item."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#eeeeee", linewidth = 0.4),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(color = "#666666", hjust = 0)
    )
}

resolve_top_unit_item_col <- function(data, item_col = NULL) {
  if (!is.null(item_col)) {
    return(item_col)
  }

  candidates <- c("book", "book_id")
  present <- candidates[candidates %in% names(data)]
  if (length(present) > 0) {
    return(present[[1]])
  }

  stop(
    "`item_col` could not be inferred. Please provide the item column name.",
    call. = FALSE
  )
}

filter_top_unit_items <- function(data, item_col, facet_by = NULL, top_n_items = NULL) {
  if (is.null(top_n_items)) {
    return(data)
  }
  if (!is.numeric(top_n_items) || length(top_n_items) != 1 ||
    is.na(top_n_items) || top_n_items < 1) {
    stop("`top_n_items` must be a positive number.", call. = FALSE)
  }

  grouping <- facet_by
  if (is.null(grouping) || length(grouping) == 0) {
    data |>
      dplyr::slice_min(.data$mean_rank, n = as.integer(top_n_items), with_ties = FALSE)
  } else {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
      dplyr::slice_min(.data$mean_rank, n = as.integer(top_n_items), with_ties = FALSE) |>
      dplyr::ungroup()
  }
}

order_top_unit_items <- function(data, item_col) {
  item_order <- data |>
    dplyr::group_by(.data[[item_col]]) |>
    dplyr::summarise(mean_rank = .mean_or_na(.data$mean_rank), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mean_rank), .data[[item_col]])
  item_order <- item_order[[item_col]]

  data |>
    dplyr::mutate(
      "..item_label.." := factor(.data[[item_col]], levels = item_order)
    )
}

add_top_unit_facets <- function(plot, facet_by = NULL) {
  if (is.null(facet_by) || length(facet_by) == 0) {
    return(plot)
  }

  plot +
    ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(facet_by, collapse = " + "))),
      scales = "free_y"
    )
}

top_unit_pair_palette <- function(levels) {
  lower_levels <- tolower(levels)
  if (all(lower_levels %in% c("democrat", "republican"))) {
    values <- c("democrat" = "#00AEF3", "republican" = "#E81B23")
    return(stats::setNames(unname(values[lower_levels]), levels))
  }

  values <- c("#2b8cbe", "#d95f0e")
  stats::setNames(values[seq_along(levels)], levels)
}
