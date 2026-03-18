#' Plot chapters over time (multi-timepoint means)
#'
#' Create a plot showing means over chapter timepoints using rempsyc::plot_means_over_time
#' for the wide-format response variables.
#'
#' @param chapters A data frame or list of processed simulation rows, typically
#'   returned by [compute_run_ai_metrics()], containing columns `book`,
#'   `chapter`, and the desired `dv`. If a list is supplied, the function will
#'   attempt to combine its data-frame elements before plotting.
#' @param dv Character. Name of the column to plot as the dependent variable (default: "pre_post_outgroup_difference").
#' @param group The group by which to plot the variable
#' @param x_label Character. X-axis label.
#' @param y_label Character. Y-axis label.
#' @param plot_title Optional character title. If `NULL` (default) or `FALSE`,
#'   no title is added.
#' @param plot_subtitle Optional plot subtitle.
#' @param append_model_info Logical. If `TRUE` (default), append model and
#'   temperature attributes to the subtitle (or create one if none is provided).
#' @param ci_type Character. Type of confidence interval to pass to `rempsyc::plot_means_over_time`.
#' @param legend.position Position for legend.
#' @param groups.order Specifies the desired display order of the groups
#' on the legend. Either provide the levels directly, or a string: "increasing"
#' or "decreasing", to order based on the average value of the variable on the
#' y axis, or "string.length", to order from the shortest to the longest
#' string (useful when working with long string names). "Defaults to "decreasing".
#' @param text_size Numeric. Base text size for axis/title text.
#' @param line_width Numeric. Line thickness used in `geom_line()`.
#'   Defaults to 3. Can be reduced for publication figures or increased
#'   for presentation slides.
#' @param point_size Numeric. Point size used in `geom_point()`.
#'   Defaults to 4. Adjust to improve readability depending on output format.
#' @param reverse_score Logical. Whether to reverse score scale.
#' @param error_bars Logical. Show error bars.
#' @param neutrality_line Logical. Add a horizontal neutrality line at 50.
#' @param facet The variable by which to facet grid.
#' @param facet_ncol Optional numeric value passed to `ggplot2::facet_wrap()`
#'   as `ncol` when `facet` is supplied.
#' @param point_images Optional named list mapping group levels to image file
#'   paths (PNG recommended). When supplied, the point markers are replaced with
#'   the corresponding images, and the legend labels are updated to show the
#'   matching image alongside the group name when `ggtext` is installed. Example:
#'   `list(Democrat = "logos/dem.png", Republican = "logos/rep.png")`.
#' @param image_size Numeric. Size of images when `point_images` is used.
#'   Passed to [ggimage::geom_image()]. Defaults to `0.04`.
#' @param image_nudge_x Numeric. Horizontal offset applied to point images only.
#'   Defaults to `0`.
#' @param image_nudge_y Numeric. Vertical offset applied to point images only.
#'   Defaults to `0`.
#' @param image_jitter_width Numeric. Horizontal jitter width applied to point
#'   images only. Defaults to `0`.
#' @param image_jitter_height Numeric. Vertical jitter height applied to point
#'   images only. Defaults to `0`.
#' @param facets.order Specifies the desired display order of facet panels.
#'   Either provide the levels directly, or a string: "increasing" or
#'   "decreasing", to order panels based on the average value of the
#'   y variable, or "string.length" to order panels by facet label length.
#'   Defaults to "increasing".
#' @return A ggplot2 object.
#'
#' @examples
#' plot_chapters_over_time(
#'   toy_sim_results,
#'   dv = "delta_outgroup",
#'   group = "party",
#'   facet = "book",
#'   y_label = "Outgroup change"
#' )
#' @export
plot_chapters_over_time <- function(
  chapters,
  dv = "delta_gap",
  group = "book",
  x_label = "Chapter",
  y_label = "Simulated scores",
  plot_title = NULL,
  plot_subtitle = "",
  append_model_info = TRUE,
  ci_type = "between",
  legend.position = "bottom",
  groups.order = "decreasing",
  text_size = 20,
  line_width = 3,
  point_size = 4,
  reverse_score = FALSE,
  error_bars = TRUE,
  neutrality_line = TRUE,
  point_images = NULL,
  image_size = 0.04,
  image_nudge_x = 0,
  image_nudge_y = 0,
  image_jitter_width = 0,
  image_jitter_height = 0,
  facet = NULL,
  facet_ncol = NULL,
  facets.order = "increasing"
) {
  input <- chapters

  # Extract metadata before binding (works for tibble, list-of-tibbles, or
  # individual tibbles loaded via readRDS and reassembled into a plain list)
  model_name <- attr(input, "model")
  model_temp <- attr(input, "temperature")

  # Fallback: check first list element (covers lapply(files, readRDS) workflow)
  if (is.null(model_name) && is.list(input) && !inherits(input, "data.frame") &&
    length(input) > 0) {
    first <- input[[1]]
    model_name <- rlang::`%||%`(model_name, attr(first, "model"))
    model_temp <- rlang::`%||%`(model_temp, attr(first, "temperature"))
  }
  model_name <- normalize_model_name(model_name)

  df <- bind_simulation_results(input)
  if (!"book" %in% names(df)) {
    df$book <- "book_1"
  }

  dv_column <- dv
  if (!dv_column %in% names(df)) {
    # Fallback candidates in the new schema
    candidate <- intersect(
      c(
        "delta_gap",
        "delta_outgroup",
        "post_outgroup",
        "pre_outgroup"
      ),
      names(df)
    )
    if (length(candidate) == 0) {
      stop(
        "Column '",
        dv,
        "' not found and no fallback columns detected.\n",
        "Available columns: ",
        paste(names(df), collapse = ", ")
      )
    }
    dv_column <- candidate[1]
  }
  df <- df |>
    dplyr::mutate(score = .data[[dv_column]])
  if (reverse_score) {
    df <- df |>
      dplyr::mutate(score = .data$score * -1)
  }
  chapter_lookup <- df |>
    dplyr::distinct(.data$book, .data$chapter) |>
    dplyr::group_by(.data$book) |>
    dplyr::group_modify(function(.x, .y) {
      .x$chapter_num <- validate_chapter_order(
        .x$chapter,
        book = .y$book[[1]],
        arg_name = "`chapters$chapter`"
      )
      .x
    }) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::left_join(chapter_lookup, by = c("book", "chapter")) |>
    dplyr::arrange(.data$book, .data$chapter_num, .data$sim) |>
    dplyr::group_by(.data$book) |>
    dplyr::mutate(chapter_index = dplyr::dense_rank(.data$chapter_num)) |>
    dplyr::ungroup()

  # Create a unique simulation ID to handle cases with multiple identities/parties per sim
  # This ensures pivot_wider has a unique key for each row
  grouping_cols <- intersect(
    c("book", "sim", "identity", "party"),
    names(df)
  )
  if (length(grouping_cols) > 0) {
    df <- df |>
      dplyr::group_by(dplyr::pick(dplyr::all_of(grouping_cols))) |>
      dplyr::mutate(sim_unique_id = dplyr::cur_group_id()) |>
      dplyr::ungroup()
  } else {
    df$sim_unique_id <- df$sim
  }

  df_wide <- df |>
    dplyr::mutate(time_var = paste0("T", .data$chapter_index)) |>
    dplyr::select(
      dplyr::all_of(c("book", "sim_unique_id", "time_var", "score")),
      dplyr::any_of("party")
    ) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = "time_var", values_from = "score")

  response_cols <- grep("^T[0-9]+$", names(df_wide), value = TRUE)

  # Auto-detect grouping variable
  group <- group

  # Update x-axis label if grouping by party
  if (group == "party" && is.null(facet)) {
    book_name <- unique(df_wide[["book"]])[1]
    x_label <- paste0(x_label, " (", book_name, ")")
  }

  p <- rempsyc::plot_means_over_time(
    data = df_wide,
    response = response_cols,
    group = group,
    ytitle = y_label,
    ci_type = ci_type,
    legend.position = legend.position,
    error_bars = error_bars,
    line_width = line_width,
    point_size = point_size,
    groups.order = groups.order,
    facet = facet,
    facets.order = facets.order
  ) +
    ggplot2::labs(x = x_label) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(size = text_size)
    )
  if (is.character(plot_title) && length(plot_title) == 1 && nzchar(plot_title)) {
    p <- p + ggplot2::labs(title = plot_title)
  }
  if (
    isTRUE(neutrality_line) &&
      !grepl("difference|diff|delta|gap", dv, ignore.case = TRUE)
  ) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 50,
        linetype = "dashed",
        linewidth = 0.6,
        color = "grey40"
      ) +
      ggplot2::annotate(
        "text",
        x = 1,
        y = 52,
        label = "Neutral (50)",
        color = "grey30",
        hjust = 0,
        size = 3
      )
  }

  # Add line at 0 for difference scores
  if (
    isTRUE(neutrality_line) &&
      grepl("difference|diff|delta|gap", dv, ignore.case = TRUE)
  ) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.6,
        color = "grey40"
      ) +
      ggplot2::annotate(
        "text",
        x = 1,
        y = 2, # Slightly above 0
        label = "No Change (0)",
        color = "grey30",
        hjust = 0,
        size = 3
      )
  }

  subtitle_value <- NULL
  if (!missing(plot_subtitle)) {
    subtitle_value <- plot_subtitle
  }

  if (isTRUE(append_model_info)) {
    model_info <- paste0(
      "model = \"",
      rlang::`%||%`(model_name, "unknown"),
      "\"; temperature = ",
      rlang::`%||%`(model_temp, "unknown")
    )
    subtitle_value <- if (!is.null(subtitle_value) && nzchar(subtitle_value)) {
      paste0(subtitle_value, " (", model_info, ")")
    } else {
      paste0("(", model_info, ")")
    }
  }

  if (!is.null(subtitle_value)) {
    p <- p +
      ggplot2::labs(subtitle = subtitle_value) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 16))
  }

  if (group %in% names(df_wide)) {
    group_levels <- tolower(levels(as.factor(df_wide[[group]])))
    if (all(group_levels %in% c("democrat", "republican"))) {
      p <- suppress_party_shape_scale_message(
        p +
          ggplot2::scale_colour_manual(
            values = c("Democrat" = "#00AEF3", "Republican" = "#E81B23")
          ) +
          ggplot2::scale_shape_manual(
            values = c("Democrat" = 21, "Republican" = 24)
          ) +
          ggplot2::theme(
            panel.spacing = ggplot2::unit(1.2, "lines"),
            axis.line = ggplot2::element_line(linewidth = 0.6),
            axis.ticks = ggplot2::element_line(linewidth = 0.6),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(face = "plain")
          ) +
          ggplot2::labs(colour = NULL, shape = NULL, linetype = NULL)
      )
    }
  }
  # Optional faceting
  if (!is.null(facet) && facet %in% names(df)) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste("~", facet)),
      ncol = facet_ncol
    )
  }

  # Replace point markers with images if requested
  if (!is.null(point_images)) {
    rlang::check_installed("ggimage", reason = "for point_images support.")
    p <- add_point_images(
      p = p,
      group = group,
      point_images = point_images,
      image_size = image_size,
      image_nudge_x = image_nudge_x,
      image_nudge_y = image_nudge_y,
      image_jitter_width = image_jitter_width,
      image_jitter_height = image_jitter_height,
      facet = facet
    )
    p <- add_image_legend_labels(
      p = p,
      point_images = point_images
    )
  }

  p
}

bind_simulation_results <- function(chapters) {
  if (is.list(chapters) && !inherits(chapters, "data.frame")) {
    return(tibble::as_tibble(flatten_sim_results(chapters)))
  } else {
    return(tibble::as_tibble(chapters))
  }
}

suppress_party_shape_scale_message <- function(plot_object) {
  withCallingHandlers(
    plot_object,
    message = function(m) {
      msg <- conditionMessage(m)
      if (grepl("^Scale for shape is already present\\.", msg)) {
        invokeRestart("muffleMessage")
      }
    }
  )
}

#' Replace point markers with images on a ggplot
#'
#' @param p A ggplot object produced by `plot_chapters_over_time()`.
#' @param group Character. Name of the grouping variable.
#' @param point_images Named list mapping group levels to image file paths.
#' @param image_size Numeric. Size passed to [ggimage::geom_image()].
#' @param image_nudge_x Numeric. Horizontal offset applied to point images.
#' @param image_nudge_y Numeric. Vertical offset applied to point images.
#' @param image_jitter_width Numeric. Horizontal jitter width for point images.
#' @param image_jitter_height Numeric. Vertical jitter height for point images.
#' @param facet Character or `NULL`. Name of the facet variable.
#' @return The modified ggplot object with images instead of points.
#' @keywords internal
add_point_images <- function(
  p,
  group,
  point_images,
  image_size = 0.04,
  image_nudge_x = 0,
  image_nudge_y = 0,
  image_jitter_width = 0,
  image_jitter_height = 0,
  facet = NULL
) {
  # Retrieve the plot data (data_summary from rempsyc)
  plot_data <- p$data

  # Map group levels to image paths (ensure it's a character vector, not a list)
  plot_data$image <- unlist(unname(point_images[as.character(plot_data[[
    group
  ]])]))

  # Validate that all groups were matched
  if (any(is.na(plot_data$image))) {
    missing <- setdiff(
      unique(as.character(plot_data[[group]])),
      names(point_images)
    )
    warning(
      "No image path found for group level(s): ",
      paste(missing, collapse = ", "),
      ". Those points will not display images."
    )
  }

  # Remove existing geom_point layer(s)
  point_idx <- which(vapply(
    p$layers,
    function(l) {
      inherits(l$geom, "GeomPoint")
    },
    logical(1)
  ))
  if (length(point_idx) > 0) {
    p$layers[point_idx] <- NULL
  }

  # Convert discrete time positions only when image offsets are requested
  x_base <- plot_data$Time
  if (isTRUE(image_nudge_x != 0 || image_jitter_width > 0)) {
    x_base <- if (is.factor(plot_data$Time)) {
      as.numeric(plot_data$Time)
    } else if (is.character(plot_data$Time)) {
      suppressWarnings(as.numeric(plot_data$Time))
    } else {
      plot_data$Time
    }
  }

  # Keep image positioning explicit so logos can be nudged or jittered
  plot_data$x_image <- if (
    isTRUE(image_nudge_x != 0 || image_jitter_width > 0)
  ) {
    x_base + image_nudge_x
  } else {
    x_base
  }
  plot_data$y_image <- plot_data$value + image_nudge_y

  if (isTRUE(image_jitter_width > 0 || image_jitter_height > 0)) {
    plot_data$x_image <- jitter(
      plot_data$x_image,
      amount = image_jitter_width
    )
    plot_data$y_image <- jitter(
      plot_data$y_image,
      amount = image_jitter_height
    )
  }

  p <- p +
    ggimage::geom_image(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$x_image,
        y = .data$y_image,
        image = .data$image
      ),
      size = image_size,
      inherit.aes = FALSE
    )

  p
}

add_image_legend_labels <- function(p, point_images) {
  if (!requireNamespace("ggtext", quietly = TRUE)) {
    return(p)
  }

  image_labels <- vapply(
    names(point_images),
    function(group_name) {
      image_path <- normalizePath(
        point_images[[group_name]],
        winslash = "/",
        mustWork = FALSE
      )
      paste0(
        "<img src='",
        image_path,
        "' width='20' style='vertical-align:middle;'/> ",
        group_name
      )
    },
    character(1)
  )

  colour_scale <- p$scales$get_scales("colour")
  if (!is.null(colour_scale)) {
    colour_scale$labels <- image_labels
    colour_scale$guide <- ggplot2::guide_legend(
      override.aes = list(
        alpha = 0,
        linewidth = 0,
        linetype = 0,
        shape = NA,
        size = 0
      )
    )
  }

  p +
    ggplot2::theme(
      legend.text = ggtext::element_markdown(
        margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
      ),
      legend.key.width = grid::unit(0.01, "pt"),
      legend.key.height = grid::unit(20, "pt"),
      legend.spacing.x = grid::unit(6, "pt"),
      legend.margin = ggplot2::margin(t = -6, r = 0, b = 0, l = 0),
      legend.box.margin = ggplot2::margin(t = -8, r = 0, b = 0, l = 0)
    ) +
    ggplot2::guides(
      shape = "none",
      linetype = "none"
    )
}
