# Replace point markers with images on a ggplot

Replace point markers with images on a ggplot

## Usage

``` r
add_point_images(
  p,
  group,
  point_images,
  image_size = 0.04,
  image_nudge_x = 0,
  image_nudge_y = 0,
  image_jitter_width = 0,
  image_jitter_height = 0,
  facet = NULL
)
```

## Arguments

- p:

  A ggplot object produced by
  [`plot_chapters_over_time()`](https://centerconflictcooperation.github.io/nalanda/reference/plot_chapters_over_time.md).

- group:

  Character. Name of the grouping variable.

- point_images:

  Named list mapping group levels to image file paths.

- image_size:

  Numeric. Size passed to
  [`ggimage::geom_image()`](https://rdrr.io/pkg/ggimage/man/geom_image.html).

- image_nudge_x:

  Numeric. Horizontal offset applied to point images.

- image_nudge_y:

  Numeric. Vertical offset applied to point images.

- image_jitter_width:

  Numeric. Horizontal jitter width for point images.

- image_jitter_height:

  Numeric. Vertical jitter height for point images.

- facet:

  Character or `NULL`. Name of the facet variable.

## Value

The modified ggplot object with images instead of points.
