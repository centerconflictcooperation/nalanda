# Save a forest plot to PNG and PDF formats

Exports a forestplot object to both PNG and PDF files using grid
graphics devices.

## Usage

``` r
save_forest_plot(
  plot_object,
  filename,
  width = 16/1.8,
  height = 9/1.8,
  res = 300
)
```

## Arguments

- plot_object:

  A forestplot grob object.

- filename:

  Character string specifying the file path without extension.

- width:

  Width of the output figure in inches.

- height:

  Height of the output figure in inches.

- res:

  Resolution in DPI for the PNG output (default = 300).

## Details

Because `forestplot` uses grid graphics (not ggplot2), `ggsave()` is not
compatible. This function opens graphics devices manually and prints the
plot object.

Two files are created:

- `filename.png`:

  High-resolution raster image

- `filename.pdf`:

  Vector-based PDF
