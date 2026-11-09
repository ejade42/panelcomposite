# Check that panels in the same column have the same width and panels in the same row have the same height

Takes a list of images that represents panels in a grid-based figure,
arranged left to right and then top to bottom, e.g.


    1 2 3 4
    5 6 7 8

Checks that all panels in the same column have identical pixel widths
e.g. `1` and `5`, `2` and `6`, `3` and `7`, `4` and `8` each have
identical widths. Checks that all panels in the same row have identical
pixel heights e.g. `1` `2` `3` and `4`, `5` `6` `7` and `8` each have
identical heights.

Throws an error if there are non-identical widths within a column or
non-identical heights within a row. Otherwise, returns a named list of
numeric vectors, with `$col_widths` storing the pixel widths of columns
1 through `ncol` and `$row_heights` storing the pixel heights of rows 1
through `nrow`.

## Usage

``` r
check_panel_dimensions(dimensions_list, panels_count, nrow, ncol)
```

## Arguments

- dimensions_list:

  `list of numeric vectors`. A list of length-2 numeric vectors storing
  the width and height of each panel in the figure, as produced by
  [`get_image_dimensions()`](https://ejade42.github.io/panelcomposite/reference/get_image_dimensions.md).

- panels_count:

  `integer`. The total number of panels in the figure. Should be equal
  to `nrow * ncol`.

- nrow:

  `integer`. The number of rows in the figure.

- ncol:

  `integer`. The number of columns in the figure.

## Value

`list of numeric vectors`. A list of 2 numeric vectors: `$col_widths`
storing the pixel widths of columns 1 through `ncol`, and `$row_heights`
storing the pixel heights of rows 1 through `nrow`.
