# Calculate pixel location of a capital `E`

Draws a capital `E` via
[`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html)
with the specified font options, then calculates how far from the top
left corner of the text box the actual `E` pixels are rendered.

This is used for positioning figure labels precisely based on the actual
text spot (which `E` is used to stand in for) *without* considering
hanging letters like `g`.

## Usage

``` r
calculate_E_offset(
  font_size,
  font_family,
  font_style,
  font_weight,
  font_angle = 0
)
```

## Arguments

- font_size:

  `numeric`. The `size` argument passed to
  [`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html).

- font_family:

  `character`. The `font` argument passed to
  [`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html).

- font_style:

  `character`. The `style` argument passed to
  [`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html).

- font_weight:

  `numeric`. The `weight` argument passed to
  [`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html).

- font_angle:

  `numeric`. The `degrees` argument passed to
  [`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html).
  Default: `0`.

## Value

`list of numeric values`. The distance in pixels from the text box to
the `left`, `top`, `bottom`, and `right` of the `E`, as well as the
`height` and `width` of the `E`.
