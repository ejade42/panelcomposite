# Calculate width of rendered text

Takes a string of text to render via
[`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html)
with the specified options, and calculates the pixel width of the text.

## Usage

``` r
get_text_width(text, font_size, font_family, font_style, font_weight)
```

## Arguments

- text:

  `character`. The text to be rendered, i.e. the `text` argument passed
  to
  [`magick::image_annotate()`](https://docs.ropensci.org/magick/reference/painting.html).

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

## Value

`integer`. The pixel width of the specified text.
