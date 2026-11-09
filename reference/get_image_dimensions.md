# Determine the pixel dimensions of images in a list

Takes a list of image objects and calculates their pixel widths and
heights via
[`magick::image_info()`](https://docs.ropensci.org/magick/reference/attributes.html).
Returns a list of `c(width, height)` numeric vectors.

## Usage

``` r
get_image_dimensions(images_list)
```

## Arguments

- images_list:

  `list of image objects`. A list of the images whose dimensions should
  be calculated, as produced by
  [`read_many_images()`](https://ejade42.github.io/panelcomposite/reference/read_many_images.md).

## Value

`list of numeric vectors`. A list of length-2 numeric vectors storing
the width and height of each image in the input list.
