# Multiply a grayscale image by a specified colour

Takes a grayscale image and multiplies it by a specified colour, with
the option to modulate the brightness.

## Usage

``` r
multiply_by_colour(img_gray, colour_hex = "#FFFFFF", brightness = 100)
```

## Arguments

- img_gray:

  `image object`. A grayscale image, e.g. produced by
  [`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html),
  to be tinted the given colour.

- colour_hex:

  `character`. A hex code e.g. `#FF0000` to recolour the grayscale
  image. Default `#FFFFFF` (white) will not change the image. Colour
  keywords e.g. `"red"` can work, but `magick` uses a different library
  of keywords (W3C) compared to RStudio preview colours (X11), so
  preview colours may not match the actual output colour. In particular,
  `"green"` maps to `"#00FF00"` in RStudio but `"#008000"` in `magick`.

- brightness:

  `numeric`. Brightness modulation to be applied to the image. `100`
  (default) does not alter brightness; higher values increase
  brightness; lower values decrease brightness.

## Value

`image object`. The image with the specified colour tinting and
brightness modulation applied.

## Examples

``` r
grayscale <- magick::image_read(system.file("extdata/images/cropped_Fus-GGC98-R.tif", package = "panelcomposite"))
multiply_by_colour(grayscale, "#FF0000")
#> # A tibble: 1 × 7
#>   format width height colorspace matte filesize density
#>   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
#> 1 png      280    210 sRGB       FALSE        0 +72x+72
multiply_by_colour(grayscale, brightness = 200)
#> # A tibble: 1 × 7
#>   format width height colorspace matte filesize density
#>   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
#> 1 png      280    210 sRGB       FALSE        0 +72x+72
multiply_by_colour(grayscale, colour_hex = "#00FFFF", brightness = 50)
#> # A tibble: 1 × 7
#>   format width height colorspace matte filesize density
#>   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
#> 1 png      280    210 sRGB       FALSE        0 +72x+72
```
