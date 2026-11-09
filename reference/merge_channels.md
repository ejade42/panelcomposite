# Merge a list of images together

Takes a list of image objects and iterates along the list, merging all
of the images into one via the specified operator.

## Usage

``` r
merge_channels(image_list, operator = "Screen")
```

## Arguments

- image_list:

  `list of image objects`. A list of images to be merged. Intended to be
  different channels of the same fluorescently labelled image. Assumed
  to be the same size - behaviour may be unpredictable if not.

- operator:

  `character`. The operator type to be used in
  [`magick::image_composite()`](https://docs.ropensci.org/magick/reference/composite.html).
  Default: `"Screen"`.

## Value

`image object`. A single image with all of the input images merged
together
