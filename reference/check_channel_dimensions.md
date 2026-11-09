# Check that all channels of the same panel have identical dimensions

Takes a list of images dimensions that should all be identical. This is
expected to be produced from a list of images via
[`get_image_dimensions()`](get_image_dimensions.md). In context, this is
expected to be the various channels of the same ICC/IHC image to be
overlaid into a panel, but any set of identical-dimensioned images
should work.

Throws an error if not all widths are equal or if not all heights are
equal, otherwise returns a named numeric vector of the shared pixel
width and height of all the images.

## Usage

``` r
check_channel_dimensions(dimensions_list, id = "")
```

## Arguments

- dimensions_list:

  `list of numeric vectors`. A list of length-2 numeric vectors storing
  the width and height of each image expected to have identical
  dimensions, as produced by
  [`get_image_dimensions()`](get_image_dimensions.md).

- id:

  `character`. An optional argument to be printed at the beginning of
  error messages if widths or heights are not equal. Default: `""`.

## Value

`numeric vector`. A numeric vector of width and height, with names
`width` and `height`.
