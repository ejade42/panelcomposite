# Package index

## Fluorescent microscopy panel compositing

### Main functions

User-facing functions to access the functionality of the
`panelcomposite` package

- [`assemble_panels()`](https://ejade42.github.io/panelcomposite/reference/assemble_panels.md)
  : Assemble panels from a list into a unified figure

### Helper functions

Internal functions used by the main user-facing functions

- [`calculate_E_offset()`](https://ejade42.github.io/panelcomposite/reference/calculate_E_offset.md)
  :

  Calculate pixel location of a capital `E`

- [`check_channel_dimensions()`](https://ejade42.github.io/panelcomposite/reference/check_channel_dimensions.md)
  : Check that all channels of the same panel have identical dimensions

- [`check_panel_dimensions()`](https://ejade42.github.io/panelcomposite/reference/check_panel_dimensions.md)
  : Check that panels in the same column have the same width and panels
  in the same row have the same height

- [`get_image_dimensions()`](https://ejade42.github.io/panelcomposite/reference/get_image_dimensions.md)
  : Determine the pixel dimensions of images in a list

- [`get_text_width()`](https://ejade42.github.io/panelcomposite/reference/get_text_width.md)
  : Calculate width of rendered text

- [`merge_channels()`](https://ejade42.github.io/panelcomposite/reference/merge_channels.md)
  : Merge a list of images together

- [`multiply_by_colour()`](https://ejade42.github.io/panelcomposite/reference/multiply_by_colour.md)
  : Multiply a grayscale image by a specified colour

- [`read_many_images()`](https://ejade42.github.io/panelcomposite/reference/read_many_images.md)
  : Read multiple images into a list
