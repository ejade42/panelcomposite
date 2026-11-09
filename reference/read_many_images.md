# Read multiple images into a list

Takes a vector of filenames (and optionally a folder to prefix to each
filename), reads all of those images via
[`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html),
and returns a list of the image objects.

## Usage

``` r
read_many_images(filenames_vector, folder = NULL)
```

## Arguments

- filenames_vector:

  `character_vector`. A vector of all the image filenames to be read.

- folder:

  `character`. The path to the folder the images are located in relative
  to the current working directory, or `NULL` (default) if the images
  are in the current working directory.

## Value

`list of image objects`. A list of all the specified images.
