#' Multiply a grayscale image by a specified colour
#'
#' @description
#' Takes a grayscale image and multiplies it by a specified colour,
#' with the option to modulate the brightness.
#'
#' @param img_gray `image object`. A grayscale image, e.g. produced by [`magick::image_read()`], to be tinted the given colour.
#' @param colour_hex `character`. A hex code e.g. `#FF0000` to recolour the grayscale image. Default `#FFFFFF` (white) will not change the image. Colour keywords e.g. `"red"` can work, but `magick` uses a different library of keywords (W3C) compared to RStudio preview colours (X11), so preview colours may not match the actual output colour. In particular, `"green"` maps to `"#00FF00"` in RStudio but `"#008000"` in `magick`.
#' @param brightness `numeric`. Brightness modulation to be applied to the image. `100` (default) does not alter brightness; higher values increase brightness; lower values decrease brightness.
#'
#' @return `image object`. The image with the specified colour tinting and brightness modulation applied.
#' @export
#'
#' @examples
#' grayscale <- image_read(system.file("extdata/images/cropped_Fus-GGC98-R.tif", package = "panelcomposite"))
#' multiply_by_colour(grayscale, "#FF0000")
#' multiply_by_colour(grayscale, brightness = 200)
#' multiply_by_colour(grayscale, colour_hex = "#00FFFF", brightness = 50)
multiply_by_colour <- function(img_gray, colour_hex = "#FFFFFF", brightness = 100) {
    info <- image_info(img_gray)

    ## Modulate brightness
    if (brightness != 100) {
        img_gray <- image_modulate(img_gray, brightness = brightness)
    }

    # Create a solid color canvas matching image dimensions
    canvas <- image_blank(info$width, info$height, color = colour_hex)

    # Multiply grayscale intensity by the solid color
    image_composite(canvas, img_gray, operator = "Multiply")
}




#' Merge a list of images together
#'
#' @description
#' Takes a list of image objects and iterates along the list,
#' merging all of the images into one via the specified operator.
#'
#' @param image_list `list of image objects`. A list of images to be merged. Intended to be different channels of the same fluorescently labelled image. Assumed to be the same size - behaviour may be unpredictable if not.
#' @param operator `character`. The operator type to be used in [`magick::image_composite()`]. Default: `"Screen"`.
#'
#' @return `image object`. A single image with all of the input images merged together
#' @export
merge_channels <- function(image_list, operator = "Screen") {
    composite <- image_list[[1]]
    if (length(image_list) > 1) {
        for (i in 2:length(image_list)) {
            composite <- image_composite(composite, image_list[[i]], operator = operator)
        }
    }
    return(composite)
}




#' Read multiple images into a list
#'
#' @description
#' Takes a vector of filenames (and optionally a folder to
#' prefix to each filename), reads all of those images via
#' [`magick::image_read()`], and returns a list of the image objects.
#'
#' @param filenames_vector `character_vector`. A vector of all the image filenames to be read.
#' @param folder `character`. The path to the folder the images are located in relative to the current working directory, or `NULL` (default) if the images are in the current working directory.
#'
#' @return `list of image objects`. A list of all the specified images.
#' @export
read_many_images <- function(filenames_vector, folder = NULL) {
    lapply(filenames_vector, function(f) {
        if (!is.null(folder) && !is.na(folder)) {
            f <- paste0(folder, "/", f)
        }
        image_read(f)
    })
}

#' Determine the pixel dimensions of images in a list
#'
#' @description
#' Takes a list of image objects and calculates their pixel
#' widths and heights via [`magick::image_info()`].
#' Returns a list of `c(width, height)` numeric vectors.
#'
#' @param images_list `list of image objects`. A list of the images whose dimensions should be calculated, as produced by [`read_many_images()`].
#'
#' @return `list of numeric vectors`. A list of length-2 numeric vectors storing the width and height of each image in the input list.
#' @export
get_image_dimensions <- function(images_list) {
    lapply(images_list, function(i) {
        info <- image_info(i)
        c(info$width, info$height)
    })
}




#' Check that all channels of the same panel have identical dimensions
#'
#' @description
#' Takes a list of images dimensions that should all be identical.
#' This is expected to be produced from a list of images via [`get_image_dimensions()`].
#' In context, this is expected to be the various channels of the same ICC/IHC image
#' to be overlaid into a panel, but any set of identical-dimensioned images should work.
#'
#' Throws an error if not all widths are equal or if not all heights are equal, otherwise
#' returns a named numeric vector of the shared pixel width and height of all the images.
#'
#' @param dimensions_list `list of numeric vectors`. A list of length-2 numeric vectors storing the width and height of each image expected to have identical dimensions, as produced by [`get_image_dimensions()`].
#' @param id `character`. An optional argument to be printed at the beginning of error messages if widths or heights are not equal. Default: `""`.
#'
#' @return `numeric vector`. A numeric vector of width and height, with names `width` and `height`.
#' @export
check_channel_dimensions <- function(dimensions_list, id = "") {
    channels <- names(dimensions_list)

    widths <- sapply(dimensions_list, `[[`, 1)
    panel_width <- unique(widths)
    if (length(panel_width) != 1) {
        abort(
            paste0(id, "Channels have different widths:", paste(channels, widths, sep = ": ", collapse = ", ")),
            call = rlang::caller_env()
        )
    }

    heights <- sapply(dimensions_list, `[[`, 2)
    panel_height <- unique(heights)
    if (length(panel_height) != 1) {
        abort(
            paste0(id, "Channels have different heights:", paste(channels, heights, sep = ": ", collapse = ", ")),
            call = rlang::caller_env()
        )
    }

    return(c(width = panel_width, height = panel_height))
}




#' Check that panels in the same column have the same width and panels in the same row have the same height
#'
#' @description
#' Takes a list of images that represents panels in a grid-based figure,
#' arranged left to right and then top to bottom, e.g. \preformatted{
#' 1 2 3 4
#' 5 6 7 8
#' }
#'
#' Checks that all panels in the same column have identical pixel widths e.g.
#' `1` and `5`, `2` and `6`, `3` and `7`, `4` and `8` each have identical widths.
#' Checks that all panels in the same row have identical pixel heights e.g.
#' `1` `2` `3` and `4`, `5` `6` `7` and `8` each have identical heights.
#'
#' Throws an error if there are non-identical widths within a column or
#' non-identical heights within a row.
#' Otherwise, returns a named list of numeric vectors, with `$col_widths` storing
#' the pixel widths of columns 1 through `ncol` and `$row_heights` storing the
#' pixel heights of rows 1 through `nrow`.
#'
#' @param dimensions_list `list of numeric vectors`. A list of length-2 numeric vectors storing the width and height of each panel in the figure, as produced by [`get_image_dimensions()`].
#' @param panels_count `integer`. The total number of panels in the figure. Should be equal to `nrow * ncol`.
#' @param nrow `integer`. The number of rows in the figure.
#' @param ncol `integer`. The number of columns in the figure.
#'
#' @return `list of numeric vectors`. A list of 2 numeric vectors: `$col_widths` storing the pixel widths of columns 1 through `ncol`, and `$row_heights` storing the pixel heights of rows 1 through `nrow`.
#' @export
check_panel_dimensions <- function(dimensions_list, panels_count, nrow, ncol) {
    col_widths <- sapply(1:ncol, function(i) {
        in_col_TF <- 1:panels_count %% ncol == i %% ncol
        in_col_dims_list <- dimensions_list[in_col_TF]
        in_col_widths <- sapply(in_col_dims_list, `[[`, 1)

        this_col_width <- unique(in_col_widths)
        if (length(this_col_width) == 1) {
            this_col_width
        } else {
            abort(
                paste0("In col ", i, ", there are multiple different widths: ", paste(in_col_widths, collapse = ", ")),
                call = rlang::caller_env()
            )
        }
    })

    row_heights <- sapply(1:nrow, function(i) {
        in_row_TF <- 1:panels_count %% nrow == i %% nrow
        in_row_dims_list <- dimensions_list[in_row_TF]
        in_row_heights <- sapply(in_row_dims_list, `[[`, 2)

        this_row_height <- unique(in_row_heights)
        if (length(this_row_height) == 1) {
            this_row_height
        } else {
            abort(
                paste0("In row ", i, ", there are multiple different heights: ", paste(in_row_heights, collapse = ", ")),
                call = rlang::caller_env()
            )
        }
    })

    list(col_widths = col_widths, row_heights = row_heights)
}




#' Calculate pixel location of a capital `E`
#'
#' @description
#' Draws a capital `E` via [`magick::image_annotate()`] with the specified
#' font options, then calculates how far from the top left corner of the text
#' box the actual `E` pixels are rendered.
#'
#' This is used for positioning figure labels precisely based on the actual text
#' spot (which `E` is used to stand in for) *without* considering hanging letters
#' like `g`.
#'
#' @param font_size `numeric`. The `size` argument passed to [`magick::image_annotate()`].
#' @param font_family `character`. The `font` argument passed to [`magick::image_annotate()`].
#' @param font_style `character`. The `style` argument passed to [`magick::image_annotate()`].
#' @param font_weight `numeric`. The `weight` argument passed to [`magick::image_annotate()`].
#' @param font_angle `numeric`. The `degrees` argument passed to [`magick::image_annotate()`]. Default: `0`.
#'
#' @return `list of numeric values`. The distance in pixels from the text box to the `left`, `top`, `bottom`, and `right` of the `E`, as well as the `height` and `width` of the `E`.
#' @export
calculate_E_offset <- function(font_size, font_family, font_style, font_weight, font_angle = 0) {
    canvas_dim <- ceiling(font_size * 4)
    draw_offset <- round(ceiling(font_size * 1.5))

    mat <- image_blank(canvas_dim, canvas_dim, "white") |>
        image_annotate("E", size = font_size, font = font_family, style = font_style, weight = font_weight, degrees = font_angle, location = paste0("+", draw_offset, "+", draw_offset)) |>
        image_threshold(type = "white", threshold = "50%") |>
        as.raster()

    ## Locate non-white pixel indices (the ink of 'E')
    ink_pixels <- which(mat != "#ffffff" & mat != "#ffffffff", arr.ind = TRUE)

    ## Compute 0-indexed pixel offsets relative to top-left (0,0)
    e_top    <- min(ink_pixels[, 1]) - draw_offset - 1
    e_bottom <- max(ink_pixels[, 1]) - draw_offset
    e_left   <- min(ink_pixels[, 2]) - draw_offset - 1
    e_right  <- max(ink_pixels[, 2]) - draw_offset

    return(list(
        left   = e_left,
        top    = e_top,
        bottom = e_bottom,
        right  = e_right,
        height = e_bottom - e_top,
        width  = e_right - e_left
    ))
}





#' Calculate width of rendered text
#'
#' @description
#' Takes a string of text to render via [`magick::image_annotate()`] with the
#' specified options, and calculates the pixel width of the text.
#'
#' @param text `character`. The text to be rendered, i.e. the `text` argument passed to `magick::image_annotate()`.
#' @param font_size `numeric`. The `size` argument passed to [`magick::image_annotate()`].
#' @param font_family `character`. The `font` argument passed to [`magick::image_annotate()`].
#' @param font_style `character`. The `style` argument passed to [`magick::image_annotate()`].
#' @param font_weight `numeric`. The `weight` argument passed to [`magick::image_annotate()`].
#'
#' @return `integer`. The pixel width of the specified text.
#' @export
get_text_width <- function(text, font_size, font_family, font_style, font_weight) {
    canvas_dim <- ceiling(font_size * nchar(text) * 1.5)
    img <- image_blank(canvas_dim, ceiling(font_size * 2), "white") |>
        image_annotate(text, size = font_size, font = font_family, style = font_style, weight = font_weight, location = "+0+0")
    mat <- as.raster(img)
    ink_pixels <- which(mat != "#ffffff" & mat != "#ffffffff", arr.ind = TRUE)
    return(max(ink_pixels[, 2]) - min(ink_pixels[, 2]) + 1)
}
