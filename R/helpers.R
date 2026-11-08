## Helper functions
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

## Helper for merging channels
merge_channels <- function(image_list, operator = "Screen") {
    composite <- image_list[[1]]
    if (length(image_list) > 1) {
        for (i in 2:length(image_list)) {
            composite <- image_composite(composite, image_list[[i]], operator = operator)
        }
    }
    return(composite)
}


## Helper for reading in images from a vector of filenames
read_many_images <- function(filenames_vector, folder = NULL) {
    lapply(filenames_vector, function(f) {
        if (!is.null(folder) && !is.na(folder)) {
            f <- paste0(folder, "/", f)
        }
        image_read(f)
    })
}

## Helper of getting list of c(w, h) dimensions from a list of images
get_image_dimensions <- function(images_list) {
    lapply(images_list, function(i) {
        info <- image_info(i)
        c(info$width, info$height)
    })
}



## Channel dimensions - should have identical dimensions between channels of the same image
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



## Panel dimensions - should have the same width within each column, and the same height within each row
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

## Function for determining offsets based on a capital E
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

## Function for calculating rendered text width
get_text_width <- function(text, font_size, font_family, font_style, font_weight) {
    canvas_dim <- ceiling(font_size * nchar(text) * 1.5)
    img <- image_blank(canvas_dim, ceiling(font_size * 2), "white") |>
        image_annotate(text, size = font_size, font = font_family, style = font_style, weight = font_weight, location = "+0+0")
    mat <- as.raster(img)
    ink_pixels <- which(mat != "#ffffff" & mat != "#ffffffff", arr.ind = TRUE)
    return(max(ink_pixels[, 2]) - min(ink_pixels[, 2]) + 1)
}
