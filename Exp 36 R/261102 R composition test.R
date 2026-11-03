library(magick)
library(rlang)
dir <- "/Volumes/ScotterLab/Evelyn Jade/Experiments/260420 - Exp 36 - Combined MMR ICC"
folder <- "imagej_export_max"
setwd(dir)

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

## Small function for making a single set of 4
## Provide: vector of filenames, vector of corresponding channel names, dimensions of layout, list of which channels should be present in each panel (as the book reads), separation between channels in px
assemble_panels <- function(filenames_vector, channels_vector, colours_vector, brightness_vector, nrow, ncol, channels_list, separation = "10", folder = NULL, background_colour = "black", border_colour = "white") {
    images_count <- length(filenames_vector)
    images_list <- read_many_images(filenames_vector, folder)
    names(images_list) <- channels_vector
    image_dimensions_list <- get_image_dimensions(images_list)
    
    names(colours_vector) <- channels_vector
    names(brightness_vector) <- channels_vector
    
    
    ## Assemble and merge panels
    panels_list <- lapply(seq_along(channels_list), function(i) {
        selected_channels <- channels_list[[i]]
        selected_images_list <- images_list[selected_channels]
        selected_images_dimensions <- get_image_dimensions(selected_images_list)
        panel_dimensions <- check_channel_dimensions(selected_images_dimensions, id = paste0("[Panel ", i, "] "))
        
        ## Can now assume that all images have equal dimensions, stored as [1] and [2] in panel_dimensions
        panel_canvas <- image_blank(panel_dimensions[1], panel_dimensions[2], color = background_colour)
        
        coloured_images_list <- lapply(selected_channels, function(channel) {
            multiply_by_colour(selected_images_list[channel][[1]], colours_vector[channel][[1]], brightness_vector[channel][[1]])
        })
        merged_image <- merge_channels(
            list(
                panel_canvas,
                merge_channels(coloured_images_list)
            )
        )
    })
    
    panels_count <- length(panels_list)
    
    ## Extract dimensions and check they are consistent within individual cols and rows
    panel_dimensions_list <- get_image_dimensions(panels_list)
    panel_dimensions <- check_panel_dimensions(panel_dimensions_list, panels_count, nrow, ncol)
    
    ## Set up full canvas for this set
    full_canvas <- image_blank(
        sum(panel_dimensions$col_widths)  + (ncol-1) * separation, 
        sum(panel_dimensions$row_heights) + (nrow-1) * separation, 
        color = border_colour
    )
    
    ## Add panels in at the right locations
    current_width_px <- 0
    current_height_px <- 0
    current_panel <- 1
    composited <- full_canvas
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            composited <- image_composite(
                composited,
                panels_list[[current_panel]],
                offset = paste0("+", current_width_px, "+", current_height_px),
                gravity = "northwest"
            )
            
            current_panel <- current_panel + 1
            current_width_px <- current_width_px + panel_dimensions$col_widths[j] + separation
        }
        current_width_px <- 0
        current_height_px <- current_height_px + panel_dimensions$row_heights[i] + separation
    }
    
    composited
}




## Testing
l <- read_many_images(c("EXO1_Day14_main_hoechst.tiff", "EXO1_Day00_main_map2.tiff", "EXO1_Day00_main_target.tiff"), "imagej_export_max")
cv <- c("H", "M", "T")
names(l) <- cv
d <- get_image_dimensions(l)

channels_list <- list(
    c("H", "T", "M"),
    "H",
    "M",
    "T"
)


assemble_panels(
    c("EXO1_Day14_main_hoechst.tiff", "EXO1_Day14_main_map2.tiff", "EXO1_Day14_main_target.tiff"),
    c("H", "M", "T"),
    c("blue", "red", "green"),
    c(100, 100, 1000),
    nrow = 2, 
    ncol = 2,
    channels_list = list(
        c("H", "T", "M"),
        "H",
        "M",
        "T"
    ),
    separation = 10,
    folder = "imagej_export_max",
    background_colour = "black",
    border_colour = "white"
)
