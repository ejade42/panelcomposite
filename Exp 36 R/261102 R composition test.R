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

merge_channels <- function(image_list, operator = "Screen") {
    composite <- image_list[[1]]
    if (length(image_list) > 1) {
        for (i in 2:length(image_list)) {
            composite <- image_composite(composite, image_list[[i]], operator = operator)
        }
    }
    return(composite)
}

image1 <- image_read(paste0(folder, "/EXO1_Day00_main_hoechst.tiff"))
image1_col <- multiply_by_colour(image1, "cyan", 200)
image1_col


h <- image_read()



read_many_images <- function(filenames_vector, folder = NULL) {
    lapply(filenames_vector, function(f) {
        if (!is.null(folder) && !is.na(folder)) {
            f <- paste0(folder, "/", f)
        }
        image_read(f)
    })
}

get_image_dimensions <- function(images_list) {
    lapply(images_list, function(i) {
        info <- image_info(i)
        c(info$width, info$height)
    })
}

check_image_dimensions <- function(dimensions_list, images_count, nrow, ncol) {
    col_widths <- sapply(1:ncol, function(i) {
        in_col_TF <- 1:images_count %% ncol == i %% ncol
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
        in_row_TF <- 1:images_count %% nrow == i %% nrow
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
assemble_panel <- function(filenames_vector, channels_vector, nrow, ncol, channels_list, separation = "10", folder = NULL, background_colour = "black", border_colour = "white") {
    images_count <- length(filenames_vector)
    images_list <- read_many_images(filenames_vector, folder)
    names(images_list) <- channels_vector
    
    
    
    
    ## This all needs to happen later once the images have been merged into panels - the ordered list comes from channels_list,
    ## whereas input images can be supplied in any order and any number and are then pulled by channel name into each panel
    panels_count <- length(channels_list)
    
    ## Extract dimensions and check they are consistent within individual cols and rows
    image_dimensions_list <- get_image_dimensions(images_list)
    panel_dimensions <- check_image_dimensions(image_dimensions_list, panels_count, nrow, ncol)
    
    ## Set up full canvas for this set
    full_canvas <- image_blank(
        sum(panel_dimensions$col_widths)  + (ncol-1) * separation, 
        sum(panel_dimensions$row_heights) + (nrow-1) * separation, 
        color = border_colour
    )
    
    ## Iterate over panels
    
}




## Testing
l <- read_many_images(c("EXO1_Day00_main_hoechst.tiff", "EXO1_Day00_main_map2.tiff", "EXO1_Day00_main_target.tiff", "EXO1_HEK293T_main_map2.tiff"), "imagej_export_max")
d <- get_image_dimensions(l)

channels_list <- list(
    c("H", "T", "M"),
    "H",
    "M",
    "T"
)
