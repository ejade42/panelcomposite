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

## Function for determining offsets based on a capital E
calculate_E_offset <- function(font_size, font_family, font_style, font_weight) {
    mat <- image_blank(200, 200, "white") |>
        image_annotate("E", size = font_size, font = font_family, style = font_style, weight = font_weight, location = "+0+0") |>
        as.raster()
    
    ## Locate non-white pixel indices (the ink of 'E')
    ink_pixels <- which(mat != "#ffffff" & mat != "#ffffffff", arr.ind = TRUE)
    
    ## Compute 0-indexed pixel offsets relative to top-left (0,0)
    e_top    <- min(ink_pixels[, 1]) - 1
    e_bottom <- max(ink_pixels[, 1])
    e_left   <- min(ink_pixels[, 2]) #- 1    ## The -1 is technically correct, but the letters render with a very dim left edge so bumping that by 1 to the first bright pixel
    e_right  <- max(ink_pixels[, 2])
    
    return(list(
        left   = e_left,
        top    = e_top,
        bottom = e_bottom,
        right  = e_right
    ))
}



## Function for assembling panels from a panels_list
assemble_panels <- function(
        panels_list,
        nrow,
        ncol,
        row_titles = NULL,
        row_titles_position = "west",
        col_titles = NULL,
        col_titles_position = "north",
        titles_colour = "#000000",
        titles_font = "",
        titles_size = 75,
        titles_style = "normal",
        titles_weight = 400,
        panel_border = 20, 
        figure_border_top = 100,
        figure_border_left = 100,
        figure_border_bottom = 20,
        figure_border_right = 20,
        background_colour = "#000000", 
        border_colour = "#FFFFFF",
        labels_top = NULL,
        labels_bottom = NULL,
        labels_colours_top = "#FFFFFF",
        labels_colours_bottom = "#FFFFFF",
        labels_font_top = "",
        labels_font_bottom = "",
        labels_size_top = 50,
        labels_size_bottom = 50,
        labels_position_top = 20,
        labels_position_bottom = 20,
        labels_style_top = "normal",
        labels_style_bottom = "normal",
        labels_weight_top = 400,
        labels_weight_bottom = 400
) {
    panels_count <- length(panels_list)
    
    ## Extract dimensions and check they are consistent within individual cols and rows
    panel_dimensions_list <- get_image_dimensions(panels_list)
    panel_dimensions <- check_panel_dimensions(panel_dimensions_list, panels_count, nrow, ncol)
    
    ## Set up full canvas for this set
    full_canvas <- image_blank(
        sum(panel_dimensions$col_widths)  + (ncol-1) * panel_border + figure_border_left + figure_border_right, 
        sum(panel_dimensions$row_heights) + (nrow-1) * panel_border + figure_border_top + figure_border_bottom, 
        color = border_colour
    )
    
    ## Get offsets
    labels_top_E <- calculate_E_offset(labels_size_top, labels_font_top, labels_style_top, labels_weight_top)
    labels_bottom_E <- calculate_E_offset(labels_size_bottom, labels_font_bottom, labels_style_bottom, labels_weight_bottom)
    titles_E <- calculate_E_offset(titles_size, titles_font, titles_style, titles_weight)
    
    ## Add panels in at the right locations
    current_width_px <- figure_border_left
    current_height_px <- figure_border_top
    current_panel <- 1
    composited <- full_canvas
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            this_panel <- panels_list[[current_panel]]
            
            ## Add labels to panel
            if (!any(is.null(labels_top)) && !any(is.na(labels_top))) {
                this_panel <- image_annotate(
                    this_panel, 
                    text = labels_top[current_panel], 
                    color = labels_colours_top[current_panel], 
                    location = paste0("+", labels_position_top - labels_top_E$left, "+", labels_position_top - labels_top_E$top), 
                    gravity = "northwest",
                    font = labels_font_top,
                    style = labels_style_top,
                    weight = labels_weight_top,
                    size = labels_size_top
                )
            }
            if (!any(is.null(labels_bottom)) && !any(is.na(labels_bottom))) {
                this_panel <- image_annotate(
                    this_panel, 
                    text = labels_bottom[current_panel], 
                    color = labels_colours_bottom[current_panel], 
                    location = paste0("+", labels_position_bottom - labels_bottom_E$left, "+", panel_dimensions$row_heights[i] - labels_position_bottom - labels_bottom_E$bottom), 
                    gravity = "northwest",
                    font = labels_font_bottom,
                    style = labels_style_bottom,
                    weight = labels_weight_bottom,
                    size = labels_size_bottom
                )
            }
            if (i == 1 && !any(is.null(col_titles)) && !any(is.na(col_titles))) {
                print("adding title")
                this_panel <- image_annotate(
                    this_panel,
                    text = col_titles[j],
                    color = titles_colour,
                    style = titles_style,
                    font = titles_font,
                    weight = titles_weight,
                    size = titles_size,
                    location = paste0("+0+", panel_dimensions$row_heights[i]-100),  ## bear in mind i==1
                    gravity = col_titles_position,
                )
            }
            
            ## Add panel to image
            composited <- image_composite(
                composited,
                this_panel,
                offset = paste0("+", current_width_px, "+", current_height_px),
                gravity = "northwest"
            )
            
            
            
            ## Iterate column
            current_panel <- current_panel + 1
            current_width_px <- current_width_px + panel_dimensions$col_widths[j] + panel_border
        }
        ## Iterate row, reset column to leftmost
        current_width_px <- figure_border_left
        current_height_px <- current_height_px + panel_dimensions$row_heights[i] + panel_border
    }
    
    composited
}




## To make a figure with identical spacing between all panels and no subgroups etc
create_simple_figure <- function(
        filenames_vector, 
        channels_vector, 
        colours_vector, 
        brightness_vector, 
        nrow, 
        ncol, 
        channels_list, 
        panel_border = 20, 
        figure_border_text = 100,
        figure_border_plain = 20,
        folder = NULL, 
        background_colour = "#000000", 
        border_colour = "#FFFFFF",
        row_titles = NULL,
        row_titles_position = "left",
        col_titles = NULL,
        col_titles_position = "top",
        titles_colour = "#000000",
        titles_font = "",
        titles_size = 75,
        titles_style = "normal",
        titles_weight = 400,
        labels_top = NULL,
        labels_bottom = NULL,
        labels_colours_top = "#FFFFFF",
        labels_colours_bottom = "#FFFFFF",
        labels_font = "",
        labels_size = 50,
        labels_position = 20,
        labels_style = "normal",
        labels_weight = 400
) {
    ## Process labels
    if (!any(is.null(labels_top)) && !any(is.na(labels_top))) {
        if (length(labels_colours_top) == 1) {labels_colours_top <- rep(labels_colours_top, length(labels_top))}
        if (length(labels_top) != length(labels_colours_top)) {abort(paste0("Length of labels colour vector must be 1 or the same length as labels vector. Current lengths: ", paste(length(labels_top), length(labels_colours_top), sep = ", ")))}
        if (length(labels_top) != length(channels_list)) {abort(paste0("Labels vector must be NULL or the same lengths as channels list. Current lengths: ", paste(length(labels_top), length(channels_list), sep = ", ")))}
        
    }
    if (!any(is.null(labels_bottom)) && !any(is.na(labels_bottom))) {
        if (length(labels_colours_bottom) == 1) {labels_colours_bottom <- rep(labels_colours_bottom, length(labels_bottom))}
        if (length(labels_bottom) != length(labels_colours_bottom)) {abort(paste0("Length of labels colour vector must be 1 or the same length as labels vector. Current lengths: ", paste(length(labels_bottom), length(labels_colours_bottom), sep = ", ")))}
        if (length(labels_bottom) != length(channels_list)) {abort(paste0("Labels vector must be NULL or the same lengths as channels list. Current lengths: ", paste(length(labels_bottom), length(channels_list), sep = ", ")))}
    }
    ## From presence and position of row/col titles, determine effective panel borders
    if (!any(is.null(row_titles)) && !any(is.na(row_titles))) {
        if (tolower(row_titles_position) %in% c("left", "l", "west", "w")) {
            figure_border_left <- figure_border_text
            figure_border_right <- figure_border_plain
            row_titles_position <- "east"
        } else if (tolower(row_titles_position) %in% c("right", "r", "east", "e")) {
            figure_border_left <- figure_border_plain
            figure_border_right <- figure_border_text
            row_titles_position <- "west"
        } else {
            abort(paste("Left/right row titles position not recognised:", row_titles_position))
        }
    } else {
        print("no row titles")
        figure_border_left <- figure_border_plain
        figure_border_right <- figure_border_plain
    }
    if (!any(is.null(col_titles)) && !any(is.na(col_titles))) {
        if (tolower(col_titles_position) %in% c("top", "t", "north", "n")) {
            figure_border_top <- figure_border_text
            figure_border_bottom <- figure_border_plain
            col_titles_position <- "south"
        } else if (tolower(col_titles_position) %in% c("bottom", "b", "south", "s")) {
            figure_border_top <- figure_border_plain
            figure_border_bottom <- figure_border_text
            col_titles_position <- "north"
        } else {
            abort(paste("Top/bottom col titles position not recognised:", col_titles_position))
        }
    } else {
        print("no col titles")
        figure_border_top <- figure_border_plain
        figure_border_bottom <- figure_border_plain
    }
    print(c(figure_border_left, figure_border_right, figure_border_top, figure_border_bottom))
    
    
    ## Read in images
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
    
    assemble_panels(
        panels_list,
        nrow = nrow,
        ncol = ncol,
        row_titles = row_titles,
        row_titles_position = row_titles_position,
        col_titles = col_titles,
        col_titles_position = col_titles_position,
        titles_colour = titles_colour,
        titles_font = titles_font,
        titles_size = titles_size,
        titles_style = titles_style,
        titles_weight = titles_weight,
        panel_border = panel_border,
        figure_border_top = figure_border_top,
        figure_border_bottom = figure_border_bottom,
        figure_border_left = figure_border_left,
        figure_border_right = figure_border_right,
        background_colour = background_colour,
        border_colour = border_colour,
        labels_top = labels_top,
        labels_bottom = labels_bottom,
        labels_colours_top = labels_colours_top,
        labels_colours_bottom = labels_colours_bottom,
        labels_font_top = labels_font,
        labels_font_bottom = labels_font,
        labels_size_top = labels_size,
        labels_size_bottom = labels_size,
        labels_position_top = labels_position,
        labels_position_bottom = labels_position,
        labels_style_top = labels_style,
        labels_style_bottom = labels_style,
        labels_weight_top = labels_weight,
        labels_weight_bottom = labels_weight
    )
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


create_simple_figure(
    c("EXO1_Day14_main_hoechst.tiff", "EXO1_Day14_main_map2.tiff", "EXO1_Day14_main_target.tiff"),
    c("H", "M", "T"),
    c("#0000FF", "#FF0000", "#00FF00"),
    c(100, 100, 100),
    nrow = 2, 
    ncol = 2,
    col_titles = c("left", "right"),
    row_titles = c("top", "bottom"),
    titles_size = 25,
    titles_colour = "#FFFF00",
    channels_list = list(
        c("H", "T", "M"),
        "H",
        "M",
        "T"
    ),
    panel_border = 20,
    folder = "imagej_export_max",
    background_colour = "#000000",
    border_colour = "#FFFFFF",
    labels_top = c("A", "B", "C", "E"),
    labels_bottom = c("Merge", "Hoechst", "MAP2", "EXO1"),
    labels_colours_bottom = c("#FFFFFF", "#0000FF", "#FF0000", "#00FF00"),
    labels_position = 30
)
