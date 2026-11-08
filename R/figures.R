library(magick)
library(rlang)
dir <- "/Volumes/ScotterLab/Evelyn Jade/Experiments/260420 - Exp 36 - Combined MMR ICC"
folder <- "imagej_export_max"
setwd(dir)




## Function for assembling panels from a panels_list
assemble_panels <- function(
    panels_list,
    nrow,
    ncol,
    row_titles = NULL,
    row_titles_side = "left",
    row_titles_bar_size = 0,
    row_titles_bar_colour = "#000000",
    col_titles = NULL,
    col_titles_side = "top",
    col_titles_bar_size = 0,
    col_titles_bar_colour = "#000000",
    titles_colour = "#000000",
    titles_font = "",
    titles_size = 75,
    titles_style = "normal",
    titles_weight = 400,
    titles_position = 20,
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
    titles_E_90  <- calculate_E_offset(titles_size, titles_font, titles_style, titles_weight, 90)
    titles_E_270 <- calculate_E_offset(titles_size, titles_font, titles_style, titles_weight, 270)


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


            ## Add panel to image
            composited <- image_composite(
                composited,
                this_panel,
                offset = paste0("+", current_width_px, "+", current_height_px),
                gravity = "northwest"
            )


            ## Add titles to image if it is the right time - col titles when iterating over cols
            if (i == 1 && col_titles_side == "top") {
                if (col_titles_bar_size > 0) {
                    bar <- image_blank(height = col_titles_bar_size, width = panel_dimensions$row_heights[i], color = col_titles_bar_colour)
                    composited <- image_composite(
                        composited,
                        bar,
                        offset = paste0("+", current_width_px, "+", current_height_px - 2*col_titles_bar_size),
                        gravity = "northwest"
                    )
                }

                if (!any(is.null(col_titles)) && !any(is.na(col_titles))) {
                    this_title <- col_titles[j]
                    this_title_width <- get_text_width(this_title, titles_size, titles_font, titles_style, titles_weight)
                    this_title_x <- current_width_px + (panel_dimensions$col_widths[j] - this_title_width) / 2
                    this_title_y <- current_height_px - titles_position - titles_E$bottom - 2*col_titles_bar_size

                    composited <- image_annotate(
                        composited,
                        text = this_title,
                        color = titles_colour,
                        location = paste0("+", round(this_title_x), "+", round(this_title_y)),
                        gravity = "northwest",
                        font = titles_font,
                        style = titles_style,
                        weight = titles_weight,
                        size = titles_size
                    )
                }
            } else if (i == nrow && col_titles_side == "bottom") {
                if (col_titles_bar_size > 0) {
                    bar <- image_blank(height = col_titles_bar_size, width = panel_dimensions$row_heights[i], color = col_titles_bar_colour)
                    composited <- image_composite(
                        composited,
                        bar,
                        offset = paste0("+", current_width_px, "+", current_height_px + panel_dimensions$row_heights[i] + col_titles_bar_size),
                        gravity = "northwest"
                    )
                }

                if (!any(is.null(col_titles)) && !any(is.na(col_titles))) {
                    this_title <- col_titles[j]
                    this_title_width <- get_text_width(this_title, titles_size, titles_font, titles_style, titles_weight)
                    this_title_x <- current_width_px + (panel_dimensions$col_widths[j] - this_title_width) / 2
                    this_title_y <- current_height_px + panel_dimensions$row_heights[i] + titles_position - titles_E$top + col_titles_bar_size

                    composited <- image_annotate(
                        composited,
                        text = this_title,
                        color = titles_colour,
                        location = paste0("+", round(this_title_x), "+", round(this_title_y)),
                        gravity = "northwest",
                        font = titles_font,
                        style = titles_style,
                        weight = titles_weight,
                        size = titles_size
                    )
                }
            }


            ## Add titles to image if it is the right time - row titles when iterating over row
            if (j == 1 && row_titles_side == "left") {
                if (row_titles_bar_size > 0) {
                    bar <- image_blank(width = row_titles_bar_size, height = panel_dimensions$row_heights[i], color = row_titles_bar_colour)
                    composited <- image_composite(
                        composited,
                        bar,
                        offset = paste0("+", current_width_px - 2*row_titles_bar_size, "+", current_height_px),
                        gravity = "northwest"
                    )
                }

                if (!any(is.null(row_titles)) && !any(is.na(row_titles))) {
                    this_title <- row_titles[i]
                    this_title_width <- get_text_width(this_title, titles_size, titles_font, titles_style, titles_weight)
                    this_title_x <- current_width_px - titles_position - titles_E_270$width - 2*row_titles_bar_size
                    this_title_y <- current_height_px + (panel_dimensions$row_heights[i] + this_title_width) / 2

                    composited <- image_annotate(
                        composited,
                        degrees = 270,
                        text = this_title,
                        color = titles_colour,
                        location = paste0("+", round(this_title_x), "+", round(this_title_y)),
                        gravity = "northwest",
                        font = titles_font,
                        style = titles_style,
                        weight = titles_weight,
                        size = titles_size
                    )
                }
            } else if (j == ncol && row_titles_side == "right") {
                if (row_titles_bar_size > 0) {
                    bar <- image_blank(width = row_titles_bar_size, height = panel_dimensions$row_heights[i], color = row_titles_bar_colour)
                    composited <- image_composite(
                        composited,
                        bar,
                        offset = paste0("+", current_width_px + panel_dimensions$col_widths[j] + row_titles_bar_size, "+", current_height_px),
                        gravity = "northwest"
                    )
                }

                if (!any(is.null(row_titles)) && !any(is.na(row_titles))) {
                    this_title <- row_titles[i]
                    this_title_width <- get_text_width(this_title, titles_size, titles_font, titles_style, titles_weight)
                    this_title_x <- current_width_px + titles_position - titles_E_90$left + panel_dimensions$col_widths[j] + 2*row_titles_bar_size
                    this_title_y <- current_height_px + (panel_dimensions$row_heights[i] - this_title_width) / 2

                    composited <- image_annotate(
                        composited,
                        degrees = 90,
                        text = this_title,
                        color = titles_colour,
                        location = paste0("+", round(this_title_x), "+", round(this_title_y)),
                        gravity = "northwest",
                        font = titles_font,
                        style = titles_style,
                        weight = titles_weight,
                        size = titles_size
                    )
                }
            }






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
    row_titles_side = "left",
    col_titles = NULL,
    col_titles_side = "top",
    titles_bar_size = 0,
    titles_bar_colour = "#000000",
    titles_colour = "#000000",
    titles_font = "",
    titles_size = 75,
    titles_style = "normal",
    titles_weight = 400,
    titles_position = 20,
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
        if (tolower(row_titles_side) %in% c("left", "l", "west", "w")) {
            figure_border_left <- figure_border_text
            figure_border_right <- figure_border_plain
            row_titles_side <- "left"
        } else if (tolower(row_titles_side) %in% c("right", "r", "east", "e")) {
            figure_border_left <- figure_border_plain
            figure_border_right <- figure_border_text
            row_titles_side <- "right"
        } else {
            abort(paste("Left/right row titles position not recognised:", row_titles_side))
        }
    } else {
        print("no row titles")
        figure_border_left <- figure_border_plain
        figure_border_right <- figure_border_plain
    }
    if (!any(is.null(col_titles)) && !any(is.na(col_titles))) {
        if (tolower(col_titles_side) %in% c("top", "t", "north", "n")) {
            figure_border_top <- figure_border_text
            figure_border_bottom <- figure_border_plain
            col_titles_side <- "top"
        } else if (tolower(col_titles_side) %in% c("bottom", "b", "south", "s")) {
            figure_border_top <- figure_border_plain
            figure_border_bottom <- figure_border_text
            col_titles_side <- "bottom"
        } else {
            abort(paste("Top/bottom col titles position not recognised:", col_titles_side))
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
        row_titles_side = row_titles_side,
        row_titles_bar_size = titles_bar_size,
        row_titles_bar_colour = titles_bar_colour,
        col_titles = col_titles,
        col_titles_side = col_titles_side,
        col_titles_bar_size = titles_bar_size,
        col_titles_bar_colour = titles_bar_colour,
        titles_colour = titles_colour,
        titles_font = titles_font,
        titles_size = titles_size,
        titles_style = titles_style,
        titles_weight = titles_weight,
        titles_position = titles_position,
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
    col_titles = c("E-left-p", "E-right"),
    col_titles_side = "top",
    row_titles = c("E-top", "E-bottom"),
    row_titles_side = "left",
    titles_bar_size = 10,
    titles_size = 75,
    titles_colour = "#000000",
    titles_position = 20,
    channels_list = list(
        c("H", "T", "M"),
        "H",
        "M",
        "T"
    ),
    panel_border = 20,
    figure_border_text = 100,
    folder = "imagej_export_max",
    background_colour = "#000000",
    border_colour = "#FFFFFF",
    labels_top = c("A", "B", "C", "E"),
    labels_colours_top = "#FFFFFF",
    labels_bottom = c("Merge", "Hoechst", "MAP2", "EXO1"),
    labels_colours_bottom = c("#FFFFFF", "#0000FF", "#FF0000", "#00FF00"),
    labels_position = 30
)
