setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(magick)
library(hexSticker)
library(panelcomposite)

images <- paste0("extdata/images/FAN1_zoom_", c("hoechst", "smi32", "target"), ".tiff")
paths <- system.file(images, package = "panelcomposite")
images <- read_many_images(paths)

colours <- c("#0000FF", "#FF0084", "#00FF00")
brightnesses <- c(150, 100, 100)
tinted <- list()
for (i in seq_along(colours)) {
    tinted[[i]] <- multiply_by_colour(images[[i]], colours[i], brightnesses[i])
}
merged <- merge_channels(tinted)
merged

image_info(merged) ## 4:3 ratio

s <- sticker(
    subplot = merged,
    package = "panelcomposite",
    p_size = 17,
    p_y = 1.3,
    s_width = 2.2,
    s_height = 2.2,
    s_x = 1,
    s_y = 1,
    white_around_sticker = FALSE,
    h_color = "#FF0084",
    filename = "sticker_exported_full.png"
)
plot(s)

s <- sticker(
    subplot = image_blank(1, 1, "#0000FF"),
    package = "panelcomposite",
    white_around_sticker = FALSE,
    h_color = "#FF0084",
    h_fill = "#0000FF",
    filename = "sticker_exported_clean.png"
)
plot(s)
