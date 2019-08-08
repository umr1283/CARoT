library(ggplot2)
library(hexSticker)
library(magick)
library(showtext)

font_add_google("Lemon", "cf")
showtext_auto()

carot_colour <- "FFB85D"

paste0("https://svgsilh.com/svg/144965-", carot_colour, ".svg") %>%
  image_read_svg() %>%
  image_background(color = "transparent") %>%
  image_rotate(degrees = -24) %>%
  sticker(
    subplot = .,
    s_x = 1.21,
    s_y = 0.88,
    s_width = 1.580,
    s_height = 1.806,
    package = "CARoT",
    p_color = paste0("#", carot_colour),
    p_size = 7 * 2.5,
    p_x = 0.6,
    p_y = 1.3,
    p_family = "cf",
    spotlight = FALSE,
    h_fill = "#F87217",
    h_color = "#FFB85D",
    dpi = 120 * 2.5,
    filename = "./man/figures/carot_hex.png"
  )

# paste0("https://svgsilh.com/svg/432492-", carot_colour, ".svg") %>%
#   image_read_svg() %>%
#   image_background(color = "transparent") %>%
#   image_rotate(degrees = 35) %>%
#   sticker(
#     subplot = .,
#     s_x = 1,
#     s_y = 1.05,
#     s_width = 1.772,
#     s_height = 1.576,
#     package = "CARoT",
#     p_color = paste0("#", carot_colour),
#     p_size = 24 / 2.5,
#     p_y = 0.55,
#     p_family = "cf",
#     spotlight = FALSE,
#     h_fill = "#F87217",
#     h_color = "#FFB85D",
#     dpi = 120,
#     filename = "./man/figures/carot_hex.png"
#   )

# image <- paste0("https://svgsilh.com/svg/1299147-", carot_colour, ".svg")

