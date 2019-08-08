library(ggplot2)
library(hexSticker)
library(magick)
library(showtext)

font_add_google("Lemon", "cf")
showtext_auto()
carot_colour <- "FFB85D"
image_read_svg(path = paste0("https://svgsilh.com/svg/432492-", carot_colour, ".svg")) %>%
  image_background(color = "transparent") %>%
  image_rotate(degrees = 35) %>%
  sticker(
    subplot = .,
    s_x = 1,
    s_y = 1.05,
    s_width = 1.772,
    s_height = 1.576,
    package = "CARoT",
    p_color = paste0("#", carot_colour),
    p_size = 24 / 2.5,
    p_y = 0.55,
    p_family = "cf",
    spotlight = FALSE,
    h_fill = "#F87217",
    h_color = "#FFB85D",
    dpi = 120,
    filename = "./man/figures/carot_hex.png"
  )

