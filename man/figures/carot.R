library(ggplot2)
library(hexSticker)
library(magick)
library(showtext)

font_add_google("Lemon", "cf")
showtext_auto()

# paste0("https://svgsilh.com/svg/144965-FFB85D.svg") %>%
#   image_read_svg() %>%
#   image_background(color = "transparent") %>%
#   image_rotate(degrees = -24) %>%
#   sticker(
#     subplot = .,
#     s_x = 1.21,
#     s_y = 0.88,
#     s_width = 1.580,
#     s_height = 1.806,
#     package = "CARoT",
#     p_color = "#FFB85D",
#     p_size = 7 * 2.5,
#     p_x = 0.6,
#     p_y = 1.3,
#     p_family = "cf",
#     spotlight = FALSE,
#     h_fill = "#F87217",
#     h_color = "#FFB85D",
#     dpi = 120 * 2.5,
#     filename = "./man/figures/carot_hex.png"
#   )

## move SVG position and different colours set
for (icolour in 1:4) {
  carot_colour <- c("FFB85D", "F87217", "00ee76", "008b45")[icolour]
  paste0("https://svgsilh.com/svg/144965-", carot_colour, ".svg") %>%
    image_read_svg() %>%
    image_background(color = "transparent") %>%
    image_rotate(degrees = -85) %>%
    sticker(
      subplot = .,
      s_x = 1.21,
      s_y = 1.135,
      s_width = 1.580 * 1.2,
      s_height = 1.806 * 1.2,
      package = "CARoT",
      p_color = paste0("#", carot_colour),
      p_size = 8 * 2.5,
      p_x = 0.75,
      p_y = 0.60,
      p_family = "cf",
      spotlight = FALSE,
      h_fill = c("#F87217", "#FFB85D", "#008b45", "#00ee76")[icolour],
      h_color = c("#FFB85D", "#F87217", "#00ee76", "#008b45")[icolour],
      dpi = 120 * 2.5,
      filename = paste0("./man/figures/carot_hex", icolour, "a.png")
    )
  # image_read(paste0("./man/figures/carot_hex", icolour, "a.png"))
}

## Old SVg picture and different colours set
for (icolour in 1:4) {
  carot_colour <- c("FFB85D", "F87217", "00ee76", "008b45")[icolour]
  paste0("https://svgsilh.com/svg/432492-", carot_colour, ".svg") %>%
    image_read_svg() %>%
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
      p_size = 10 * 2.5,
      p_y = 0.55,
      p_family = "cf",
      spotlight = FALSE,
        h_fill = c("#F87217", "#FFB85D", "#008b45", "#00ee76")[icolour],
        h_color = c("#FFB85D", "#F87217", "#00ee76", "#008b45")[icolour],
      dpi = 120 * 2.5,
      filename = paste0("./man/figures/carot_hex", icolour, "b.png")
    )
  # image_read(paste0("./man/figures/carot_hex", icolour, "b.png"))
}


for (icolour in 1:4) {
  carot_colour <- c("FFB85D", "F87217", "00ee76", "008b45")[icolour]
  paste0("https://svgsilh.com/svg/1299147-", carot_colour, ".svg") %>%
    image_read_svg() %>%
    image_background(color = "transparent") %>%
    image_rotate(degrees = 0) %>%
    sticker(
      subplot = .,
      s_x = 0.97,
      s_y = 0.93,
      s_width = 1.235 * 1.1,
      s_height = 1.600 * 1.1,
      package = "CARoT",
      p_color = paste0("#", carot_colour),
      p_size = 6 * 2.5,
      p_x = 1.47,
      p_y = 0.8,
      p_family = "cf",
      spotlight = FALSE,
        h_fill = c("#F87217", "#FFB85D", "#008b45", "#00ee76")[icolour],
        h_color = c("#FFB85D", "#F87217", "#00ee76", "#008b45")[icolour],
      dpi = 120 * 2.5,
      filename = paste0("./man/figures/carot_hex", icolour, "c.png")
    )
  # image_read(paste0("./man/figures/carot_hex", icolour, "c.png"))
}

file.copy(paste0("./man/figures/carot_hex2b.png"), paste0("./man/figures/carot_hex.png"), overwrite = TRUE)
