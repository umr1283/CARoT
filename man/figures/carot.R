# devtools::install_github("GuangchuangYu/hexSticker")
# install.packages("rsvg")
# install.packages("pdftools")


library(ggplot2)
library(hexSticker)


sticker <- function(subplot, s_x = 0.8, s_y = 0.75, s_width = 0.4, s_height = 0.5,
                    package, p_x = 1, p_y = 1.4, p_color = "#FFFFFF", p_family = "Aller_Rg",
                    p_size = 8, h_size = 1.2, h_fill = "#1881C2", h_color = "#87B13F",
                    spotlight = FALSE, l_x = 1, l_y = 0.5, l_width = 3, l_height = 3,
                    l_alpha = 0.4, url = "", u_x = 1, u_y = 0.08, u_color = "black",
                    u_family = "Aller_Rg", u_size = 1.5, white_around_sticker = FALSE,
                    ..., filename = paste0(package, ".png"), asp = 1, dpi = 300) {
  sticker <- ggplot() +
    geom_hexagon(size = h_size, fill = h_fill, color = NA)

  sticker <- sticker +
    geom_hexagon(size = h_size, fill = NA, color = h_color)

  if (spotlight) {
    sticker <- sticker +
      ggimage::geom_subview(
        subview = hexSticker:::spotlight(l_alpha), x = l_x, y = l_y, width = l_width, height = l_height
      )
  }

  if (inherits(subplot, "character")) {
    d <- data.frame(x = s_x, y = s_y, image = subplot)
    sticker <- sticker +
      ggimage::geom_image(
        aes_(x = ~x, y = ~y, image = ~image), d, size = s_width, asp = asp
      )
  } else {
    sticker <- sticker +
      ggimage::geom_subview(
        subview = subplot, x = s_x, y = s_y, width = s_width, height = s_height
      )
  }

  sticker <- sticker +
    geom_pkgname(package, p_x, p_y, p_color, p_family, p_size, ...)

  sticker <- sticker +
    geom_url(url, x = u_x, y = u_y, color = u_color, family = u_family, size = u_size)

  if (white_around_sticker) {
    sticker <- sticker +
      white_around_hex(size = h_size)
  }

  sticker <- sticker +
    theme_sticker(size = h_size)
  save_sticker(filename, sticker, dpi = dpi)
  invisible(sticker)
}


sticker(
  subplot = "./man/figures/carot.png",
  s_x = 1,
  s_y = 1.19,
  s_width = 0.65,
  package = "CARoT",
  p_color = "white",
  p_size = 24,
  p_y = 0.46,
  spotlight = TRUE,
  l_alpha = 0.75,
  l_x = 1,
  l_y = 1,
  l_width = 5,
  l_height = 5,
  h_fill = "springgreen3",
  h_color = "springgreen4",
  dpi = 300,
  filename = "./man/figures/carot_hex.png"
)
